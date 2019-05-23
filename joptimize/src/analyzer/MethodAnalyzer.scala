package joptimize.analyzer

import joptimize.graph.TarjansStronglyConnectedComponents
import joptimize.model._
import MethodAnalyzer.{BlockJump, Evaluate, Invalidate, Result, Step, topoSort}
import joptimize.{FileLogger, Logger, Util}

import scala.collection.mutable

/**
  * Optimistically incrementally walks a method to infer a property [[T]] over the
  * [[SSA.Val]]s inside the method body.
  *
  * Execution is controlled by two inputs: the [[lattice]] that infers values for each
  * [[SSA.Val]] based on its upstream inputs, and a [[brancher]] that determines which
  * blocks to branch to depending on the input of each [[SSA.Jump]].
  *
  * The traversal state is entirely encapsulated in the current set of inferences
  * (`evaluated`, `liveBlocks`, `inferredReturns`) as well as the two work lists of
  * `evaluateWorkList` and `invalidateWorkList` containing nodes to evaluate or
  * invalidate.
  *
  * Inference of previously-unseen nodes takes place block-by-block exclusively through
  * the `evaluateWorkList`, while inference of already-seen nodes takes place node-by-node
  * through `invalidateWorkList`. The coarse block-by-block traversal is necessary for
  * reachability and control-flow analysis to occur, while the node-by-node traversal
  * allows for more finer-grained invalidation of already-seen nodes and simply delegates
  * to the block-by-block analysis for nodes it hasn't already seen.
  *
  * In the presence of invalidation, both `evaluateWorkList` and `invalidateWorkList` may
  * end up with nodes which are in the wrong seen state for that work list to process; in
  * such a case those nodes are simply ignored.
  *
  * Inference of [[SSA.Invoke]] method call nodes is delegated to the caller, who has
  * to take the [[Step.Continue]] returned by [[step]], schedule that node to have its
  * return type inferred, and insert the value into the current `evaluated` dictionary
  * before continuing to step through this [[MethodAnalyzer]]
  */
class MethodAnalyzer[T](
  methodBody: MethodBody,
  initialValues: Map[SSA.Val, T],
  initialBlock: SSA.Block,
  lattice: Lattice[T],
  log: Logger.InferredMethod,
  brancher: Brancher[T],
  bottom: T,
  void: T
) {

  log.graph("PRE OPTIMISTIC ANALYSIS")(Renderer.dumpSvg(methodBody))

  /**
    * A queue of tasks to perform when traversing new previously-unseen code.
    */
  val evaluateWorkList = mutable.LinkedHashSet[Evaluate](Evaluate.Block(initialBlock))
  val jumpWorkList = mutable.LinkedHashSet[BlockJump]()

  /**
    * A queue of tasks to perform when traversing invalidated already-seen code. Takes
    * priority over [[evaluateWorkList]] during traversal, as we would like to complete
    * invalidations as early as possible to minimize the amount of code that gets
    * invalidated
    */
  val invalidateWorkList = mutable.LinkedHashSet[Invalidate]()

  /**
    * The primary set of things being inferred over the values of the program.
    */
  val evaluated = mutable.LinkedHashMap.empty[SSA.Val, T]

  /**
    * The set of live blocks that have been visited in the course of this method body's
    * traversal. Grows monotonically
    */
  val liveBlocks = mutable.LinkedHashSet(initialBlock)

  /**
    * The set of return instructions that have been seen in the course of this method body's
    * traversal, and their returned values. Grows monotonically
    */
  val inferredReturns = mutable.LinkedHashMap.empty[SSA.Jump, T]
  val inferredThrows = mutable.LinkedHashMap.empty[SSA.AThrow, T]

  def step(): MethodAnalyzer.Step[T] = {
    if (invalidateWorkList.nonEmpty) {
      val item = invalidateWorkList.head
      invalidateWorkList.remove(item)
      item match {
        case Invalidate.Phi(v) => queueDownstreamInvalidations(v)
        case Invalidate.Invoke(v) => queueDownstreamInvalidations(v)
        case Invalidate.Incremental(v) => invalidateValue(v)
      }
    } else if (evaluateWorkList.nonEmpty) {
      val item = evaluateWorkList.head
      log.pprint(item)
      evaluateWorkList.remove(item)
      item match {
        case Evaluate.Val(v) =>
          if (!evaluated.contains(v)) evaluateVal(v)
          else Step.Continue(Nil)

        case Evaluate.Block(currentBlock) =>
          log.pprint(currentBlock)
          val set = currentBlock.next match {
            case next: SSA.Merge =>
              next.incoming.find(_._1 == currentBlock).map(_._2: SSA.ValOrState).toSet
            case next => next.upstream.collect { case v: SSA.ValOrState => v }.toSet
          }

          queueSortedUpstreams(set)
          jumpWorkList.add(BlockJump(currentBlock))
          Step.Continue(Nil)

        case Evaluate.Transition(currentBlock, nextBlock) =>
          queueNextBlockTwo(currentBlock, nextBlock)
          Step.Continue(Nil)
      }

    } else if (jumpWorkList.nonEmpty) {
      val item = jumpWorkList.head
      jumpWorkList.remove(item)
      item match {

        case BlockJump(currentBlock) =>
          val nextBlocks = computeNextBlocks(currentBlock)
          for (nextBlock <- nextBlocks) {
            evaluateWorkList.add(Evaluate.Transition(currentBlock, nextBlock))
          }
          Step.Continue(Nil)
      }
    } else {
      Step.Done()
    }
  }

  def queueDownstreamInvalidations(v: SSA.Val): Step.Continue[T] = {
    val downstreams = v.downstreamList.filter {
      case v: SSA.Val => evaluated.contains(v)
      case j: SSA.Jump => liveBlocks.contains(j.block)
      case s: SSA.State => false
    }
    downstreams.foreach {
      case phi: SSA.Phi =>
        if (evaluated(v) != evaluated(phi)) {
          evaluated(phi) = lattice.join(evaluated(v), evaluated(phi))
          invalidateWorkList.add(Invalidate.Phi(phi))
        }
      case i: SSA.Invoke => invalidateWorkList.add(Invalidate.Invoke(i))
      case i: SSA.InvokeDynamic => invalidateWorkList.add(Invalidate.InvokeDynamic(i))
      case nextV: SSA.Val => invalidateWorkList.add(Invalidate.Incremental(nextV))
      case j: SSA.Jump => jumpWorkList.add(BlockJump(j.block))
    }
    Step.Continue(downstreams.collect { case v: SSA.Val => v })
  }

  def invalidateValue(v: SSA.Val): MethodAnalyzer.Step[T] = {
    val upstream = v.upstream.collect { case v: SSA.Val => evaluated.getOrElse(v, JType.Bottom) }

    if (upstream.contains(JType.Bottom)) Step.Continue(Seq(v)) // do nothing
    else {
      val newValue = lattice.transferValue(v, evaluated)
      if (evaluated(v) == newValue) Step.Continue(Seq(v))
      else {
        evaluated(v) = newValue
        queueDownstreamInvalidations(v)
      }
    }
  }

  def computeNextBlocks(currentBlock: SSA.Block) = {
    currentBlock.next match {
      case nextBlock: SSA.Block =>
        queueNextBlock(currentBlock, nextBlock)
        Seq(nextBlock)

      case n: SSA.Jump =>
        n match {
          case r: SSA.AThrow =>
            inferredThrows(r) = bottom
            Nil
          case r: SSA.Return =>
            inferredReturns(r) = void
            Nil
          case r: SSA.ReturnVal =>
            if (evaluated(r.src) != bottom) inferredReturns(r) = evaluated(r.src)
            Nil
          case n: SSA.UnaBranch =>
            val valueA = evaluated(n.a)
            val doBranch = brancher.evaluateUnaBranch(valueA, n.opcode)

            queueBranchBlock(currentBlock, n, doBranch)
          case n: SSA.BinBranch =>
            val valueA = evaluated(n.a)
            val valueB = evaluated(n.b)
            val doBranch = brancher.evaluateBinBranch(valueA, valueB, n.opcode)
            queueBranchBlock(currentBlock, n, doBranch)

          case n: SSA.Switch =>
            val value = evaluated(n.src)
            val cases = n.cases.values
            val default = n.default
            val doBranch = brancher.evaluateSwitch(value)

            doBranch match {
              case None =>
                for (dest <- cases) queueNextBlock(currentBlock, dest)
                queueNextBlock(currentBlock, default)

                cases.toSeq :+ default
              case Some(destValue) =>
                val dest = cases.find(_.n == destValue).getOrElse(default)
                queueNextBlock(currentBlock, dest)
                Seq(dest)
            }
        }
    }
  }

  def getNewPhiExpressions(currentBlock: SSA.Block, nextBlock: SSA.Block) = {
    log.pprint(nextBlock)
    val nextPhis = nextBlock
      .downstreamList
      .collect { case p: SSA.Phi => p }
      .filter(phi => phi.block == nextBlock)

    val newPhiExpressions = nextPhis
      .map { phi =>
        val Seq(expr) = phi
          .incoming
          .collect { case (k, v) if k == currentBlock => v }
          .toSeq

        (phi, expr)
      }
    newPhiExpressions
  }

  def queueNextBlockTwo(currentBlock: SSA.Block, nextBlock: SSA.Block) = {
    val newPhiValues = getNewPhiExpressions(currentBlock, nextBlock)
      .map { case (phi, expr) => phi -> evaluated(expr) }

    val continueNextBlock = !liveBlocks(nextBlock)

    if (continueNextBlock) {
      for ((k, v) <- newPhiValues) {
        evaluated(k) = v
      }
      liveBlocks.add(nextBlock)
      evaluateWorkList.add(Evaluate.Block(nextBlock))
    } else {
      for ((k, v) <- newPhiValues) {
        val old = evaluated(k)
        if (old != v) {

          val merged = lattice.join(old, v)
          if (merged != old) {
            invalidateWorkList.add(Invalidate.Phi(k))
            evaluated(k) = merged
          }
        }
      }

    }
  }

  def queueNextBlock(currentBlock: SSA.Block, nextBlock: SSA.Block) = {
    val newPhiExpressions = getNewPhiExpressions(currentBlock, nextBlock)

    queueSortedUpstreams(newPhiExpressions.map(_._2).toSet)
  }

  def queueSortedUpstreams(set: Set[SSA.ValOrState]) = {
    val topoed = topoSort(set.filter(!_.isInstanceOf[SSA.Phi]))
    log.pprint(topoed)
    topoed.collect {
      case v: SSA.Val =>
        evaluateWorkList.add(Evaluate.Val(v))
    }
  }

  def queueBranchBlock(currentBlock: SSA.Block, n: SSA.Branch, doBranch: Option[Boolean]) = {
    doBranch match {
      case None =>
        queueNextBlock(currentBlock, n.trueBranch)
        queueNextBlock(currentBlock, n.falseBranch)
        Seq(n.trueBranch, n.falseBranch)
      case Some(bool) =>
        if (bool) {
          queueNextBlock(currentBlock, n.trueBranch)
          Seq(n.trueBranch)
        } else {
          queueNextBlock(currentBlock, n.falseBranch)
          Seq(n.falseBranch)
        }
    }
  }

  def evaluateVal(v: SSA.Val): Step[T] = {
    if (evaluated.contains(v)) Step.Continue(Nil)
    else {
      val upstream = v.upstream.collect { case v: SSA.Val => evaluated(v) }
      if (upstream.contains(bottom)) {
        evaluated(v) = bottom
        Step.Continue(Nil)
      } else {
        v match {
          case n: SSA.Invoke =>
          case n: SSA.InvokeDynamic =>
          case _ => evaluated(v) = lattice.transferValue(v, evaluated)
        }
        Step.Continue(Seq(v))
      }
    }
  }
}
object MethodAnalyzer {
  sealed trait Invalidate
  object Invalidate {
    case class Phi(src: SSA.Phi) extends Invalidate
    case class Invoke(src: SSA.Invoke) extends Invalidate
    case class InvokeDynamic(src: SSA.InvokeDynamic) extends Invalidate
    case class Incremental(src: SSA.Val) extends Invalidate {
      assert(!src.isInstanceOf[SSA.Invoke] && !src.isInstanceOf[SSA.Phi])
    }
  }
  sealed trait Evaluate
  object Evaluate {
    case class Val(value: SSA.Val) extends Evaluate
    case class Block(value: SSA.Block) extends Evaluate
    case class Transition(src: SSA.Block, dest: SSA.Block) extends Evaluate
  }

  case class BlockJump(value: SSA.Block)

  sealed trait Step[T]
  object Step {
    case class Continue[T](node: Seq[SSA.Val]) extends Step[T]
    case class Done[T]() extends Step[T]
  }
  case class Result[T](
    inferredTerminals: mutable.LinkedHashMap[SSA.Jump, T],
    inferred: mutable.LinkedHashMap[SSA.Val, T],
    liveBlocks: Set[SSA.Block]
  )

  def topoSort[T](set: Set[SSA.ValOrState]) = {
    val agg = Util.breadthFirstSeen[SSA.ValOrState](set)(
      _.upstream.collect { case s: SSA.ValOrState if !s.isInstanceOf[SSA.Phi] => s }
    )

    val aggArray = agg.toArray
    val aggIndices = aggArray.zipWithIndex.toMap
    val edges = aggArray.map(_.upstreamVals.collect(aggIndices))

    val topoSorted = TarjansStronglyConnectedComponents(edges)
    val res = topoSorted.map { case Seq(x) => aggArray(x) }
    res
  }
}
