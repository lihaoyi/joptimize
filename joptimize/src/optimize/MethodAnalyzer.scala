package joptimize.optimize

import joptimize.analyzer.Namer
import joptimize.graph.TarjansStronglyConnectedComponents
import joptimize.model._
import joptimize.optimize.MethodAnalyzer.{Evaluate, Invalidate, Result, Step, topoSort}
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
  * Inference of [[SSA.Invoke]] method call nodes is delegated to the caller, who has
  * to take the [[Step.Continue]] returned by [[step]], schedule that node to have its
  * return type inferred, and insert the value into the current `evaluated` dictionary
  * before continuing to step through this [[MethodAnalyzer]]
  */
class MethodAnalyzer[T](methodBody: MethodBody,
                        initialValues: Map[SSA.Val, T],
                        initialBlock: SSA.Block,
                        lattice: Lattice[T],
                        log: Logger.InferredMethod,
                        brancher: Brancher[T]){

  /**
    * A queue of tasks to perform when traversing new previously-unseen code.
    */
  val evaluateWorkList = mutable.LinkedHashSet[Evaluate](Evaluate.Block(initialBlock))
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
  val inferredReturns = mutable.LinkedHashMap.empty[SSA.Jump, Seq[T]]

  def step(): MethodAnalyzer.Step[T] = {
//    pprint.log(workList)
    if (invalidateWorkList.nonEmpty){
      val item = invalidateWorkList.head
      invalidateWorkList.remove(item)
      item match {
        case Invalidate.Phi(v) => queueDownstreamInvalidations(v)
        case Invalidate.Invoke(v) => queueDownstreamInvalidations(v)
        case Invalidate.Incremental(v) => invalidateValue(v)
      }
    } else if (evaluateWorkList.nonEmpty){
      val item = evaluateWorkList.head
      evaluateWorkList.remove(item)
      item match{
        case Evaluate.Val(v) =>
          if (!evaluated.contains(v)) evaluateVal(v)
          else Step.Continue(Nil)

        case Evaluate.Block(currentBlock) =>

          log.pprint(currentBlock)
          queueSortedUpstreams(currentBlock.next.upstreamVals.toSet)
          evaluateWorkList.add(Evaluate.BlockJump(currentBlock))
          Step.Continue(Nil)

        case Evaluate.BlockJump(currentBlock) =>
          val nextBlocks = computeNextBlocks(currentBlock)
          for(nextBlock <- nextBlocks){
            evaluateWorkList.add(Evaluate.Transition(currentBlock, nextBlock))
          }
          Step.Continue(Nil)

        case Evaluate.Transition(currentBlock, nextBlock) =>
          queueNextBlockTwo(currentBlock, nextBlock)
          Step.Continue(Nil)
      }

    }else {
      Step.Done()
    }
  }

  def queueDownstreamInvalidations(v: SSA.Val): Step.Continue[T] = {
    val downstreams = v.downstreamList.filter{
      case v: SSA.Val => evaluated.contains(v)
      case j: SSA.Jump => liveBlocks.contains(j.block)
    }
    downstreams.foreach {
      case phi: SSA.Phi =>
        if (evaluated(v) != evaluated(phi)){
          evaluated(phi) = lattice.join(evaluated(v), evaluated(phi))
          invalidateWorkList.add(Invalidate.Phi(phi))
        }
      case i: SSA.Invoke => invalidateWorkList.add(Invalidate.Invoke(i))
      case nextV: SSA.Val => invalidateWorkList.add(Invalidate.Incremental(nextV))
      case j: SSA.Jump => evaluateWorkList.add(Evaluate.BlockJump(j.block))
    }
    Step.Continue(downstreams.collect{case v: SSA.Val => v})
  }

  def invalidateValue(v: SSA.Val): MethodAnalyzer.Step[T] = {
    val upstream = v.upstream.collect{case v: SSA.Val => evaluated.getOrElse(v, IType.Bottom)}

    if (upstream.contains(IType.Bottom)) Step.Continue(Seq(v)) // do nothing
    else {
      val newValue = lattice.transferValue(v, evaluated)
      if (evaluated(v) == newValue) Step.Continue(Seq(v))
      else{
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
          case r: SSA.Return =>
            inferredReturns(r) = Seq(evaluated(r.state))
            Nil
          case r: SSA.ReturnVal =>
            inferredReturns(r) = Seq(evaluated(r.src), evaluated(r.state))
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
    }else{
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


  def queueSortedUpstreams(set: Set[SSA.Val]) = {
    topoSort(set.filter(!_.isInstanceOf[SSA.Phi])).foreach { v =>
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
    if (!evaluated.contains(v)){
      val upstream = v.upstream.collect{case v: SSA.Val => evaluated(v)}
      if (upstream.contains(IType.Bottom)) evaluated(v) = IType.Bottom.asInstanceOf[T]
      else v match{
        case n: SSA.Invoke =>
        case _ => evaluated(v) = lattice.transferValue(v, evaluated)
      }
    }
    Step.Continue(Seq(v))
  }

  /**
    * Performs an optimistic analysis on the given method body.
    *
    * Walks the method body block-by-block, evaluating all phi nodes each time
    * a block transition is made. The current evaluation environment is kept as
    * a global mapping of phi nodes to [[T]], which is updated as evaluation
    * occurs. Evaluation of non-phi nodes are cached, unless invalidated by a
    * change in the value assigned to an upstream phi node.
    *
    * The phi-inference mapping can be shared by all blocks being evaluated, as
    * the inferences monotonically widen as time goes on, and we never have two
    * blocks which treat a single phi node as having multiple, incompatible
    * inferences. If a block is re-visited with a set of phi-inferences
    * different from what it was assigned earlier, the conflicting inferences
    * are merged via [[Lattice.join]] and the block re-visited using the new
    * inferences.
    *
    * As long as new blocks are being discovered, or phi node inferences are
    * modified, the respective blocks are added to the worklist for processing.
    * When the worklist is empty, inference is complete and the algorithm exits
    */
  def apply(): Result[T] = {
    Result(
      inferredReturns,
      evaluated,
      liveBlocks.toSet
    )
  }
}
object MethodAnalyzer {
  sealed trait Invalidate
  object Invalidate{
    case class Phi(src: SSA.Phi) extends Invalidate
    case class Invoke(src: SSA.Invoke) extends Invalidate
    case class Incremental(src: SSA.Val) extends Invalidate{
      assert(!src.isInstanceOf[SSA.Invoke] && !src.isInstanceOf[SSA.Phi])
    }
  }
  sealed trait Evaluate
  object Evaluate{
    case class Val(value: SSA.Val) extends Evaluate
    case class Block(value: SSA.Block) extends Evaluate
    case class BlockJump(value: SSA.Block) extends Evaluate
    case class Transition(src: SSA.Block, dest: SSA.Block) extends Evaluate

  }

  sealed trait Step[T]
  object Step{
    case class Continue[T](node: Seq[SSA.Val]) extends Step[T]
    case class Done[T]() extends Step[T]
  }
  case class Result[T](inferredReturns: mutable.LinkedHashMap[SSA.Jump, Seq[T]],
                       inferred: mutable.LinkedHashMap[SSA.Val, T],
                       liveBlocks: Set[SSA.Block])


  def topoSort[T](set: Set[SSA.Val]) = {
    val agg = Util.breadthFirstSeen(set)(
      v => v.upstreamVals.filter(!_.isInstanceOf[SSA.Phi])
    )

    val aggArray = agg.toArray
    val aggIndices = aggArray.zipWithIndex.toMap
    val edges = aggArray.map(_.upstreamVals.collect(aggIndices))

    val topoSorted = TarjansStronglyConnectedComponents(edges)
    val res = topoSorted.map { case Seq(x) => aggArray(x) }
    res
  }
}
