package joptimize.optimize

import joptimize.analyzer.Namer
import joptimize.graph.TarjansStronglyConnectedComponents
import joptimize.model._
import joptimize.optimize.OptimisticAnalyze.{Result, Step, WorkItem, topoSort}
import joptimize.{FileLogger, Logger, Util}

import scala.collection.mutable

class OptimisticAnalyze[T](methodBody: MethodBody,
                           initialValues: Map[SSA.Val, T],
                           initialBlock: SSA.Block,
                           lattice: Lattice[T],
                           log: Logger.InferredMethod,
                           evaluateUnaBranch: (T, SSA.UnaBranch.Code) => Option[Boolean],
                           evaluateBinBranch: (T, T, SSA.BinBranch.Code) => Option[Boolean],
                           evaluateSwitch: T => Option[Int],
                           methodProps: InferredSig => T){

  val inferredBlocks = mutable.Set(initialBlock)

  // blocks that have not yet been processed
  // false -> start processing this block
  // true -> block has been fully processed, start processing next control (block or jump)
  val blockWorkList = mutable.LinkedHashSet[WorkItem](WorkItem.Block(initialBlock))

  val valWorkList = mutable.LinkedHashSet.empty[SSA.Val]
  val evaluated = mutable.LinkedHashMap.empty[SSA.Val, T]

  val inferredReturns = mutable.Buffer.empty[T]

  def step(): OptimisticAnalyze.Step[T] = {
    if (valWorkList.nonEmpty) evaluateVal()
    else if (blockWorkList.nonEmpty){
      val item = blockWorkList.head
      blockWorkList.remove(item)
      item match{
        case WorkItem.Block(currentBlock) =>
          inferredBlocks.add(currentBlock)
          log.pprint(currentBlock)
          topoSort(currentBlock.next.upstreamVals.toSet.filter(!_.isInstanceOf[SSA.Phi])).foreach(valWorkList.add)
          blockWorkList.add(WorkItem.BlockJump(currentBlock))
          Step.Continue(None)

        case WorkItem.BlockJump(currentBlock) =>
          val nextBlocks = computeNextBlocks(currentBlock)
          for(nextBlock <- nextBlocks){
            blockWorkList.add(WorkItem.Transition(currentBlock, nextBlock))
          }
          Step.Continue(None)
        case WorkItem.Transition(currentBlock, nextBlock) =>
          queueNextBlockTwo(currentBlock, nextBlock)
          Step.Continue(None)
      }

    }else {
      Step.Done()
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
            inferredReturns.append(evaluated(r.state))
            Nil
          case r: SSA.ReturnVal =>
            inferredReturns.append(evaluated(r.src))
            inferredReturns.append(evaluated(r.state))
            Nil
          case n: SSA.UnaBranch =>
            val valueA = evaluated(n.a)
            val doBranch = evaluateUnaBranch(valueA, n.opcode)

            queueBranchBlock(currentBlock, n, doBranch)
          case n: SSA.BinBranch =>
            val valueA = evaluated(n.a)
            val valueB = evaluated(n.b)
            val doBranch = evaluateBinBranch(valueA, valueB, n.opcode)
            queueBranchBlock(currentBlock, n, doBranch)

          case n: SSA.Switch =>
            val value = evaluated(n.src)
            val cases = n.cases.values
            val default = n.default
            val doBranch = evaluateSwitch(value)

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

    var continueNextBlock = !inferredBlocks(nextBlock)

    val invalidatedPhis = mutable.Set.empty[SSA.Phi]

    for ((k, v) <- newPhiValues) {
      evaluated.get(k) match {
        case None =>
          continueNextBlock = true
          evaluated(k) = v
        case Some(old) =>
          if (old != v) {

            val merged = lattice.join(old, v)
            if (merged != old) {
              continueNextBlock = true
              //                  log.pprint((k, old, v, merged))
              invalidatedPhis.add(k)
              evaluated(k) = merged
            }
          }
      }
    }

    //        log.pprint(invalidatedPhis)
    //        log.pprint(continueNextBlock)
    if (continueNextBlock) {

      blockWorkList.add(WorkItem.Block(nextBlock))
      val invalidated = Util.breadthFirstSeen[SSA.Node](invalidatedPhis.toSet)(_.downstreamList.filter(!_.isInstanceOf[SSA.Phi]))
        .filter(!_.isInstanceOf[SSA.Phi])

      //          log.pprint(invalidated)
      invalidated.foreach {
        case removed: SSA.Block => inferredBlocks.remove(removed)
        case removed: SSA.Jump => inferredBlocks.remove(removed.block)
        case removed: SSA.Val => evaluated.remove(removed)
        case _ => // do nothing
      }
    }
  }


  def queueNextBlock(currentBlock: SSA.Block, nextBlock: SSA.Block) = {
    val newPhiExpressions = getNewPhiExpressions(currentBlock, nextBlock)

    topoSort(newPhiExpressions.map(_._2).toSet.filter(!_.isInstanceOf[SSA.Phi])).foreach{  v =>
      valWorkList.add(v)
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

  def evaluateVal(): Step[T] = {
    valWorkList.headOption match{
      case Some(v) =>
        valWorkList.remove(v)
        val upstream = v.upstream.collect{case v: SSA.Val => evaluated(v)}
        if (upstream.contains(IType.Bottom)) {
          evaluated(v) = IType.Bottom.asInstanceOf[T]
          Step.Continue(Some(v))
        } else v match{
          case n: SSA.Invoke =>
            val isig = n.inferredSig(evaluated(_).asInstanceOf[IType])
            Step.ComputeSig[T](isig, n)
          case _ =>
            evaluated(v) = lattice.transferValue(v, evaluated)
            Step.Continue(Some(v))
        }
      case None => Step.Continue(None)
    }
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
      inferredBlocks.toSet
    )
  }
}
object OptimisticAnalyze {

  sealed trait WorkItem
  object WorkItem{
    case class Block(value: SSA.Block) extends WorkItem
    case class BlockJump(value: SSA.Block) extends WorkItem
    case class Transition(src: SSA.Block, dest: SSA.Block) extends WorkItem
  }

  sealed trait Step[T]
  object Step{
    case class Continue[T](node: Option[SSA.Val]) extends Step[T]
    case class Done[T]() extends Step[T]
    case class ComputeSig[T](isig: InferredSig, invoke: SSA.Invoke) extends Step[T]
  }
  case class Result[T](inferredReturns: Seq[T],
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
