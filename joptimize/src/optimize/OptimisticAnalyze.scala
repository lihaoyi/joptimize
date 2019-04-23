package joptimize.optimize

import joptimize.analyzer.Namer
import joptimize.graph.TarjansStronglyConnectedComponents
import joptimize.model._
import joptimize.optimize.OptimisticAnalyze.{Result, State, Step, topoSort}
import joptimize.{FileLogger, Logger, Util}

import scala.collection.mutable

class OptimisticAnalyze[T](methodBody: MethodBody,
                           initialValues: Map[SSA.Val, T],
                           initialBlock: SSA.Block,
                           lattice: Lattice[T],
                           log: Logger.InferredMethod,
                           evaluateUnaBranch: (T, SSA.UnaBranch.Code) => Option[Boolean],
                           evaluateBinBranch: (T, T, SSA.BinBranch.Code) => Option[Boolean],
                           evaluateSwitch: T => Option[Int]){

  val inferredBlocks = mutable.Set(initialBlock)

  val blockWorkList = mutable.LinkedHashSet(initialBlock)

  val valWorkList = mutable.LinkedHashSet.empty[SSA.Val]
  val evaluated = mutable.LinkedHashMap.empty[SSA.Val, T]

  val inferredReturns = mutable.Buffer.empty[T]
  var state: State = State.HandleBlock()
  var currentBlock: SSA.Block = null
  var nextControl: SSA.Control = null
  var nextBlocks: Seq[SSA.Block] = null

  def getNewPhiExpressions(nextBlock: SSA.Block) = {
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


  def queueNextBlockTwo(nextBlock: SSA.Block) = {
    val newPhiValues = getNewPhiExpressions(nextBlock)
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

      blockWorkList.add(nextBlock)
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


  def queueNextBlock(nextBlock: SSA.Block) = {
    val newPhiExpressions = getNewPhiExpressions(nextBlock)

    topoSort(newPhiExpressions.map(_._2).toSet.filter(!_.isInstanceOf[SSA.Phi])).foreach{  v =>
      valWorkList.add(v)
    }
  }


  def queueBranchBlock(n: SSA.Branch, doBranch: Option[Boolean]) = {
    doBranch match {
      case None =>
        queueNextBlock(n.trueBranch)
        queueNextBlock(n.falseBranch)
        nextBlocks = Seq(n.trueBranch, n.falseBranch)
      case Some(bool) =>
        if (bool) {
          queueNextBlock(n.trueBranch)
          nextBlocks = Seq(n.trueBranch)
        } else {
          queueNextBlock(n.falseBranch)
          nextBlocks = Seq(n.falseBranch)
        }
    }
  }

  def evaluateVals(onFinish: => Unit): Step[T] = {
    valWorkList.headOption match{
      case Some(v) =>
        valWorkList.remove(v)
        v match{
          case n: SSA.Invoke =>
            Step.ComputeSig[T](n.sig, n, n.srcs.map(evaluated), res => {
              evaluated(v) = res
            })
          case _ =>
            evaluated(v) = lattice.transferValue(v, evaluated)
            Step.Continue(Some(v))
        }
      case None =>
        onFinish
        Step.Continue(None)
    }
  }

  def step(): OptimisticAnalyze.Step[T] = state match{
    case State.HandleBlock() =>
      blockWorkList.headOption match{
        case None => Step.Done()
        case Some(currentBlock0) =>
          currentBlock = currentBlock0
          inferredBlocks.add(currentBlock)
          blockWorkList.remove(currentBlock)
          log.pprint(currentBlock)
          val Seq(nextControl0) = currentBlock.downstreamList.collect{case n: SSA.Control => n}
          nextControl = nextControl0
          topoSort(nextControl.upstreamVals.toSet.filter(!_.isInstanceOf[SSA.Phi])).foreach(valWorkList.add)
          state = State.HandleVal()
          Step.Continue(None)
      }
    case State.HandleVal() =>
      evaluateVals{
        state = State.HandleControl()
      }

    case State.HandleControl() =>

      nextControl match{
        case nextBlock: SSA.Block =>
          queueNextBlock(nextBlock)
          nextBlocks = Seq(nextBlock)

        case n: SSA.Jump =>
          n match{
            case r: SSA.Return =>
              inferredReturns.append(evaluated(r.state))
              nextBlocks = Nil
            case r: SSA.ReturnVal =>
              inferredReturns.append(evaluated(r.src))
              inferredReturns.append(evaluated(r.state))
              nextBlocks = Nil
            case n: SSA.UnaBranch =>
              val valueA = evaluated(n.a)
              val doBranch = evaluateUnaBranch(valueA, n.opcode)

              queueBranchBlock(n, doBranch)
            case n: SSA.BinBranch =>
              val valueA = evaluated(n.a)
              val valueB = evaluated(n.b)
              val doBranch = evaluateBinBranch(valueA, valueB, n.opcode)
              queueBranchBlock(n, doBranch)

            case n: SSA.Switch =>
              val value = evaluated(n.src)
              val cases = n.cases.values
              val default = n.default
              val doBranch = evaluateSwitch(value)

              doBranch match{
                case None =>
                  for(dest <- cases) queueNextBlock(dest)
                  queueNextBlock(default)

                  nextBlocks = cases.toSeq :+ default
                case Some(destValue) =>
                  val dest = cases.find(_.n == destValue).getOrElse(default)
                  queueNextBlock(dest)
                  nextBlocks = Seq(dest)
              }
          }
      }

      state = State.HandleControlVals()
      Step.Continue(None)

    case State.HandleControlVals() =>
      evaluateVals{
        for(nextBlock <- nextBlocks){
          queueNextBlockTwo(nextBlock)
        }
        state = State.HandleBlock()
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
  sealed trait Step[T]
  object Step{
    case class Continue[T](node: Option[SSA.Val]) extends Step[T]
    case class Done[T]() extends Step[T]
    case class ComputeSig[T](sig: MethodSig, invoke: SSA.Invoke, inferred: Seq[T], callback: T => Unit) extends Step[T]
  }
  sealed trait State
  object State{
    case class HandleBlock() extends State
    case class HandleVal() extends State
    case class HandleControl() extends State
    case class HandleControlVals() extends State
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
