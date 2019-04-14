package joptimize.optimize

import joptimize.analyzer.Namer
import joptimize.model._
import joptimize.{FileLogger, Logger, Util}

import scala.collection.mutable

object OptimisticAnalyze {
  case class Result[T](inferredReturns: Seq[T],
                       inferred: mutable.LinkedHashMap[SSA.Val, T],
                       liveBlocks: Set[SSA.Block])
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
  def apply[T](methodBody: MethodBody,
               initialValues: Map[SSA.Val, T],
               initialBlock: SSA.Block,
               lattice: Lattice[T],
               naming: Namer.Result,
               log: Logger.InferredMethod,
               evaluateUnaBranch: (T, SSA.UnaBranch.Code) => Option[Boolean],
               evaluateBinBranch: (T, T, SSA.BinBranch.Code) => Option[Boolean],
               evaluateSwitch: T => Option[Int]): Result[T] = {

    val inferredBlocks = mutable.Set(initialBlock)

    val workList = mutable.LinkedHashSet(initialBlock)

    val evaluated = mutable.LinkedHashMap.empty[SSA.Val, T]

    def evaluate(v: SSA.Val): T = {
      evaluated.getOrElseUpdate(
        v,
        v match {
          case phi: SSA.Phi => evaluated(phi)
          case _ => lattice.transferValue(v, evaluate)
        }
      )
    }

    val inferredReturns = mutable.Buffer.empty[T]
    while(workList.nonEmpty){

      val currentBlock = workList.head
      inferredBlocks.add(currentBlock)
      workList.remove(currentBlock)
//      log.pprint(currentBlock)
      val Seq(nextControl) = currentBlock.downstreamList.collect{case n: SSA.Control => n}
//      log.pprint(nextControl)
//      log.pprint(inferredBlocks)
//      log.pprint(workList)

      def queueNextBlock(nextBlock: SSA.Block) = {
//        log.pprint(nextBlock)
        val nextPhis = nextBlock
          .downstreamList
          .collect{case p: SSA.Phi => p}
          .filter(phi => phi.block == nextBlock)

        val newPhiMapping = nextPhis
          .map{phi =>
            val Seq(expr) = phi
              .incoming
              .collect{case (k, v) if k == currentBlock => v}
              .toSeq
            val res = evaluate(expr)
            (phi, res)
          }
          .toMap

        var continueNextBlock = !inferredBlocks(nextBlock)

        val invalidatedPhis = mutable.Set.empty[SSA.Phi]

        for((k, v) <- newPhiMapping) {
          evaluated.get(k) match{
            case None =>
              continueNextBlock = true
              evaluated(k) = v
            case Some(old) =>
              if (old != v){

                val merged = lattice.join(old, v)
                if (merged != old){
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

          workList.add(nextBlock)
          val invalidated = Util.breadthFirstSeen[SSA.Node](invalidatedPhis.toSet)(_.downstreamList.filter(!_.isInstanceOf[SSA.Phi]))
            .filter(!_.isInstanceOf[SSA.Phi])

//          log.pprint(invalidated)
          invalidated.foreach{
            case removed: SSA.Block => inferredBlocks.remove(removed)
            case removed: SSA.Jump => inferredBlocks.remove(removed.block)
            case removed: SSA.Val => evaluated.remove(removed)
            case _ => // do nothing
          }
        }

      }


      nextControl.upstream.collect{case v: SSA.Val => evaluate(v)}
      nextControl match{
        case nextBlock: SSA.Block => queueNextBlock(nextBlock)

        case n: SSA.Jump =>
          n match{
            case r: SSA.Return =>
              inferredReturns.append(evaluated(r.state))
            case r: SSA.ReturnVal =>
              inferredReturns.append(evaluated(r.src))
              inferredReturns.append(evaluated(r.state))
            case n: SSA.UnaBranch =>
              val valueA = evaluate(n.a)
              val doBranch = evaluateUnaBranch(valueA, n.opcode)

              doBranch match{
                case None =>
                  queueNextBlock(n.downstreamList.collect{ case t: SSA.True => t}.head)
                  queueNextBlock(n.downstreamList.collect{ case t: SSA.False => t}.head)

                case Some(bool) =>
                  if (bool) queueNextBlock(n.downstreamList.collect{ case t: SSA.True => t}.head)
                  else queueNextBlock(n.downstreamList.collect{ case t: SSA.False => t}.head)
              }
            case n: SSA.BinBranch =>
              val valueA = evaluate(n.a)
              val valueB = evaluate(n.b)
              val doBranch = evaluateBinBranch(valueA, valueB, n.opcode)
              doBranch match{
                case None =>
                  queueNextBlock(n.downstreamList.collect{ case t: SSA.True => t}.head)
                  queueNextBlock(n.downstreamList.collect{ case t: SSA.False => t}.head)

                case Some(bool) =>
                  if (bool) queueNextBlock(n.downstreamList.collect{ case t: SSA.True => t}.head)
                  else queueNextBlock(n.downstreamList.collect{ case t: SSA.False => t}.head)
              }
            case n: SSA.TableSwitch =>
              val value = evaluate(n.src)
              val cases = n.cases.values
              val default = n.default
              val doBranch = value match{
                case (CType.I(v), _) => Some(cases.find(_.n == v).getOrElse(default))
                case _ => None
              }

              doBranch match{
                case None =>
                  for(dest <- cases) queueNextBlock(dest)
                  queueNextBlock(default)

                case Some(dest) => queueNextBlock(dest)
              }
            case n: SSA.LookupSwitch =>

              val value = evaluate(n.src)
              val cases = n.cases.values
              val default = n.default
              val doBranch = evaluateSwitch(value)

              doBranch match{
                case None =>
                  for(dest <- cases) queueNextBlock(dest)
                  queueNextBlock(default)

                case Some(destValue) =>
                  val dest = cases.find(_.n == destValue).getOrElse(default)
                  queueNextBlock(dest)
              }
          }
      }
    }

    Result(
      inferredReturns,
      evaluated,
      inferredBlocks.toSet
    )
  }
}
