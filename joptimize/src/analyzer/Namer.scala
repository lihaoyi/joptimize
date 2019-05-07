package joptimize.analyzer

import joptimize.Logger
import joptimize.Util.sortVerticesForPrinting
import joptimize.model.{MethodBody, SSA}

import scala.collection.mutable

/**
  * Walks the SSA program and assigns unique labels to each named node within
  * it, and unique offsets to values that get stored in local variable slots
  */
object Namer {
  case class Result(finalOrderingMap: Map[SSA.Node, Int],
                    saveable: Set[SSA.Node],
                    savedLocals: Map[SSA.Node, (Int, String)]){
    def apply(n: SSA.Node) = savedLocals.get(n).map(_._2)
  }

  def apply(methodBody: MethodBody,
            scheduledVals: Map[SSA.Val, SSA.Control],
            allVertices: Set[SSA.Node],
            log: Logger.InferredMethod): Result = log.block{

    val downstreamEdges = allVertices.flatMap(x => x.downstreamList.map(x -> _)).toSeq ++ scheduledVals.map(_.swap)

    val finalOrderingMap = sortVerticesForPrinting(allVertices, downstreamEdges) {
      case (_, _: SSA.Phi | _: SSA.Merge) => true
      case (v: SSA.Val, c: SSA.Control) => true
      case _ => false
    }

    val saveable =
      allVertices.filter { k =>
        val x = k.downstreamList
        val scheduled = k match {
          case v: SSA.Val =>
            val downstreamControls = x.collect {
              case t: SSA.Val => scheduledVals.get(t)
              case t: SSA.Control => Some(t)

            }.flatten
            scheduledVals.get(v).exists(c => downstreamControls.exists(_ != c)) ||
            v.downstreamList.exists(_.isInstanceOf[SSA.ChangedState])
          case _ => false
        }
        (k.upstream.nonEmpty || k.isInstanceOf[SSA.New]) && (k.downstreamSize > 1 || scheduled)
      } ++
      allVertices.collect {
        case k: SSA.Phi => k
        case c: SSA.Copy => c
        case b: SSA.Control => b
        case b: SSA.ChangedState => b
      }

    val savedLocals = mutable.Map[SSA.Val, (Int, String)]()
    val savedControls = mutable.Map[SSA.Control, (Int, String)]()

    var maxVal = 0
    log.pprint(methodBody.args)
    for(a <- methodBody.args){
      savedLocals.update(a, (a.index, "arg" + a.index))
      maxVal += a.getSize
    }

    log.pprint(maxVal)
    var maxControl = 0

    saveable.toSeq.sortBy(finalOrderingMap).collect{
      case r: SSA.Val =>
        savedLocals(r) = (
          maxVal,
          if (r.getSize == 0) ""
          else if (r.downstreamSize > 1) "local" + maxVal
          else "stack" + maxVal
        )
        maxVal += r.getSize

      case c: SSA.Control =>
        val str = c match{
          case _: SSA.Return | _: SSA.ReturnVal => "return" + maxControl
          case _: SSA.AThrow => "throw" + maxControl
          case n: SSA.True => "true" + savedControls(n.branch)._1 + "_" + maxControl
          case n: SSA.False => "false" + savedControls(n.branch)._1 + "_" + maxControl
          case r: SSA.Merge => (if (c.upstream.isEmpty) "start" else "block") + maxControl
          case _: SSA.UnaBranch | _: SSA.BinBranch=> "branch" + maxControl
          case _: SSA.LookupSwitch => "lookupswitch" + maxControl
          case _: SSA.TableSwitch => "tableswitch" + maxControl
          case _: SSA.Case => "case" + maxControl
          case _: SSA.Default => "default" + maxControl
        }
        savedControls(c) = (maxControl, str)
        maxControl += 1
    }

    Result(finalOrderingMap, saveable, (savedLocals ++ savedControls).toMap)
  }
}
