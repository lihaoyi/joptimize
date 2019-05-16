//package joptimize.analyzer
//
//import joptimize.Logger
//import joptimize.Util.sortVerticesForPrinting
//import joptimize.model.{MethodBody, SSA}
//
//import scala.collection.mutable
//
///**
//  * Walks the SSA program and assigns unique labels to each named node within
//  * it, and unique offsets to values that get stored in local variable slots
//  */
//object Namer {
//  case class Result(savedLocals: Map[SSA.Val, Int])
//
//  def apply(methodBody: MethodBody,
//            scheduledVals: Map[SSA.ValOrState, SSA.Control],
//            allVertices: Set[SSA.Node],
//            log: Logger.InferredMethod): Result = log.block{
//
//    val downstreamEdges = allVertices.flatMap(x => x.downstreamList.map(x -> _)).toSeq ++ scheduledVals.map(_.swap)
//
//    val finalOrderingMap = sortVerticesForPrinting(allVertices, downstreamEdges) {
//      case (_, _: SSA.Phi | _: SSA.Merge) => true
//      case (v: SSA.Val, c: SSA.Control) => true
//      case _ => false
//    }
//
//    val saveable = allVertices
//      .collect{case v: SSA.Val => v}
//      .filter {
//        case p: SSA.Phi => true
//        case n =>
//          val scheduled = n match {
//            case v: SSA.Val =>
//              val downstreamControls = n.downstreamList.collect {
//                case t: SSA.Val => scheduledVals.get(t)
//                case t: SSA.Control => Some(t)
//
//              }.flatten
//              (scheduledVals.get(v).exists(c => downstreamControls.exists(_ != c)) ||
//              v.downstreamList.exists(_.isInstanceOf[SSA.State])) &&
//              v.getSize != 0
//            case _ => false
//          }
//          (n.upstream.nonEmpty || n.isInstanceOf[SSA.New]) && (n.downstreamSize > 1 || scheduled)
//      }
//
//    val savedLocals = mutable.Map[SSA.Val, Int]()
//
//    var maxVal = 0
//
//    for(a <- methodBody.args){
//      savedLocals(a) = a.index
//      maxVal += a.getSize
//    }
//
//    log.pprint(saveable)
//    saveable.toSeq.sortBy(finalOrderingMap).collect{
//      case r: SSA.Val =>
//        savedLocals(r) = maxVal
//        maxVal += r.getSize
//    }
//
//    Result(savedLocals.toMap)
//  }
//}
