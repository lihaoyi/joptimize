package joptimize.analysis

import joptimize.Util
import joptimize.model.{Program, SSA}

import scala.collection.mutable

object RegisterAllocator {
  def apply(program: Program,
            immediateDominators: Map[SSA.Block, SSA.Block]): Unit /*Map[SSA.Val, Int]*/ = {
    val (allVertices, _, _) =
      Util.breadthFirstAggregation[SSA.Node](program.allTerminals.toSet)(_.upstream)

    val copies = mutable.Set.empty[SSA.Copy]
    val eqClses = mutable.Set.empty[Set[SSA.Val]]
    // PhiLifting
    for(v <- allVertices){
      v match{
        case phi: SSA.Phi if phi.getSize != 0 =>
          phi.incoming = phi.incoming.map{ case (k, v) =>
            v match{
              case _: SSA.Arg | _: SSA.ConstNull | _: SSA.ConstF | _: SSA.ConstD |
                   _: SSA.ConstI | _: SSA.ConstJ | _: SSA.ConstStr | _: SSA.ConstCls =>
                (k, v)
              case _ =>
                val copy = SSA.Copy(v)
                v.downstreamRemove(phi)
                copy.downstreamAdd(phi)
                copies.add(copy)
                (k, copy)
            }
          }
          val replacement = SSA.Copy(phi)
          phi.downstreamList.filter(_ != replacement).foreach{down =>
            down.replaceUpstream(phi, replacement)
            replacement.downstreamAdd(down)
            phi.downstreamRemoveAll(down)
          }

          eqClses.add(Set(phi) ++ phi.incoming.map(_._2))
        case _ => //donothing
      }
    }
//    val phiEqClses = eqClses.flatten
//    copies.map(_.src)
//      .filter(!phiEqClses.contains(_))
//      .foreach{src => eqClses.add(Set(src))}
//
//    pprint.log(eqClses)
//    // PhiMemCoalesce
//    for(copy <- copies){
//      val copyEqCls = eqClses.find(_.contains(copy)).get
//      val srcEqCls = eqClses.find(_.contains(copy.src)).get
//      pprint.log(copy)
//      pprint.log(copyEqCls)
//      pprint.log(srcEqCls)
//      if (copyEqCls.intersect(srcEqCls).isEmpty){
//        eqClses.remove(copyEqCls)
//        eqClses.remove(srcEqCls)
//        eqClses.add(copyEqCls.union(srcEqCls))
//      }
//    }
//
//    pprint.log(eqClses)

//    for(copy <- copies){
//      if (eqClses.find(_.contains(copy.src)) == eqClses.find(_.contains(copy))){
//        copy.src.downstream.remove(copy)
//        copy.downstream.foreach(copy.src.downstream.add)
//        val phi = copy.downstream.head.asInstanceOf[SSA.Phi]
//        phi.incoming = phi.incoming.map{ case (k, v) => (k, if (v == copy) copy.src else v) }
//      }
//    }
  }
}
