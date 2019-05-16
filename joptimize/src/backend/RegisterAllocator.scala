package joptimize.backend

import joptimize.{Logger, Util}
import joptimize.model.{MethodBody, SSA}

import scala.collection.mutable

object RegisterAllocator {
  def apply(methodBody: MethodBody,
            immediateDominators: Map[SSA.Block, SSA.Block],
            log: Logger.InferredMethod): Unit /*Map[SSA.Val, Int]*/ = log.block{
    val allVertices = Util.breadthFirstSeen[SSA.Node](methodBody.allTerminals.toSet)(_.upstream)

    val copies = mutable.Set.empty[SSA.Copy]
    val eqClses = mutable.Set.empty[Set[SSA.Val]]
    // PhiLifting
    for(v <- allVertices){
      v match{
        case phi: SSA.Phi if phi.getSize != 0 =>
          val arr = phi.incoming.toArray
          phi.incoming.clear()
          for((k, v) <- arr){
            v match{
              case _: SSA.Arg | _: SSA.ConstNull | _: SSA.ConstF | _: SSA.ConstD |
                   _: SSA.ConstI | _: SSA.ConstJ | _: SSA.ConstStr | _: SSA.ConstCls =>
                phi.incoming(k) = v
              case _ =>
                val copy = new SSA.Copy(v)
                v.downstreamRemove(phi)
                copy.downstreamAdd(phi)
                copies.add(copy)
                phi.incoming(k) = copy
            }
          }
          val replacement = new SSA.Copy(phi)
          phi.downstreamList.filter(_ != replacement).foreach{down =>
            down.replaceUpstream(phi, replacement)
            replacement.downstreamAdd(down)
            phi.downstreamRemoveAll(down)
          }

          eqClses.add(Set(phi) ++ phi.incoming.map(_._2))
        case _ => //donothing
      }
    }

  }
}
