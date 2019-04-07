package joptimize.optimize

import joptimize.analyzer.Renderer
import joptimize.model._
import joptimize.{FileLogger, Logger, Util}

import scala.collection.mutable

object OptimisticSimplify {
  def apply(program: Program,
            inferred: mutable.LinkedHashMap[SSA.Val, IType],
            liveBlocks: Set[SSA.Block],
            log: Logger.InferredMethod,
            classExists: JType.Cls => Boolean) = {

    val calledMethodSigs = mutable.Set.empty[MethodSig]
    program.getAllVertices().foreach{
      case unInferred: SSA.Val if !inferred.contains(unInferred) =>
        log.pprint(unInferred)
      // do nothing
      case p: SSA.ChangedState => // do nothing
      case n: SSA.Invoke =>
        val (mangledName, mangledDesc) =
          if (n.name == "<init>" || !classExists(n.cls)) (n.name, n.desc)
          else Util.mangle(
            n.sig,
            n.srcs.map(inferred).drop(if(n.sig.static) 0 else 1),
            inferred.getOrElseUpdate(n, n.desc.ret)
          )
        calledMethodSigs.add(n.sig)
        n.name = mangledName
        n.desc = mangledDesc

      case p: SSA.Phi =>
        p.incoming = p.incoming.filter{t =>
          val live = liveBlocks(t._1)
          if (!live) {
            t._1.next = null
            t._2.downstreamRemove(p)
          }
          live
        }
      case m: SSA.Merge =>
        m.incoming = m.incoming.filter{ t =>
          val live = liveBlocks(t)
          if (!live) t.next = null
          live
        }

      case n: SSA.Val =>
        val replacement = inferred.get(n) match{
          case Some(CType.I(v)) => Some(new SSA.ConstI(v))
          case Some(CType.J(v)) => Some(new SSA.ConstJ(v))
          case Some(CType.F(v)) => Some(new SSA.ConstF(v))
          case Some(CType.D(v)) => Some(new SSA.ConstD(v))
          case _ => None
        }
        replacement.foreach{r =>
          inferred(r) = inferred(n)
          n.upstreamVals.foreach(_.downstreamRemoveAll(n))
          for(d <- n.downstreamList) {
            r.downstreamAdd(d)
            d.replaceUpstream(n, r)
          }
        }

      case j: SSA.Jump =>
        val allTargets = j.downstreamList.collect{case b: SSA.Block => b}
        val liveTargets = allTargets.filter(liveBlocks)
        if (liveTargets.isEmpty){
         // do nothing
        } else if (liveTargets.size == 1){
          PartialEvaluator.replaceJump(j, liveTargets.head, liveBlocks, log)
        } else if (liveTargets.size < allTargets.size){
          ???
        }
      case _ => // do nothing
    }

//    log.pprint(program.allTerminals)
    program.allTerminals = program.allTerminals.filter{
      case j: SSA.Jump => liveBlocks.contains(j.block)
      case t: SSA.AThrow => liveBlocks.contains(t.block)
    }
//    log.pprint(liveBlocks)
//    log.pprint(program.allTerminals)

    log.graph(Renderer.dumpSvg(program))
    //      program.checkLinks(checkDead = false)

    calledMethodSigs
  }
}
