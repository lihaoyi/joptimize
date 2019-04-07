package joptimize.optimize

import joptimize.analyzer.Renderer
import joptimize.model._
import joptimize.{FileLogger, Logger, Util}

import scala.collection.mutable

object OptimisticSimplify {
  def apply(program: Program,
            inferred: mutable.LinkedHashMap[SSA.Val, (IType, Boolean)],
            liveBlocks: Set[SSA.Block],
            log: Logger.InferredMethod,
            classExists: JType.Cls => Boolean) = {

    for(n <- program.getAllVertices()){
//      log.graph(Renderer.dumpSvg(program))
//      log.pprint(n)
//      n match{case n: SSA.Val => log.pprint(inferred.get(n)) case _ =>}
      simplifyNode(n, inferred, classExists, log, liveBlocks)
    }

//    log.pprint(liveBlocks.map(x => (x, x.next)))
//    log.pprint(program.allTerminals.map{ case j: SSA.Jump => j -> j.block })
    program.allTerminals = program.allTerminals.filter{
      case j: SSA.Jump => liveBlocks.contains(j.block)
    }
//    log.pprint(program.allTerminals)

    log.graph(Renderer.dumpSvg(program))

  }

  def simplifyNode(node: SSA.Node,
                   inferred: mutable.LinkedHashMap[SSA.Val, (IType, Boolean)],
                   classExists: JType.Cls => Boolean,
                   log: Logger.InferredMethod,
                   liveBlocks: Set[SSA.Block]) = node match {
    case p: SSA.ChangedState =>
      log.pprint(inferred.contains(p))
    // do nothing
    case unInferred: SSA.Val if !inferred.contains(unInferred) =>
      log.pprint(unInferred)
    // do nothing
    case n: SSA.Invoke =>
      val (mangledName, mangledDesc) =
        if (n.name == "<init>" || !classExists(n.cls)) (n.name, n.desc)
        else Util.mangle(
          n.sig,
          n.srcs.map(inferred(_)._1).drop(if(n.sig.static) 0 else 1),
          inferred.getOrElseUpdate(n, (n.desc.ret, false))._1
        )

      if (inferred(n)._2){
        val replacement = inferred.get(n).map(_._1) match{
          case Some(CType.I(v)) => Some(new SSA.ConstI(v))
          case Some(CType.J(v)) => Some(new SSA.ConstJ(v))
          case Some(CType.F(v)) => Some(new SSA.ConstF(v))
          case Some(CType.D(v)) => Some(new SSA.ConstD(v))
          case _ => None
        }
        replacement match{
          case Some(r) =>
            inferred(r) = inferred(n)
            var upstreamState: SSA.ChangedState = null
            n.upstreamVals.foreach{
              case s: SSA.ChangedState =>
                upstreamState = s
                s.downstreamRemoveAll(n)
              case u => u.downstreamRemoveAll(n)
            }
            n.downstreamList.foreach{
              case s: SSA.ChangedState if upstreamState != null  =>
                s.parent = upstreamState
                upstreamState.downstreamAdd(s)
              case d =>
                r.downstreamAdd(d)
                d.replaceUpstream(n, r)
            }
          case None =>
            n.name = mangledName
            n.desc = mangledDesc
        }
      }else{
        n.name = mangledName
        n.desc = mangledDesc
      }

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
      val replacement = inferred.get(n).map(_._1) match{
        case Some(CType.I(v)) => Some(new SSA.ConstI(v))
        case Some(CType.J(v)) => Some(new SSA.ConstJ(v))
        case Some(CType.F(v)) => Some(new SSA.ConstF(v))
        case Some(CType.D(v)) => Some(new SSA.ConstD(v))
        case _ => None
      }

      replacement.foreach{r =>
        inferred(r) = inferred(n)
        var upstreamState: SSA.ChangedState = null
        n.upstreamVals.foreach{
          case s: SSA.ChangedState =>
            upstreamState = s
            s.downstreamRemoveAll(n)
          case u => u.downstreamRemoveAll(n)
        }
        n.downstreamList.foreach{
          case s: SSA.ChangedState if upstreamState != null  =>
            s.parent = upstreamState
            upstreamState.downstreamAdd(s)
          case d =>
            r.downstreamAdd(d)
            d.replaceUpstream(n, r)
        }
      }

    case j: SSA.Jump =>
      val allTargets = j.downstreamList.collect{case b: SSA.Block => b}
      val liveTargets = allTargets.filter(liveBlocks)
      if (liveTargets.isEmpty){
        // do nothing
      } else if (liveTargets.size == 1 && allTargets.size > 1){
        PartialEvaluator.replaceJump(j, liveTargets.head, liveBlocks, log)
      } else if (liveTargets.size < allTargets.size){
        ???
      }
    case _ =>
    // do nothing
  }
}
