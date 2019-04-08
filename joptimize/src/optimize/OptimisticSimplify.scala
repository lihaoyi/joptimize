package joptimize.optimize

import joptimize.analyzer.Renderer
import joptimize.model._
import joptimize.{FileLogger, Logger, Util}

import scala.collection.mutable

object OptimisticSimplify {
  def apply(isStatic: Boolean,
            argMapping: Map[Int, Int],
            program: Program,
            inferred: mutable.LinkedHashMap[SSA.Val, (IType, Boolean, Set[Int])],
            liveBlocks: Set[SSA.Block],
            log: Logger.InferredMethod,
            classExists: JType.Cls => Boolean,
            liveArgsFor: (MethodSig, Boolean, Seq[IType]) => Set[Int]) = {

    log.pprint(program.args -> argMapping)
    program.args = program.args.filter(a => argMapping.contains(a.index) || (a.index == 0 && !isStatic))
    log.pprint(program.args -> argMapping)

//    log.pprint(argMapping)
    for(n <- program.getAllVertices()){
//      log.graph(Renderer.dumpSvg(program))
//      log.pprint(n)
//      n match{case n: SSA.Val => log.pprint(inferred.get(n)) case _ =>}
      simplifyNode(n, inferred, classExists, log, liveBlocks, liveArgsFor, argMapping)
    }

//    log.pprint(liveBlocks.map(x => (x, x.next)))
//    log.pprint(program.allTerminals.map{ case j: SSA.Jump => j -> j.block })
    program.allTerminals = program.allTerminals.filter{
      case j: SSA.Jump => liveBlocks.contains(j.block)
    }


//    log.pprint(program.allTerminals)

    log.println("POST OPTIMISTIC SIMPLIFY")
    log.graph(Renderer.dumpSvg(program))

  }

  def simplifyNode(node: SSA.Node,
                   inferred: mutable.LinkedHashMap[SSA.Val, (IType, Boolean, Set[Int])],
                   classExists: JType.Cls => Boolean,
                   log: Logger.InferredMethod,
                   liveBlocks: Set[SSA.Block],
                   liveArgsFor: (MethodSig, Boolean, Seq[IType]) => Set[Int],
                   argMapping: Map[Int, Int]) = node match {
    case p: SSA.ChangedState =>
//      log.pprint(inferred.contains(p))
    // do nothing
    case unInferred: SSA.Val if !inferred.contains(unInferred) =>
//      log.pprint(unInferred)
    // do nothing
    case n: SSA.Invoke =>
//      log.pprint(n)
      val (mangledName, mangledDesc, liveArgsOpt) =
        if (n.name == "<init>" || !classExists(n.cls)) (n.name, n.desc, None)
        else {

          val inferredArgs0 = n.srcs.map(inferred(_)._1)
          val liveArgs = liveArgsFor(n.sig, n.isInstanceOf[SSA.InvokeSpecial], inferredArgs0)
          log.pprint(liveArgs)
          val inferredArgs = inferredArgs0.drop(if(n.sig.static) 0 else 1)
          //          log.pprint(n.sig)
//          log.pprint(inferredArgs)
//          log.pprint(liveArgs)
          val (name, desc) = Util.mangle(
            n.sig,
            inferredArgs,
            inferred.getOrElseUpdate(n, (n.desc.ret, false, n.desc.args.indices.toSet))._1,
            liveArgs
          )
          val liveArgsOut: Set[Int] = {

            var originalIndex = if (n.sig.static) 0 else 1
            log.pprint(originalIndex)
            val output = mutable.Set.empty[Int]
            for((arg, i) <- n.sig.desc.args.zipWithIndex){
              if (liveArgs(originalIndex)){
                output.add(i + (if (n.sig.static) 0 else 1))
              }
              originalIndex += arg.size
            }

            output.toSet
          }
          log.pprint(liveArgsOut)

          (name, desc, Some(liveArgsOut))
        }

      log.pprint(liveArgsOpt)
      val (inferredType, pure, _) = inferred(n)
      if (pure){
        val replacement = inferredType match{
          case CType.I(v) => Some(new SSA.ConstI(v))
          case CType.J(v) => Some(new SSA.ConstJ(v))
          case CType.F(v) => Some(new SSA.ConstF(v))
          case CType.D(v) => Some(new SSA.ConstD(v))
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
            var upstreamState: SSA.ChangedState = null
            n.upstreamVals.foreach{
              case s: SSA.ChangedState =>
                upstreamState = s
              case _ => // do nothing
            }

            var downstreamState: SSA.ChangedState = null
            n.downstreamList.foreach{
              case s: SSA.ChangedState if upstreamState != null  =>
                downstreamState = s
                s.parent = upstreamState
                upstreamState.downstreamAdd(s)
              case _ => // do nothing
            }
            if (upstreamState != null){
              n.downstreamRemove(downstreamState)
            }
            for(liveArgs <- liveArgsOpt){

              val (live, die) = n.srcs.zipWithIndex.partition{
                case (a, i) => (!n.sig.static && i == 0) || liveArgs(i)
              }
              n.srcs = live.map(_._1)
              die.map(_._1).foreach(_.downstreamRemove(n))
            }
            n.name = mangledName
            n.desc = mangledDesc
        }
      }else{
        for(liveArgs <- liveArgsOpt){
          val (live, die) = n.srcs.zipWithIndex.partition{
            case (a, i) => (!n.sig.static && i == 0) || liveArgs(i)
          }
          n.srcs = live.map(_._1)
          die.map(_._1).foreach(_.downstreamRemove(n))
        }
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
      n match{
        case a: SSA.Arg if argMapping.contains(a.index) => a.index = argMapping(a.index)
        case _ => //do nothing
      }
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
