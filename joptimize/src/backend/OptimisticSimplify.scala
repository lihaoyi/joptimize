package joptimize.backend

import joptimize.analyzer.{ProgramAnalyzer, Renderer}
import joptimize.model._
import joptimize.{Logger, Util}

import scala.collection.mutable

object OptimisticSimplify {
  def apply(isStatic: Boolean,
            argMapping: Map[Int, Int],
            methodBody: MethodBody,
            inferred: mutable.LinkedHashMap[SSA.Val, IType],
            liveBlocks: Set[SSA.Block],
            log: Logger.InferredMethod,
            classExists: JType.Cls => Boolean,
            resolvedProperties: (InferredSig, Boolean) => ProgramAnalyzer.Properties) = log.block{

    methodBody.args = methodBody.args.filter(a => argMapping.contains(a.index) || (a.index == 0 && !isStatic))

//    log.pprint(argMapping)
    for(n <- methodBody.getAllVertices()){
      // log.global().graph(n.toString())(Renderer.dumpSvg(methodBody))
//      log.pprint(n)
//      n match{case n: SSA.Val => log.pprint(inferred.get(n)) case _ =>}
      simplifyNode(n, inferred, classExists, log, liveBlocks, resolvedProperties, argMapping)
    }

//    log.pprint(liveBlocks.map(x => (x, x.next)))
//    log.pprint(program.allTerminals.map{ case j: SSA.Jump => j -> j.block })
    methodBody.allTerminals = methodBody.allTerminals.filter{
      case j: SSA.Jump => liveBlocks.contains(j.block)
    }
  }

  def simplifyNode(node: SSA.Node,
                   inferred: mutable.LinkedHashMap[SSA.Val, IType],
                   classExists: JType.Cls => Boolean,
                   log: Logger.InferredMethod,
                   liveBlocks: Set[SSA.Block],
                   resolvedProperties: (InferredSig, Boolean) => ProgramAnalyzer.Properties,
                   argMapping: Map[Int, Int]) = node match {
    case p: SSA.ChangedState =>
//      log.pprint(inferred.contains(p))
    // do nothing
    case unInferred: SSA.Val if !inferred.contains(unInferred) =>
//      log.pprint(unInferred)
    // do nothing
    case n: SSA.Invoke =>
      val inferredSig = n.inferredSig(inferred)
      val properties = resolvedProperties(
        inferredSig,
        n.isInstanceOf[SSA.InvokeSpecial]
      )
      //      log.pprint(n)
      val (mangledName, mangledDesc, liveArgsOpt) =
        if (n.name == "<init>" || !classExists(n.cls)) (n.name, n.desc, None)
        else {
          //          log.pprint(n.sig)
//          log.pprint(inferredArgs)
//          log.pprint(liveArgs)
          val (name, desc) = Util.mangle(
            inferredSig,
            inferred.getOrElseUpdate(n, n.desc.ret),
            properties.liveArgs
          )

          (name, desc, Some(properties.liveArgs))
        }

      val mangledCls =
        if (n.isInstanceOf[SSA.InvokeStatic] || n.isInstanceOf[SSA.InvokeSpecial]) n.cls
        else inferred(n.srcs(0)).asInstanceOf[JType.Cls]

      if (properties.pure){
        val replacement = prepareNodeReplacement(inferred, n)
        replacement match{
          case Some(r) => constantFoldNode(inferred, r, n)
          case None =>
            purifyNode(n)
            mangleInvocation(n, liveArgsOpt, mangledName, mangledDesc, mangledCls)
        }
      }else{
        mangleInvocation(n, liveArgsOpt, mangledName, mangledDesc, mangledCls)
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
        val live = liveBlocks(t._1)
        if (!live) t._1.next = null
        live
      }

    case n: SSA.Val =>
      n match{
        case a: SSA.Arg if argMapping.contains(a.index) => a.index = argMapping(a.index)
        case _ => //do nothing
      }
      val replacement = prepareNodeReplacement(inferred, n)

      for(r <- replacement) constantFoldNode(inferred, r, n)

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

  def prepareNodeReplacement(inferred: mutable.LinkedHashMap[SSA.Val, IType],
                             n: SSA.Val) = {
    inferred(n) match {
      case CType.I(v) => Some(new SSA.ConstI(v))
      case CType.J(v) => Some(new SSA.ConstJ(v))
      case CType.F(v) => Some(new SSA.ConstF(v))
      case CType.D(v) => Some(new SSA.ConstD(v))
      case _ => None
    }
  }

  /**
    * Removes a node entirely and replace it with a constant value
    */
  def constantFoldNode(inferred: mutable.LinkedHashMap[SSA.Val, IType],
                       replacement: SSA.Val,
                       original: SSA.Val) = {
    inferred(replacement) = inferred(original)
    var upstreamState: SSA.State = null
    original.upstreamVals.foreach {
      case s: SSA.ChangedState =>
        upstreamState = s
        s.downstreamRemoveAll(original)
      case u => u.downstreamRemoveAll(original)
    }
    original.downstreamList.foreach {
      case s: SSA.ChangedState if upstreamState != null =>
        s.parent = upstreamState
        upstreamState.downstreamAdd(s)
      case d =>
        replacement.downstreamAdd(d)
        d.replaceUpstream(original, replacement)
    }
  }

  /**
    * Leaves a node in place but removes its effect on the state edges
    */
  def purifyNode(n: SSA.Val) = {
    var upstreamState: SSA.ChangedState = null
    n.upstreamVals.foreach {
      case s: SSA.ChangedState =>
        upstreamState = s
      case _ => // do nothing
    }

    var downstreamState: SSA.ChangedState = null
    n.downstreamList.foreach {
      case s: SSA.ChangedState if upstreamState != null =>
        downstreamState = s
        s.parent = upstreamState
        upstreamState.downstreamAdd(s)
      case _ => // do nothing
    }
    if (upstreamState != null) {
      n.downstreamRemove(downstreamState)
    }
  }

  def mangleInvocation(n: SSA.Invoke,
                       liveArgsOpt: Option[Set[Int]],
                       mangledName: String,
                       mangledDesc: Desc,
                       mangledCls: JType.Cls) = {
    for (liveArgs <- liveArgsOpt) {
      val (live, die) = n.srcs.zipWithIndex.partition {
        case (a, i) => (!n.sig.static && i == 0) || liveArgs(i)
      }
      n.srcs = live.map(_._1)
      die.map(_._1).foreach(_.downstreamRemove(n))
    }
    n.name = mangledName
    n.desc = mangledDesc
    n.cls = mangledCls
  }
}
