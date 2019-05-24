package joptimize.backend

import joptimize.analyzer.{ProgramAnalyzer, Renderer}
import joptimize.model._
import joptimize.{Logger, Util}

import scala.collection.mutable

object OptimisticSimplify {
  def apply(
    isStatic: Boolean,
    argMapping: Map[Int, Int],
    methodBody: MethodBody,
    inferred: mutable.LinkedHashMap[SSA.Val, IType],
    liveBlocks: Set[SSA.Block],
    log: Logger.InferredMethod,
    classExists: JType.Cls => Boolean,
    resolvedProperties: (InferredSig, Boolean) => ProgramAnalyzer.Properties
  ) = log.block {

    methodBody.args =
      methodBody.args.filter(a => argMapping.contains(a.index) || (a.index == 0 && !isStatic))

//    log.pprint(inferred)
    for (n <- methodBody.getAllVertices()) {
      simplifyNode(n, inferred, classExists, log, liveBlocks, resolvedProperties, argMapping)
//      log.graph(n.toString())(Renderer.dumpSvg(methodBody))
    }

    methodBody.allTerminals = methodBody.allTerminals.filter {
      case j: SSA.Jump => liveBlocks.contains(j.block)
    }
  }

  def simplifyNode(
    node: SSA.Node,
    inferred: mutable.LinkedHashMap[SSA.Val, IType],
    classExists: JType.Cls => Boolean,
    log: Logger.InferredMethod,
    liveBlocks: Set[SSA.Block],
    resolvedProperties: (InferredSig, Boolean) => ProgramAnalyzer.Properties,
    argMapping: Map[Int, Int]
  ) = node match {
    case p: SSA.State => // do nothing
    case unInferred: SSA.Val
      if !inferred.contains(unInferred) || inferred(unInferred) == JType.Bottom=> // do nothing
    case n: SSA.Invoke =>
      val inferredSig =
        if (n.name == "<init>") InferredSig(n.sig, n.sig.desc.args)
        else n.inferredSig(inferred)
      val properties = resolvedProperties(
        inferredSig,
        n.isInstanceOf[SSA.InvokeSpecial]
      )
      val (mangledName, mangledDesc, liveArgsOpt) =
        if (n.name == "<init>" || !classExists(n.cls)) (n.name, n.desc, None)
        else {
          val (name, desc) = Util.mangle(
            inferredSig,
            properties.inferredReturn,
            properties.liveArgs
          )

          (name, desc, Some(properties.liveArgs))
        }

      val mangledCls =
        if (n.isInstanceOf[SSA.InvokeStatic] || n.isInstanceOf[SSA.InvokeSpecial]) n.cls
        else
          inferred(n.srcs(0)) match {
            case j: JType.Cls => j
            case a: JType.Arr => JType.Cls("java/lang/Object")
          }

      if (properties.pure) {
        val replacement = prepareNodeReplacement(inferred, n)
        replacement match {
          case Some(r) => constantFoldNode(inferred, r, n)
          case None =>
            purifyNode(n)

            mangleInvocation(n, liveArgsOpt, mangledName, mangledDesc, mangledCls)
        }
      } else {
        mangleInvocation(n, liveArgsOpt, mangledName, mangledDesc, mangledCls)
      }

    case p: SSA.Phi =>
      for ((k, v) <- p.incoming.toArray) {
        val live = liveBlocks(k)
        if (!live) {
          k.next = null
          v.downstreamRemove(p)
          p.incoming.remove(k)
        }
      }

    case m: SSA.Merge =>
      for ((k, v) <- m.incoming.toArray) {
        val live = liveBlocks(k)
        if (!live) {
          k.next = null
          m.incoming.remove(k)
        }
      }

    case n: SSA.Val =>
      n match {
        case a: SSA.Arg if argMapping.contains(a.index) => a.index = argMapping(a.index)
        case _ => //do nothing
      }
      val replacement = prepareNodeReplacement(inferred, n)

      for (r <- replacement) constantFoldNode(inferred, r, n)

    case j: SSA.Jump =>
      val allTargets = j.downstreamList.collect { case b: SSA.Block => b }
      val liveTargets = allTargets.filter(liveBlocks)
      if (liveTargets.isEmpty) {
        // do nothing
      } else if (liveTargets.size == 1 && allTargets.size > 1) {
        PartialEvaluator.replaceJump(j, liveTargets.head, liveBlocks, log)
      } else if (liveTargets.size < allTargets.size) {
        ???
      }
    case _ =>
    // do nothing
  }

  def prepareNodeReplacement(inferred: mutable.LinkedHashMap[SSA.Val, IType], n: SSA.Val) = {
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
  def constantFoldNode(
    inferred: mutable.LinkedHashMap[SSA.Val, IType],
    replacement: SSA.Val,
    original: SSA.Val
  ) = {
    inferred(replacement) = inferred(original)
    var upstreamState: SSA.State = null
    original.upstream.foreach {
      case s: SSA.State =>
        upstreamState = s
        s.next = null
      case u: SSA.Val => u.downstreamRemoveAll(original)
    }
    original.downstreamList.foreach {
      case s: SSA.State if upstreamState != null =>
        s.parent = upstreamState
        upstreamState.next = s
      case d =>
        replacement.downstreamAdd(d)
        d.replaceUpstream(original, replacement)
    }
  }

  /**
    * Leaves a node in place but removes its effect on the state edges
    */
  def purifyNode(n: SSA.Invoke) = {

    val upstreamState = n.state

    var downstreamState: SSA.State = null
    n.downstreamList.toArray.foreach {
      case s: SSA.State if upstreamState != null =>
        downstreamState = s
        s.parent = upstreamState
        upstreamState.next = s
      case _ => // do nothing
    }
    if (upstreamState != null) {
      n.downstreamRemove(downstreamState)
    }
    n.state = null
  }

  def mangleInvocation(
    n: SSA.Invoke,
    liveArgsOpt: Option[Set[Int]],
    mangledName: String,
    mangledDesc: Desc,
    mangledCls: JType.Cls
  ) = {
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
