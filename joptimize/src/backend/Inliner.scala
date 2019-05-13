package joptimize.backend

import joptimize.{Logger, Util}
import joptimize.analyzer.ProgramAnalyzer.{CallEdge, MethodResult}
import joptimize.analyzer.{ProgramAnalyzer, Renderer}
import joptimize.frontend.ClassManager
import joptimize.model.{IType, InferredSig, JType, MethodBody, SSA}

import scala.collection.mutable

object Inliner {
  def inlineAll(analyzerRes: ProgramAnalyzer.ProgramResult,
                classManager: ClassManager.ReadOnly,
                log: Logger.Global) = log.block{
    val filteredCallgraph = analyzerRes
      .callGraph
      .filter(e => e.called.method.name != "<init>" && e.called.method.name != "<clinit>")
    val singleCallerEdges = filteredCallgraph
      .groupBy(_.called)
      .values
      .collect { case Seq(singleEdge) => singleEdge }

    val singleCalledEdges = filteredCallgraph
      .groupBy(_.caller)
      .values
      .collect { case Seq(singleEdge) => singleEdge }

    val callGraphLookup = filteredCallgraph.groupBy(_.called).collect{
      case (src, Seq(dest)) => (src, dest.caller)
    }
    val inlineableEdgesSet = singleCallerEdges.to[mutable.LinkedHashSet]
      .intersect(singleCalledEdges.to[mutable.LinkedHashSet])

    log.pprint(inlineableEdgesSet.map(_.toString))
    val inlinedMethodsSet = inlineableEdgesSet.map(_.called)
    val visitedMethods = mutable.LinkedHashMap.empty[InferredSig, MethodResult]
    for((k, v) <- analyzerRes.visitedMethods){
      visitedMethods.put(k, v)
    }

    for {
      edge <- inlineableEdgesSet
      node <- edge.node
      if edge.caller != edge.called
    } {
      var caller = edge.caller
      //      pprint.log(analyzerRes.visitedMethods.keys)
      //      pprint.log(edge)
      while({
        if (visitedMethods.contains(caller)) {
          inlineSingleMethod(
            classManager,
            log,
            visitedMethods,
            edge.copy(caller = caller),
            node
          )
          false
        }
        else{
          caller = callGraphLookup(caller)
          true
        }
      })()

    }

    ProgramAnalyzer.ProgramResult(
      visitedMethods,
      analyzerRes.visitedResolved.filterKeys(!inlinedMethodsSet.contains(_)),
      analyzerRes.staticFieldReferencedClasses,
      analyzerRes.callGraph.filter(!inlineableEdgesSet(_))
    )
  }

  def inlineSingleMethod(classManager: ClassManager.ReadOnly,
                         log: Logger.Global,
                         visitedMethods: mutable.LinkedHashMap[InferredSig, MethodResult],
                         edge: CallEdge,
                         node: SSA.Invoke) = {
    val nodeBlock = node.block.get
    val calledMethod = visitedMethods(edge.called)
    log.pprint(node)
    log.pprint(edge)
    log.graph("edge.caller") {
      Renderer.dumpSvg(visitedMethods(edge.caller).methodBody)
    }
    log.graph("edge.called") {
      Renderer.dumpSvg(visitedMethods(edge.called).methodBody)
    }
    val calledBody = calledMethod.methodBody
    val calledStartBlock = calledMethod.liveBlocks.collect { case s: SSA.Start => s }.head

    // Wire up input params, state and block
    calledStartBlock.downstreamList.foreach(_.replaceUpstream(calledStartBlock, nodeBlock))

    for ((arg, src) <- calledBody.args.zip(node.srcs)) {
      src.downstreamRemove(node)
      Util.replace(arg, src)
    }
    Util.replace(calledStartBlock.startingState, node.state)

    // Collect returns
    val returns = mutable.Buffer.empty[(SSA.State, SSA.Jump, Option[SSA.Val])]
    val throws = mutable.Buffer.empty[SSA.AThrow]
    calledBody.allTerminals.foreach {
      case r: SSA.ReturnVal => returns.append((r.state, r, Some(r.src)))
      case r: SSA.Return => returns.append((r.state, r, None))
      case a: SSA.AThrow => throws.append(a)
    }

    val Seq(nextState) = node.downstreamList.collect { case s: SSA.ChangedState => s }

    val outputNodes = prepareOutputNodes(classManager, log, nodeBlock, returns, nextState, visitedMethods(edge.caller).methodBody)
    val addedBlocks = outputNodes match{
      case Some((inlinedLastBlock, inlinedFinalValOpt, inlinedFinalState)) =>

        // Wire up return value, state and block

        inlinedFinalValOpt.foreach(Util.replace(node, _))

        Util.replace(nextState, inlinedFinalState)

        inlinedLastBlock.next = nodeBlock.next
        // Wire up return block
        nodeBlock.next.replaceUpstream(nodeBlock, inlinedLastBlock)
        nodeBlock.next match {
          case m: SSA.Merge =>
            m.phis.foreach { phi =>
              nodeBlock.nextPhis = nodeBlock.nextPhis.filter(_ != phi)
              inlinedLastBlock.nextPhis = Seq(phi) ++ inlinedLastBlock.nextPhis
              phi.replaceUpstream(nodeBlock, inlinedLastBlock)
            }
          case _ => // do nothing
        }
        Seq(inlinedLastBlock)
      case None =>
        node.upstreamVals.foreach(_.downstreamRemove(node))
        for(t <- throws){
          t.block
        }
        Nil
    }

    // Wire up input block
    if (outputNodes.isEmpty || !calledBody.allTerminals.contains(calledStartBlock.next)){
      // If the inlined method has internal control flow, the input block and output
      // blocks are different and we have to wire them up properly
      nodeBlock.next = calledStartBlock.next
      nodeBlock.nextPhis = calledStartBlock.nextPhis
    }

    nodeBlock.blockInvokes = calledStartBlock.blockInvokes ++ nodeBlock.blockInvokes.filter(_ != node)

    // Consolidate two method bodies in `visitedMethods`
    val newLiveBlocks =
      visitedMethods(edge.caller).liveBlocks ++
      visitedMethods(edge.called).liveBlocks ++
      addedBlocks

    visitedMethods(edge.caller) = visitedMethods(edge.caller).copy(
      inferred = visitedMethods(edge.caller).inferred ++ visitedMethods(edge.called).inferred,
      liveBlocks = newLiveBlocks
    )

    visitedMethods.remove(edge.called)

    visitedMethods(edge.caller).methodBody.allTerminals =
      visitedMethods(edge.caller).methodBody.allTerminals ++
      throws
    visitedMethods(edge.caller).methodBody.removeDeadNodes()
    log.graph("INLINED " + edge) {
      Renderer.dumpSvg(visitedMethods(edge.caller).methodBody)
    }

    log.check(visitedMethods(edge.caller).methodBody.checkLinks())
    visitedMethods.remove(edge.called)
  }

  def prepareOutputNodes(classManager: ClassManager.ReadOnly,
                         log: Logger.Global,
                         nodeBlock: SSA.Block,
                         returns: mutable.Buffer[(SSA.State, SSA.Jump, Option[SSA.Val])],
                         nextState: SSA.ChangedState,
                         methodBody: MethodBody) = {
    returns match {
      case Seq() => None
      case Seq((returnedState, returnNode, returnedValOpt)) =>
        returnedState.downstreamRemoveAll(returnNode)
        returnedValOpt.foreach(_.downstreamRemoveAll(returnNode))

        Some((returnNode.block, returnedValOpt, returnedState))

      case triplets => // multiple returns

        log.pprint(triplets)
        val valPhiOpt =
          if (!returns.forall(_._3.isDefined)) None
          else Some(new SSA.Phi(
            null,
            triplets.map { case (s, j, v) => (j.block, v.get) }.toSet,
            classManager.mergeTypes(triplets.map { case (s, j, v) => v.get.jtype }).asInstanceOf[JType]
          ))

        val statePhi = new SSA.Phi(
          null,
          triplets.map { case (s, j, v) => (j.block, s) }.toSet,
          JType.Prim.V
        )

        val merge = new SSA.Merge(
          triplets.map(_._2.block).toSet,
          next = nodeBlock.next,
          phis = Seq(statePhi) ++ valPhiOpt
        )

        triplets.foreach { case (s, j, v) =>
          j.block.next = merge
          j.block.nextPhis ++= Seq(statePhi) ++ valPhiOpt
          v.foreach(_.downstreamRemoveAll(j))
          s.downstreamRemoveAll(j)
        }

        valPhiOpt.foreach(_.block = merge)
        statePhi.block = merge

        Some((merge, valPhiOpt, statePhi))
    }
  }
}
