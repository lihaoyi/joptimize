package joptimize.backend

import joptimize.{Logger, Util}
import joptimize.analyzer.ProgramAnalyzer.{CallEdge, MethodResult}
import joptimize.analyzer.{ProgramAnalyzer, Renderer}
import joptimize.frontend.ClassManager
import joptimize.model.{InferredSig, JType, MethodBody, SSA}

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

    inlineWireParams(log, node, calledBody)


    val returnedVals = mutable.Buffer.empty[(SSA.Val, SSA.Jump)]
    val returnedStates = mutable.Buffer.empty[(SSA.State, SSA.Jump)]
    val throws = mutable.Buffer.empty[SSA.AThrow]
    calledBody.allTerminals.foreach {
      case r: SSA.ReturnVal =>
        returnedVals.append(r.src -> r)
        returnedStates.append(r.state -> r)
      case r: SSA.Return =>
        returnedStates.append(r.state -> r)
      case a: SSA.AThrow => throws.append(a)
    }

    val Seq(nextState) = node.downstreamList.collect { case s: SSA.ChangedState => s }

    val addedBlocks = (returnedVals, returnedStates) match {
      case (Seq(), Seq((rState, ret1))) =>
        Util.replace(nextState, rState)
        rState.downstreamRemoveAll(ret1)
        Nil
      case (Seq((rVal, ret0)), Seq((rState, ret1))) =>
        Util.replace(nextState, rState)
        rState.downstreamRemoveAll(ret1)
        Util.replace(node, rVal)
        rVal.downstreamRemoveAll(ret0)
        ret0.block.next = nodeBlock.next
        nodeBlock.next.replaceUpstream(nodeBlock, ret0.block)
        nodeBlock.next match {
          case m: SSA.Merge =>
            m.phis.foreach { phi =>
              nodeBlock.nextPhis = nodeBlock.nextPhis.filter(_ != phi)
              ret0.block.nextPhis = Seq(phi) ++ ret0.block.nextPhis
              phi.replaceUpstream(nodeBlock, ret0.block)
            }
          case _ => // do nothing
        }

        Nil

      case (Seq(), states) => // multiple returns

        log.pprint(states)
        states.foreach { case (s, j) =>
          s.downstreamRemoveAll(j)
        }

        val statePhi = new SSA.Phi(
          null,
          states.map { case (s, j) => (j.block, s) }.toSet,
          JType.Prim.V
        )

        states.foreach { case (s, j) => j.block.nextPhis ++= Seq(statePhi) }
        val merge = new SSA.Merge(
          states.map(_._2.block).toSet,
          next = nodeBlock.next,
          phis = Seq(statePhi)
        )

        states.foreach { case (s, j) =>
          j.block.next = merge
        }
        statePhi.block = merge

        nodeBlock.next.replaceUpstream(nodeBlock, merge)
        nodeBlock.next match {
          case m: SSA.Merge =>
            m.phis.foreach { phi =>
              nodeBlock.nextPhis = nodeBlock.nextPhis.filter(_ != phi)
              merge.nextPhis = Seq(phi) ++ merge.nextPhis
              phi.replaceUpstream(nodeBlock, merge)
            }
          case _ => // do nothing
        }

        Util.replace(nextState, statePhi)
        Seq(merge)
      case (values, states) => // multiple returns
        val triplets = values.zip(states).map { case ((v, j), (s, _)) => (v, j, s) }

        log.pprint(triplets)
        triplets.foreach { case (v, j, s) =>
          v.downstreamRemoveAll(j)
          s.downstreamRemoveAll(j)
        }
        val valPhi = new SSA.Phi(
          null,
          triplets.map { case (v, j, s) => (j.block, v) }.toSet,
          classManager.mergeTypes(triplets.map { case (v, j, s) => v.jtype }).asInstanceOf[JType]
        )

        val statePhi = new SSA.Phi(
          null,
          triplets.map { case (v, j, s) => (j.block, s) }.toSet,
          JType.Prim.V
        )

        triplets.foreach { case (v, j, s) => j.block.nextPhis ++= Seq(statePhi, valPhi) }
        val merge = new SSA.Merge(
          triplets.map(_._2.block).toSet,
          next = nodeBlock.next,
          phis = Seq(valPhi, statePhi)
        )

        triplets.foreach { case (v, j, s) =>
          j.block.next = merge
        }
        valPhi.block = merge
        statePhi.block = merge

        nodeBlock.next.replaceUpstream(nodeBlock, merge)
        nodeBlock.next match {
          case m: SSA.Merge =>
            m.phis.foreach { phi =>
              nodeBlock.nextPhis = nodeBlock.nextPhis.filter(_ != phi)
              merge.nextPhis = Seq(phi) ++ merge.nextPhis
              phi.replaceUpstream(nodeBlock, merge)
            }
          case _ => // do nothing
        }

        Util.replace(nextState, statePhi)
        Util.replace(node, valPhi)
        Seq(merge)
    }


    calledStartBlock.startingState.downstreamList.foreach { d =>
      d.replaceUpstream(calledStartBlock.startingState, node.state)
      node.state.downstreamAdd(d)
    }
    log.pprint(calledStartBlock)
    log.pprint(nodeBlock)
    calledStartBlock.downstreamList.foreach { n =>
      log.pprint(n)
      n.replaceUpstream(calledStartBlock, nodeBlock)
    }

    if (!calledBody.allTerminals.contains(calledStartBlock.next)){

      nodeBlock.next = calledStartBlock.next
      nodeBlock.nextPhis = calledStartBlock.nextPhis

      nodeBlock.blockInvokes = calledStartBlock.blockInvokes ++ nodeBlock.blockInvokes.filter(_ != node)
    }else {
      nodeBlock.blockInvokes = nodeBlock.blockInvokes.filter(_ != node)
    }

    val newLiveBlocks =
      visitedMethods(edge.caller).liveBlocks ++
        visitedMethods(edge.called).liveBlocks ++
        addedBlocks

    visitedMethods(edge.caller) = visitedMethods(edge.caller).copy(
      inferred = visitedMethods(edge.caller).inferred ++ visitedMethods(edge.called).inferred,
      liveBlocks = newLiveBlocks
    )

    visitedMethods.remove(edge.called)
    log.graph("INLINED " + edge) {
      Renderer.dumpSvg(visitedMethods(edge.caller).methodBody)
    }

    visitedMethods(edge.caller).methodBody.allTerminals ++= throws
    visitedMethods(edge.caller).methodBody.removeDeadNodes()
    visitedMethods(edge.caller).methodBody.checkLinks()
    visitedMethods.remove(edge.called)
  }

  def inlineWireParams(log: Logger.Global, node: SSA.Invoke, calledBody: MethodBody) = {
    for ((arg, src) <- calledBody.args.zip(node.srcs)) {
      src.downstreamRemove(node)
      Util.replace(arg, src, log)
    }
    node.state.downstreamRemove(node)
  }
}
