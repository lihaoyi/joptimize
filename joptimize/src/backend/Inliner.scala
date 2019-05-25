package joptimize.backend

import joptimize.{Logger, Util}
import joptimize.analyzer.ProgramAnalyzer.{CallEdge, MethodResult}
import joptimize.analyzer.{ProgramAnalyzer, Renderer}
import joptimize.frontend.ClassManager
import joptimize.model.{IType, InferredSig, JType, MethodBody, SSA}

import scala.collection.mutable

object Inliner {
  def inlineAll(
    analyzerRes: ProgramAnalyzer.ProgramResult,
    classManager: ClassManager.Frozen,
    log: Logger.Global
  ) = log.block {

    val filteredCallgraph = analyzerRes
      .callGraph
      .filter(e => e.called.method.name != "<init>" && e.called.method.name != "<clinit>")

    val singleCallerEdges = filteredCallgraph
      .groupBy(_.called)
      .values
      .collect { case Seq(singleEdge) => singleEdge }

    val singleCalledEdges = filteredCallgraph
      .filter(_.node.nonEmpty)
      .groupBy(_.node.get)
      .values
      .collect { case Seq(singleEdge) => singleEdge }

    val callGraphLookup = filteredCallgraph
      .groupBy(_.called)
      .collect { case (src, Seq(dest)) => (src, dest.caller) }

    val inlineableEdgesSet = singleCallerEdges
      .to[mutable.LinkedHashSet]
      .intersect(singleCalledEdges.to[mutable.LinkedHashSet])
      .filter(edge => edge.caller != edge.called)

    log.pprint(inlineableEdgesSet.map(_.toString))

    val inlinedMethodsSet = inlineableEdgesSet.map(_.called)
    val visitedMethods = mutable.LinkedHashMap.empty[InferredSig, Option[MethodResult]]
    for ((k, v) <- analyzerRes.visitedMethods) {
      visitedMethods.put(k, v)
    }

    for {
      edge <- inlineableEdgesSet
      node <- edge.node
    } {
      var caller = edge.caller
      //      pprint.log(analyzerRes.visitedMethods.keys)
      //      pprint.log(edge)
      while ({
        if (visitedMethods.contains(caller)) {
          inlineSingleMethod(
            classManager,
            log.inferredMethod(caller),
            visitedMethods,
            edge.copy(caller = caller),
            node
          )
          false
        } else {
          caller = callGraphLookup(caller)
          true
        }
      }) ()

    }

    ProgramAnalyzer.ProgramResult(
      visitedMethods,
      analyzerRes.visitedResolved.filter { case (k, v) => !inlinedMethodsSet.contains(k) },
      analyzerRes.staticFieldReferencedClasses,
      analyzerRes.callGraph.filter(!inlineableEdgesSet(_))
    )
  }

  def inlineSingleMethod(
    classManager: ClassManager.Frozen,
    log: Logger.InferredMethod,
    visitedMethods: mutable.LinkedHashMap[InferredSig, Option[MethodResult]],
    edge: CallEdge,
    node: SSA.Invoke
  ) = Util.labelExceptions(edge.toString) {
    log.pprint(node)
    log.pprint(edge)
    log.graph("edge.caller") {
      Renderer.dumpSvg(visitedMethods(edge.caller).get.methodBody)
    }
    log.graph("edge.called") {
      Renderer.dumpSvg(visitedMethods(edge.called).get.methodBody)
    }
    def findBlock(current: SSA.Node): SSA.Block = {

      current match {
        case b: SSA.Block => b
        case s: SSA.State => findBlock(s.parent)
        case s: SSA.Stateful => findBlock(s.state)
      }
    }
    val nodeBlock = findBlock(node)
    val calledMethod = visitedMethods(edge.called).get

    val calledBody = calledMethod.methodBody
    val calledStartBlock =
      calledMethod.methodBody.getAllVertices().collect { case s: SSA.Start => s }.head

    // Wire up input params, state and block
    calledStartBlock.downstreamList.foreach(_.replaceUpstream(calledStartBlock, nodeBlock))

    for ((arg, src) <- calledBody.args.zip(node.srcs)) {
      src.downstreamRemove(node)
      Util.replace(arg, src)
    }

    node.state.next = calledStartBlock.nextState
    calledStartBlock.nextState.parent = node.state

    log.graph("INPUT PARAMS STATE BLOCK WIRED") {
      Renderer.dumpSvg(visitedMethods(edge.caller).get.methodBody)
    }
    // Collect returns
    val returns = mutable.Buffer.empty[(SSA.State, SSA.Jump, Option[SSA.Val])]
    val throws = mutable.Buffer.empty[SSA.AThrow]
    calledBody.allTerminals.foreach {
      case r: SSA.ReturnVal => returns.append((r.state, r, Some(r.src)))
      case r: SSA.Return => returns.append((r.state, r, None))
      case a: SSA.AThrow => throws.append(a)
    }

    val Seq(nextState) = node.downstreamList.collect { case s: SSA.State => s }

    node.downstreamRemove(nextState)

    val outputNodes = prepareOutputNodes(
      classManager,
      log,
      nodeBlock,
      returns,
      nextState,
      visitedMethods(edge.called).get.inferred ++ visitedMethods(edge.caller).get.inferred
    )
    val (addedBlocks, inlinedFinalValOpt, inlinedFinalStateOpt) = outputNodes match {
      case Some((inlinedLastBlock, inlinedFinalValOpt, inlinedFinalState)) =>
        // Wire up return value, state and block
        nextState.parent = inlinedFinalState
        inlinedFinalValOpt.foreach(t => Util.replace(node, t._1))
        nextState.parent = inlinedFinalState
        inlinedFinalState.next = nextState

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
        (Seq(inlinedLastBlock), inlinedFinalValOpt, Some(inlinedFinalState))
      case None =>
        Util.replace(node, new SSA.ConstNull())
        val throwBlock = new SSA.ThrowBlock(nodeBlock.next)
        nextState.parent = throwBlock
        throwBlock.nextState = nextState
        nodeBlock.next.replaceUpstream(nodeBlock, throwBlock)
        nodeBlock.next match {
          case b: SSA.Merge =>
            b.phis.foreach(_.replaceUpstream(nodeBlock, throwBlock))
            throwBlock.nextPhis ++= b.phis
          case _ => //do nothing
        }

        (Nil, None, None)
    }
    log.graph("RETURNS WIRED") {
      Renderer.dumpSvg(visitedMethods(edge.caller).get.methodBody)
    }

    // Wire up input block
    if (outputNodes.isEmpty || !calledBody.allTerminals.contains(calledStartBlock.next)) {
      // If the inlined method has internal control flow, the input block and output
      // blocks are different and we have to wire them up properly
      nodeBlock.next = calledStartBlock.next
      nodeBlock.nextPhis = calledStartBlock.nextPhis
    }

    // Consolidate two method bodies in `visitedMethods`
    val newInferred = mutable.LinkedHashMap.empty[SSA.Val, IType]
    visitedMethods(edge.caller).get.inferred.foreach(t => newInferred.put(t._1, t._2))
    visitedMethods(edge.called).get.inferred.foreach(t => newInferred.put(t._1, t._2))
    inlinedFinalValOpt.foreach(t => newInferred.put(t._1, t._2))

    visitedMethods(edge.caller) = Some(
      visitedMethods(edge.caller)
        .get
        .copy(
          inferred = newInferred,
          liveBlocks =
            visitedMethods(edge.caller).get.liveBlocks ++
            visitedMethods(edge.called).get.liveBlocks ++
            addedBlocks,
          liveTerminals =
            visitedMethods(edge.caller).get.liveTerminals ++
            visitedMethods(edge.called).get.liveTerminals.filter(_.isInstanceOf[SSA.AThrow])
        )
    )

    visitedMethods.remove(edge.called)

    visitedMethods(edge.caller).get.methodBody.allTerminals =
      visitedMethods(edge.caller).get.methodBody.allTerminals ++
      throws
    visitedMethods(edge.caller).get.methodBody.removeDeadNodes()
    log.graph("INLINED " + edge) {
      Renderer.dumpSvg(visitedMethods(edge.caller).get.methodBody)
    }

    log.check(visitedMethods(edge.caller).get.methodBody.checkLinks())
    visitedMethods.remove(edge.called)
  }

  def prepareOutputNodes(
    classManager: ClassManager.Frozen,
    log: Logger.InferredMethod,
    nodeBlock: SSA.Block,
    returns: mutable.Buffer[(SSA.State, SSA.Jump, Option[SSA.Val])],
    nextState: SSA.State,
    inferred: mutable.LinkedHashMap[SSA.Val, IType]
  ) = {
    log.pprint(returns)
    returns match {
      case Seq() =>
        None
      case Seq((returnedState, returnNode, returnedValOpt)) =>
//        returnedState.downstreamRemoveAll(returnNode)
        returnedValOpt.foreach(_.downstreamRemoveAll(returnNode))

        Some(
          (
            returnNode.block,
            returnedValOpt.flatMap(v => inferred.get(v).map((v, _))),
            returnedState
          )
        )

      case triplets => // multiple returns
        val valPhiOpt =
          if (!returns.forall(_._3.isDefined)) None
          else {
            val phi = new SSA.Phi(
              null,
              mutable.LinkedHashMap.empty,
              classManager
                .mergeTypes(
                  triplets.collect { case (s, j, v) if inferred.contains(v.get) => v.get.jtype }
                )
                .get
                .asInstanceOf[JType]
            )
            triplets.map { case (s, j, v) => phi.incoming.put(j.block, v.get) }
            val inferredMerged = classManager
              .mergeTypes(
                triplets.flatMap { case (s, j, v) => inferred.get(v.get) }
              )
              .get
            Some((phi, inferredMerged))
          }

        val merge = new SSA.Merge(
          mutable.LinkedHashMap.empty,
          next = nodeBlock.next,
          phis = valPhiOpt.map(_._1).toSeq
        )

        val returnedState = new SSA.State(merge)

        for ((s, j, v) <- triplets) {
          merge.incoming.put(j.block, s)
          s.next = merge
          valPhiOpt.foreach(t => v.foreach(_.downstreamAdd(t._1)))
        }

        triplets.foreach {
          case (s, j, v) =>
            j.block.next = merge
            j.block.nextPhis ++= valPhiOpt.map(_._1)
            v.foreach(_.downstreamRemoveAll(j))
        }

        valPhiOpt.foreach(_._1.block = merge)

        Some(
          (
            merge,
            valPhiOpt,
            returnedState
          )
        )
    }
  }
}
