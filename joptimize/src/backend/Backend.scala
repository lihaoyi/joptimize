package joptimize.backend

import joptimize.{FileLogger, Logger, Util}
import joptimize.algorithms.{Dominator, MultiBiMap, Scheduler}
import joptimize.analyzer.ProgramAnalyzer.{CallEdge, MethodResult}
import joptimize.analyzer.{Namer, ProgramAnalyzer, Renderer}
import joptimize.frontend.ClassManager
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{ClassNode, InsnList, MethodNode}

import collection.JavaConverters._
import scala.collection.mutable

object Backend {
  def apply(analyzerRes: ProgramAnalyzer.ProgramResult,
            entrypoints: scala.Seq[MethodSig],
            classManager: ClassManager.ReadOnly,
            eliminateOldMethods: Boolean,
            log: Logger.Global): Seq[ClassNode] = log.block{


    val loadMethodCache = classManager.loadMethodCache.collect{case (k, Some(v)) => (k, v)}.toMap
    val loadClassCache = classManager.loadClassCache.collect{case (k, Some(v)) => (k, v)}.toMap
    //    pprint.log(loadMethodCache.keys)


    val inlinedAnalyzerRes = inlineAll(analyzerRes, classManager, log)
//    val inlinedAnalyzerRes = analyzerRes

    val combined =
      inlinedAnalyzerRes.visitedResolved.mapValues(Right(_)) ++
      inlinedAnalyzerRes.visitedMethods.mapValues(Left(_))

    val highestMethodDefiners = computeHighestMethodDefiners(
      inlinedAnalyzerRes,
      log,
      loadMethodCache,
      loadClassCache,
      combined
    )

    val newMethods = generateNewMethods(
      inlinedAnalyzerRes,
      entrypoints,
      log,
      loadMethodCache,
      loadClassCache,
      combined,
      highestMethodDefiners,
      classManager
    )


    if (eliminateOldMethods) {
      for ((k, cn) <- loadClassCache) {
        cn.methods.clear()
      }
    }

    val visitedInterfaces =
      Util.findSeenInterfaces(loadClassCache, newMethods.map(_._1)) ++
      Seq("scala/runtime/Nothing$")

    log.pprint(visitedInterfaces)
    val grouped =
      (visitedInterfaces ++ inlinedAnalyzerRes.staticFieldReferencedClasses.map(_.name))
        .filter(s => loadClassCache.contains(JType.Cls(s)))
        .map(loadClassCache(_) -> Nil)
        .toMap ++
      newMethods.groupBy(_._1).mapValues(_.map(_._2))

    for((cn, mns) <- grouped) yield {
      log.pprint(cn.name)
      log.pprint(mns.map(_.name))
      if (cn.attrs != null) Util.removeFromJavaList(cn.attrs)(_.`type` == "ScalaSig")
      if (cn.attrs != null) Util.removeFromJavaList(cn.attrs)(_.`type` == "ScalaInlineInfo")
      if (cn.visibleAnnotations != null) {
        Util.removeFromJavaList(cn.visibleAnnotations )(_.desc == "Lscala/reflect/ScalaSignature;")
      }

      cn.methods.addAll(mns.asJava)
    }
    def ignore(s: String) = s.startsWith("java/")

    val outClasses = log.block{
      BytecodeDCE.apply(
        entrypoints,
        grouped.keys.toSeq,
        resolvePossibleSigs = classManager.resolvePossibleSigs(_).toSeq.flatten,
        getLinearSuperclasses = classManager.getLinearSuperclasses,
        ignore = ignore,
        log = log
      )
    }
    outClasses
//    grouped.keys.toSeq
  }

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
        if (analyzerRes.visitedMethods.contains(caller)) {
          inlineSingleMethod(
            analyzerRes,
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

  def inlineSingleMethod(analyzerRes: ProgramAnalyzer.ProgramResult,
                         classManager: ClassManager.ReadOnly,
                         log: Logger.Global,
                         visitedMethods: mutable.LinkedHashMap[InferredSig, MethodResult],
                         edge: CallEdge,
                         node: SSA.Invoke) = {
    val nodeBlock = node.block.get
    val calledMethod = analyzerRes.visitedMethods(edge.called)
    log.pprint(node)
    log.pprint(edge)
    log.graph("edge.caller") {
      Renderer.dumpSvg(analyzerRes.visitedMethods(edge.caller).methodBody)
    }
    log.graph("edge.called") {
      Renderer.dumpSvg(analyzerRes.visitedMethods(edge.called).methodBody)
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

    visitedMethods(edge.caller) = visitedMethods(edge.caller).copy(
      inferred = visitedMethods(edge.caller).inferred ++ visitedMethods(edge.called).inferred,
      liveBlocks =
        visitedMethods(edge.caller).liveBlocks ++
        visitedMethods(edge.called).liveBlocks ++
        addedBlocks
    )
    visitedMethods.remove(edge.called)
    log.graph("INLINED " + edge) {
      Renderer.dumpSvg(analyzerRes.visitedMethods(edge.caller).methodBody)
    }
    analyzerRes.visitedMethods(edge.caller).methodBody.allTerminals ++= throws
    analyzerRes.visitedMethods(edge.caller).methodBody.removeDeadNodes()
    analyzerRes.visitedMethods(edge.caller).methodBody.checkLinks()
    analyzerRes.visitedMethods.remove(edge.called)
  }

  def inlineWireParams(log: Logger.Global, node: SSA.Invoke, calledBody: MethodBody) = {
    for ((arg, src) <- calledBody.args.zip(node.srcs)) {
      src.downstreamRemove(node)
      Util.replace(arg, src, log)
    }
    node.state.downstreamRemove(node)
  }

  def generateNewMethods(analyzerRes: ProgramAnalyzer.ProgramResult,
                         entrypoints: Seq[MethodSig],
                         log: Logger.Global,
                         loadMethodCache: Map[MethodSig, MethodNode],
                         loadClassCache: Map[JType.Cls, ClassNode],
                         combined: collection.Map[InferredSig, Either[ProgramAnalyzer.MethodResult, ProgramAnalyzer.Properties]],
                         highestMethodDefiners: collection.Map[InferredSig, JType.Cls],
                         classManager: ClassManager.ReadOnly) = {
    log.block {
      for {
        (isig, result) <- combined.toList
        if loadClassCache.contains(isig.method.cls)
        if loadMethodCache.contains(isig.method)
      } yield {
        val liveArgs =
          if (entrypoints.contains(isig.method)) (_: Int) => true
          else {
            val highestSig =
              if (isig.method.static) isig.method
              else isig.method.copy(cls = highestMethodDefiners(isig))

            analyzerRes.visitedResolved(isig.copy(method = highestSig)).liveArgs
          }

        val originalNode = loadMethodCache(isig.method)

        val props = result match {
          case Left(res) => res.props
          case Right(props) => props
        }
        val (mangledName, mangledDesc) =
          if (entrypoints.contains(isig.method) || isig.method.name == "<init>") (isig.method.name, isig.method.desc)
          else Util.mangle(isig, props.inferredReturn, liveArgs)

        val newNode = new MethodNode(
          Opcodes.ASM6,
          originalNode.access,
          mangledName,
          mangledDesc.unparse,
          originalNode.signature,
          originalNode.exceptions.asScala.toArray
        )
        originalNode.accept(newNode)
        result match {
          case Right(props) => newNode.instructions = new InsnList()
          case Left(res) =>
            val argMapping = Util.argMapping(isig.method, liveArgs)

            newNode.instructions = processMethodBody(
              isig.method,
              res,
              log.inferredMethod(isig),
              loadClassCache.contains,
              (invokedSig, invokeSpecial) => {

                if (invokeSpecial) {
                  analyzerRes.visitedMethods.get(invokedSig) match {
                    case Some(res) => res.props
                    case None => ProgramAnalyzer.dummyProps(invokedSig.method, optimistic = false)
                  }
                } else {

                  val highestSig =
                    if (invokedSig.method.static || !loadMethodCache.contains(invokedSig.method)) invokedSig.method
                    else invokedSig.method.copy(cls = highestMethodDefiners(invokedSig))


                  val res = analyzerRes.visitedResolved.getOrElse(
                    invokedSig.copy(method = highestSig),
                    ProgramAnalyzer.dummyProps(highestSig, optimistic = false)
                  )

                  res
                }
              },
              argMapping,
              cls => classManager.loadClassCache(cls).map(c => (c.access & Opcodes.ACC_INTERFACE) != 0)
            )
        }
        newNode.desc = mangledDesc.unparse
        newNode.tryCatchBlocks = Nil.asJava

        loadClassCache(isig.method.cls) -> newNode
      }
    }
  }

  def computeHighestMethodDefiners(analyzerRes: ProgramAnalyzer.ProgramResult,
                                   log: Logger.Global,
                                   loadMethodCache: Map[MethodSig, MethodNode],
                                   loadClassCache: Map[JType.Cls, ClassNode],
                                   combined: collection.Map[InferredSig, Either[ProgramAnalyzer.MethodResult, ProgramAnalyzer.Properties]]) = {
    log.block {
      for {
        (isig, _) <- combined
        if loadMethodCache.contains(isig.method)
      } yield {
        var parentClasses = List.empty[JType.Cls]
        var current = isig.method.cls
        while ( {
          if (!loadClassCache.contains(current)) false
          else {
            parentClasses = current :: parentClasses
            loadClassCache(current).superName match {
              case null => false
              case name =>
                current = JType.Cls(name)
                true
            }
          }
        }) ()

        val highestCls = parentClasses
          .find { cls =>

            val key = isig.copy(method = isig.method.copy(cls = cls))
            val res = analyzerRes.visitedResolved.contains(key)
            res
          }
          .get

        (isig, highestCls)
      }
    }
  }

  def processMethodBody(originalSig: MethodSig,
                        result: ProgramAnalyzer.MethodResult,
                        log: Logger.InferredMethod,
                        classExists: JType.Cls => Boolean,
                        resolvedProperties: (InferredSig, Boolean) => ProgramAnalyzer.Properties,
                        argMapping: Map[Int, Int],
                        isInterface: JType.Cls => Option[Boolean]) = log.block{

    // Strip out the SSA.Invoke#block edges from the method body before proceeding with
    // simplification and code generation.
    //
    // These edges are necessary for simplifying the inlining step done earlier, but once
    // inlining is done, they are unnecessary. They also seem to cause problems with downstream
    // code generations for unknown reasons
    result.methodBody.getAllVertices().foreach{
      case i: SSA.Block => i.blockInvokes = Nil
      case i: SSA.InvokeStatic => i.block = None
      case i: SSA.InvokeVirtual=> i.block = None
      case i: SSA.InvokeSpecial => i.block = None
      case _ =>
    }

    log.pprint(argMapping)

    OptimisticSimplify.apply(
      originalSig.static,
      argMapping,
      result.methodBody,
      result.inferred,
      result.liveBlocks,
      log,
      classExists,
      resolvedProperties
    )

    log.check(result.methodBody.checkLinks(checkDead = false))
    result.methodBody.removeDeadNodes()
    log.graph("POST OPTIMISTIC SIMPLIFY")(Renderer.dumpSvg(result.methodBody))
    log.check(result.methodBody.checkLinks())

    val allVertices2 = result.methodBody.getAllVertices()

//    pprint.log(originalSig)
//    pprint.log(result.program)
    val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
      analyzeBlockStructure(result.methodBody)
    val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

    val dominators2 = Dominator.findDominators(blockEdges, allBlocks)

    RegisterAllocator.apply(result.methodBody, dominators2.immediateDominators, log)

    val nodesToBlocks = Scheduler.apply(
      loopTree2, dominators2, startBlock,
      allVertices2, log
    )

    val postRegisterAllocNaming = Namer.apply(
      result.methodBody,
      nodesToBlocks,
      result.methodBody.getAllVertices(),
      log
    )

    val (blockCode, finalInsns) = CodeGenMethod(
      result.methodBody,
      allVertices2,
      nodesToBlocks,
      analyzeBlockStructure(result.methodBody)._1,
      postRegisterAllocNaming,
      log,
      isInterface
    )

    log.println("================ OUTPUT BYTECODE ================")
    log(Renderer.renderBlockCode(blockCode, finalInsns))
    finalInsns
  }

  def analyzeBlockStructure(methodBody: MethodBody) = {
    val controlFlowEdges = Renderer.findControlFlowGraph(methodBody)
    val startBlock = (controlFlowEdges.map(_._1).toSet -- controlFlowEdges.map(_._2)).head.asInstanceOf[SSA.Block]
    val allBlocks = controlFlowEdges
      .flatMap { case (k, v) => Seq(k, v) }
      .collect { case b: SSA.Block => b }

    val blockEdges = controlFlowEdges.flatMap {
      case (k: SSA.Block, v: SSA.Jump) => Nil
      case (k: SSA.Jump, v: SSA.Block) => Seq(k.block -> v)
      case (k: SSA.Block, v: SSA.Block) => Seq(k -> v)
    }

    (controlFlowEdges, startBlock, allBlocks, blockEdges)
  }
}
