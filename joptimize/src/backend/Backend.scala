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


    val inlinedAnalyzerRes = inline(analyzerRes, log)

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

  def inline(analyzerRes: ProgramAnalyzer.ProgramResult, log: Logger.Global) = log.block{
    val inlineableEdges = analyzerRes
      .callGraph
      .filter(e => e.called.method.name != "<init>" && e.called.method.name != "<clinit>")
      .groupBy(_.called)
      .values
      .collect { case Seq(singleEdge) => singleEdge }

    val inlineableEdgeSet = inlineableEdges.toSet
    log.pprint(inlineableEdges.map(_.toString))
    val inlinedMethodsSet = inlineableEdges.map(_.called).toSet
    val visitedMethods = mutable.LinkedHashMap.empty[InferredSig, MethodResult]
    for((k, v) <- analyzerRes.visitedMethods){
      visitedMethods.put(k, v)
    }


    for {
      edge <- inlineableEdges
      node <- edge.node
    } {
      val calledMethod = analyzerRes.visitedMethods(edge.called)
      log.graph("edge.caller") {
        Renderer.dumpSvg(analyzerRes.visitedMethods(edge.caller).methodBody)
      }
      log.graph("edge.called") {
        Renderer.dumpSvg(analyzerRes.visitedMethods(edge.called).methodBody)
      }
      val calledBody = calledMethod.methodBody

      for ((arg, src) <- calledBody.args.zip(node.srcs)) Util.replace(arg, src)

      val returnedVals = mutable.Buffer.empty[(SSA.Val, SSA.Jump)]
      val returnedStates = mutable.Buffer.empty[(SSA.State, SSA.Jump)]
      calledBody.allTerminals.foreach {
        case r: SSA.ReturnVal =>
          returnedVals.append(r.src -> r)
          returnedStates.append(r.state -> r)
        case r: SSA.Return =>
          returnedStates.append(r.state -> r)
      }

      val Seq(nextState) = node.downstreamList.collect { case s: SSA.State => s }

      (returnedVals, returnedStates) match {
        case (Seq(), Seq((rState, ret))) =>
          Util.replace(nextState, rState)
          rState.downstreamRemoveAll(ret)
        case (Seq((rVal, ret0)), Seq((rState, ret1))) =>
          Util.replace(nextState, rState)
          rState.downstreamRemoveAll(ret1)
          Util.replace(node, rVal)
          rVal.downstreamRemoveAll(ret0)
        case _ =>
      }
      val calledStartBlock = calledMethod.liveBlocks.find(_.upstream.isEmpty).get
      calledStartBlock.downstreamList.foreach {
        case s: SSA.ChangedState =>
          Util.replace(s, node.state)
//          s.replaceUpstream(calledStartBlock, node.state)
        case _ => //do nothing
      }

      visitedMethods(edge.caller) = visitedMethods(edge.caller).copy(inferred = visitedMethods(edge.caller).inferred ++ visitedMethods(edge.called).inferred)
      visitedMethods.remove(edge.called)
      log.graph("INLINED " + edge) {
        Renderer.dumpSvg(analyzerRes.visitedMethods(edge.caller).methodBody)
      }
    }

    ProgramAnalyzer.ProgramResult(
      visitedMethods,
      analyzerRes.visitedResolved.filterKeys(!inlinedMethodsSet.contains(_)),
      analyzerRes.staticFieldReferencedClasses,
      analyzerRes.callGraph.filter(!inlineableEdgeSet(_))
    )
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
