package joptimize.backend

import joptimize.{FileLogger, Logger, Util}
import joptimize.algorithms.{Dominator, MultiBiMap, Scheduler}
import joptimize.analyzer.ProgramAnalyzer.{CallEdge, MethodResult}
import joptimize.analyzer.{ProgramAnalyzer, Renderer}
import joptimize.frontend.{ClassManager, Frontend}
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
            log: Logger.Global,
            inline: Boolean): Seq[ClassNode] = log.block{

    val loadMethodCache = classManager.loadMethodCache.collect{case (k, Some(v)) => (k, v)}.toMap
    val loadClassCache = classManager.loadClassCache.collect{case (k, Some(v)) => (k, v)}.toMap

    val inlinedAnalyzerRes =
      if(inline) Inliner.inlineAll(analyzerRes, classManager, log)
      else analyzerRes

    val actualMethodInferredSigs = inlinedAnalyzerRes.visitedResolved.keysIterator
      .++(inlinedAnalyzerRes.visitedMethods.keysIterator)
      .filter(k => loadMethodCache.contains(k.method))
      .to[mutable.LinkedHashSet]

    val highestMethodDefiners = computeHighestMethodDefiners(
      inlinedAnalyzerRes,
      log,
      loadMethodCache,
      loadClassCache,
      actualMethodInferredSigs
    )

    val newMethods = generateNewMethods(
      inlinedAnalyzerRes,
      entrypoints,
      log,
      loadMethodCache,
      loadClassCache,
      actualMethodInferredSigs,
      highestMethodDefiners.toMap,
      classManager
    )


    if (eliminateOldMethods) {
      for ((k, cn) <- loadClassCache) {
        cn.methods.clear()
      }
    }

    val visitedInterfaces =
      Util.findSeenInterfaces(loadClassCache, newMethods.map(_._1)) ++
      Seq("scala/runtime/Nothing$", "scala/runtime/Null$")

    log.pprint(visitedInterfaces)
    val lhs = (visitedInterfaces ++ inlinedAnalyzerRes.staticFieldReferencedClasses.flatMap(classManager.getAllSupertypes).map(_.name))
      .filter(s => loadClassCache.contains(JType.Cls(s)))
      .map(loadClassCache(_) -> Nil)
      .toMap
    val grouped: Map[ClassNode, Seq[MethodNode]] =
      lhs ++
      newMethods.groupBy(_._1).mapValues(_.toSeq.map(_._2))

    for((cn, mns) <- grouped) yield {
      log.pprint(cn.name)
      log.pprint(mns.map(_.name))
      if (cn.attrs != null) Util.removeFromJavaList(cn.attrs)(_.`type` == "ScalaSig")
      if (cn.attrs != null) Util.removeFromJavaList(cn.attrs)(_.`type` == "ScalaInlineInfo")
      if (cn.visibleAnnotations != null) {
        Util.removeFromJavaList(cn.visibleAnnotations )(_.desc == "Lscala/reflect/ScalaSignature;")
      }

      mns.foreach(cn.methods.add)
    }
    def ignore(s: String) = s.startsWith("java/")

    val outClasses = log.block{
      BytecodeDCE.apply(
        entrypoints,
        grouped.keys.toSeq,
        resolvePossibleSigs = classManager.resolvePossibleSigs(_).toSeq.flatten,
        getLinearSuperclasses = classManager.getLinearSuperclasses,
        getAllSupertypes = classManager.getAllSupertypes,
        ignore = ignore,
        log = log
      )
    }
    outClasses
//    grouped.keys.toSeq
  }



  def generateNewMethods(analyzerRes: ProgramAnalyzer.ProgramResult,
                         entrypoints: Seq[MethodSig],
                         log: Logger.Global,
                         loadMethodCache: Map[MethodSig, MethodNode],
                         loadClassCache: Map[JType.Cls, ClassNode],
                         actualMethodInferredSigs: mutable.LinkedHashSet[InferredSig],
                         highestMethodDefiners: collection.Map[InferredSig, JType.Cls],
                         classManager: ClassManager.ReadOnly) = log.block {

    def resolveDefsiteProps(isig: InferredSig) = {
      val resolvedSig =
        if (isig.method.static) isig
        else isig.copy(method = isig.method.copy(cls = highestMethodDefiners(isig)))

      analyzerRes.visitedResolved(resolvedSig)
    }

    def resolveCallsiteProps(isig: InferredSig, invokeSpecial: Boolean) = {
      if (!loadClassCache.contains(isig.method.cls)) ProgramAnalyzer.dummyProps(isig.method, optimistic = false)
      else if (classManager.resolvePossibleSigs(isig.method).exists(!_.exists(loadMethodCache.contains))) {
        ProgramAnalyzer.dummyProps(isig.method, optimistic = false)
      }
      else if (invokeSpecial) analyzerRes.visitedMethods(isig).props
      else resolveDefsiteProps(isig)
    }

    for (isig <- actualMethodInferredSigs.toArray) yield Util.labelExceptions(isig.toString){
      val props = resolveDefsiteProps(isig)

      val liveArgs =
        if (entrypoints.contains(isig.method)) (_: Int) => true
        else props.liveArgs

      val originalNode = loadMethodCache(isig.method)

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
      if (!analyzerRes.visitedMethods.contains(isig)) newNode.instructions = new InsnList()
      else {
        val argMapping = Util.argMapping(isig.method, liveArgs)

        newNode.instructions = processMethodBody(
          isig.method,
          analyzerRes.visitedMethods(isig),
          log.inferredMethod(isig),
          loadClassCache.contains,
          resolveCallsiteProps,
          argMapping,
          cls => classManager.loadClassCache.get(cls).flatten.map(c => (c.access & Opcodes.ACC_INTERFACE) != 0)
        )
      }
      newNode.desc = mangledDesc.unparse
      newNode.tryCatchBlocks = Nil.asJava

      loadClassCache(isig.method.cls) -> newNode
    }
  }

  def computeHighestMethodDefiners(analyzerRes: ProgramAnalyzer.ProgramResult,
                                   log: Logger.Global,
                                   loadMethodCache: Map[MethodSig, MethodNode],
                                   loadClassCache: Map[JType.Cls, ClassNode],
                                   actualMethodInferredSigs: mutable.LinkedHashSet[InferredSig]) = log.block {
    for (isig <- actualMethodInferredSigs ++ analyzerRes.visitedResolved.keys) yield {
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
          analyzerRes.visitedResolved.contains(key) || analyzerRes.visitedMethods.contains(key)
        }
        .get

      (isig, highestCls)
    }
  }

  def processMethodBody(originalSig: MethodSig,
                        result: ProgramAnalyzer.MethodResult,
                        log: Logger.InferredMethod,
                        classExists: JType.Cls => Boolean,
                        resolvedProperties: (InferredSig, Boolean) => ProgramAnalyzer.Properties,
                        argMapping: Map[Int, Int],
                        isInterface: JType.Cls => Option[Boolean]) = log.block{

    log.check(result.methodBody.checkLinks())
    log.graph("PROCESSMETHODBODY")(Renderer.dumpSvg(result.methodBody))

//    result.methodBody.removeDeadNodes()
//    log.global().graph("ZZZ")(Renderer.dumpSvg(result.methodBody))


    log.pprint(argMapping)
    log.pprint(result.liveTerminals)
    log.pprint(result.liveBlocks)

    val liveBlocks = {
      val (_, startBlock, _, blockEdges) = analyzeBlockStructure(result.methodBody)
      val blockEdgeMap = blockEdges.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
      Util.breadthFirstSeen(Set(startBlock: SSA.Block))(blockEdgeMap.getOrElse(_, Nil))
    }

    log.pprint(liveBlocks)
    log.pprint(result.liveBlocks)

    log.graph("PRE OPTIMISTIC SIMPLIFY")(Renderer.dumpSvg(result.methodBody))
    OptimisticSimplify.apply(
      originalSig.static,
      argMapping,
      result.methodBody,
      result.inferred,
      result.liveBlocks.intersect(liveBlocks),
      log,
      classExists,
      resolvedProperties
    )
    log.graph("POST OPTIMISTIC SIMPLIFY")(Renderer.dumpSvg(result.methodBody))
    log.check(result.methodBody.checkLinks(checkDead = false))

    Frontend.simplifyPhiMerges(result.methodBody)

    result.methodBody.removeDeadNodes()

    log.graph("POST OPTIMISTIC SIMPLIFY CLEANUP")(Renderer.dumpSvg(result.methodBody))

    log.check(result.methodBody.checkLinks())

    val allVertices2 = result.methodBody.getAllVertices()

//    pprint.log(originalSig)
//    pprint.log(result.program)
    val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
      analyzeBlockStructure(result.methodBody)

    val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

    val dominators2 = Dominator.findDominators(blockEdges, allBlocks)

//    RegisterAllocator.apply(result.methodBody, dominators2.immediateDominators, log)
//
//    log.graph("COPY INSERTED")(Renderer.dumpSvg(result.methodBody))

    val nodesToBlocks = Scheduler.apply(
      loopTree2, dominators2, startBlock,
      allVertices2, log
    )
//
//    val postRegisterAllocNaming = Namer.apply(
//      result.methodBody,
//      nodesToBlocks,
//      result.methodBody.getAllVertices(),
//      log
//    )

    val finalInsns = new CodeGenMethod(
//      postRegisterAllocNaming,
      log,
      isInterface,
      result.methodBody,
      nodesToBlocks,
      analyzeBlockStructure(result.methodBody)._1
    ).apply()


    finalInsns
  }

  def analyzeBlockStructure(methodBody: MethodBody) = {
    val controlFlowEdges = Renderer.findControlFlowGraph(methodBody)
    val startBlock = controlFlowEdges.collect{case (s: SSA.Start, _) => s}.head
    val allBlocks = controlFlowEdges
      .flatMap { case (k, v) => Seq(k, v) }
      .collect { case b: SSA.Block => b }
      .distinct

    val blockEdges = controlFlowEdges.flatMap {
      case (k: SSA.Block, v: SSA.Jump) => Nil
      case (k: SSA.Jump, v: SSA.Block) => Seq(k.block -> v)
      case (k: SSA.Block, v: SSA.Block) => Seq(k -> v)
    }

    (controlFlowEdges, startBlock, allBlocks, blockEdges)
  }
}
