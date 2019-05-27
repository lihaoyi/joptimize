package joptimize.backend

import joptimize.{Logger, Util}
import joptimize.algorithms.{Dominator, Scheduler}
import joptimize.analyzer.{ProgramAnalyzer, Renderer}
import joptimize.frontend.{ClassManager, Frontend}
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{ClassNode, InsnList, MethodNode}

import collection.JavaConverters._

object Backend {
  def apply(
    analyzerRes: ProgramAnalyzer.ProgramResult,
    entrypoints: scala.Seq[MethodSig],
    classManager: ClassManager.Frozen,
    log: Logger.Global
  ): Seq[ClassNode] = log.block {

    val highestDefinerProps = computeHighestDefinerProps(
      analyzerRes,
      log,
      classManager
    )

    val newMethods = generateNewMethods(
      analyzerRes,
      entrypoints,
      log,
      highestDefinerProps,
      classManager
    )

    for (cn <- classManager.allClasses) cn.methods.clear()

    val visitedInterfaces =
      Util.findSeenInterfaces(classManager.loadClass, newMethods.map(_._1)) ++
      Seq("scala/runtime/Nothing$", "scala/runtime/Null$")

    log.pprint(visitedInterfaces)
    val lhs = (visitedInterfaces ++ analyzerRes
      .staticFieldReferencedClasses
      .flatMap(classManager.getAllSupertypes)
      .map(_.name))
      .filter(s => classManager.loadClass(JType.Cls(s)).nonEmpty)
      .map(classManager.loadClass(_).get -> Nil)
      .toMap
    val grouped: Map[ClassNode, Seq[MethodNode]] =
      lhs ++
      newMethods.groupBy(_._1).mapValues(_.toSeq.map(_._2))

    for ((cn, mns) <- grouped) yield {
      log.pprint(cn.name)
      log.pprint(mns.map(_.name))
      mns.foreach(cn.methods.add)
    }

    grouped.keys.toSeq
  }

  def generateNewMethods(
    analyzerRes: ProgramAnalyzer.ProgramResult,
    entrypoints: Seq[MethodSig],
    log: Logger.Global,
    highestDefinerProps: collection.Map[InferredSig, ProgramAnalyzer.Properties],
    classManager: ClassManager.Frozen
  ) = log.block {

    def resolveDefsiteProps(isig: InferredSig) = {
      if (isig.method.static) analyzerRes.visitedResolved(isig)
      else if (isig.method.name == "<init>") analyzerRes.visitedMethods(isig).get.props
      else highestDefinerProps(isig)
    }

    def resolveCallsiteProps(isig: InferredSig, invokeSpecial: Boolean) = {
      if (classManager.loadClass(isig.method.cls).isEmpty)
        ProgramAnalyzer.dummyProps(isig.method, optimistic = false)
      else if (classManager.resolvePossibleSigs(isig.method).exists(_.isEmpty)) {
        ProgramAnalyzer.dummyProps(isig.method, optimistic = false)
      } else if (invokeSpecial) analyzerRes.visitedMethods(isig).get.props
      else resolveDefsiteProps(isig)
    }

    for {
      isig <- analyzerRes.visitedMethods.keysIterator.toArray
      if classManager.loadMethod(isig.method).nonEmpty
    } yield Util.labelExceptions(isig.toString) {
      val props = resolveDefsiteProps(isig)

      val liveArgs =
        if (entrypoints.contains(isig.method)) (_: Int) => true
        else props.liveArgs

      val originalNode = classManager.loadMethod(isig.method).get

      val (mangledName, mangledDesc) =
        if (entrypoints.contains(isig.method) || isig.method.name == "<init>")
          (isig.method.name, isig.method.desc)
        else Util.mangle(isig, props.inferredReturn, liveArgs)

      val newNode = new MethodNode(
        Opcodes.ASM6,
        originalNode.access,
        mangledName,
        mangledDesc.render,
        originalNode.signature,
        originalNode.exceptions.asScala.toArray
      )
      originalNode.accept(newNode)
      if (analyzerRes.visitedMethods(isig).isEmpty) newNode.instructions = new InsnList()
      else {
        newNode.instructions = processMethodBody(
          isig.method,
          analyzerRes.visitedMethods(isig).get,
          log.inferredMethod(isig),
          classManager.loadClass(_).nonEmpty,
          resolveCallsiteProps,
          argMapping = Util.argMapping(isig.method, liveArgs),
          cls => classManager.loadClass(cls).map(c => (c.access & Opcodes.ACC_INTERFACE) != 0)
        )
      }
      newNode.desc = mangledDesc.render
      newNode.tryCatchBlocks = Nil.asJava

      classManager.loadClass(isig.method.cls).get -> newNode
    }
  }

  def computeHighestDefinerProps(
    analyzerRes: ProgramAnalyzer.ProgramResult,
    log: Logger.Global,
    classManager: ClassManager.Frozen
  ) = log.block {

    val allKeys = analyzerRes.visitedMethods.keySet ++ analyzerRes.visitedResolved.keySet
    val items = for (isig <- allKeys if !isig.method.static) yield {

//      pprint.log(isig.toString)
      val allSupertypes = classManager.getAllSupertypes(isig.method.cls)
//      pprint.log(allSupertypes)
      val filtered = allSupertypes.filter(
        cls => analyzerRes.visitedMethods.contains(isig.copy(method = isig.method.copy(cls = cls)))
      )
//      pprint.log(filtered)
      val flatMapped = filtered.flatMap(cls => classManager.getAllSubtypes(cls))
//      pprint.log(flatMapped)
      val mapped = flatMapped.map(cls => isig.copy(method = isig.method.copy(cls = cls)))
//      pprint.log(mapped)
      val allProps: Seq[ProgramAnalyzer.MethodResult] =
        mapped.flatMap(analyzerRes.visitedMethods.getOrElse(_, None))

      val (allRets, allPures, allLives) =
        allProps.map(p => (p.props.inferredReturn, p.props.pure, p.props.liveArgs)).unzip3

//      pprint.log(allRets)
      Tuple2(
        isig,
        ProgramAnalyzer.Properties(
          classManager.mergeTypes(allRets),
          allPures.forall(identity),
          allLives.flatten.toSet
        )
      )
    }
    items.toMap
  }

  def processMethodBody(
    originalSig: MethodSig,
    result: ProgramAnalyzer.MethodResult,
    log: Logger.InferredMethod,
    classExists: JType.Cls => Boolean,
    resolvedProperties: (InferredSig, Boolean) => ProgramAnalyzer.Properties,
    argMapping: Map[Int, Int],
    isInterface: JType.Cls => Option[Boolean]
  ) = log.block {

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

    log.pprint(result.inferred)
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

    val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
      analyzeBlockStructure(result.methodBody)

    val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

    val dominators2 = Dominator.findDominators(blockEdges, allBlocks)

    val nodesToBlocks = Scheduler.apply(
      loopTree2,
      dominators2,
      startBlock,
      allVertices2,
      log
    )

    val finalInsns = new CodeGenMethod(
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
    val startBlock = controlFlowEdges.collect { case (s: SSA.Start, _) => s }.head
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
