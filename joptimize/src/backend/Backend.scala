package joptimize.backend

import joptimize.{FileLogger, Logger, Util}
import joptimize.algorithms.{Dominator, Scheduler}
import joptimize.analyzer.{Analyzer, Namer, Renderer}
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import joptimize.optimize.OptimisticSimplify
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{ClassNode, InsnList, MethodNode}

import collection.JavaConverters._
import scala.collection.mutable

object Backend {
  def apply(entrypoints: scala.Seq[MethodSig],
            originalMethods: Map[MethodSig, MethodNode],
            classNodeMap: Map[JType.Cls, ClassNode],
            visitedMethods: mutable.LinkedHashMap[(MethodSig, Seq[IType]), Analyzer.Result],
            eliminateOldMethods: Boolean,
            classFileMap: Map[String, ClassNode],
            visitedClasses: mutable.LinkedHashSet[JType.Cls],
            subtypeMap: mutable.LinkedHashMap[JType.Cls, scala.List[JType.Cls]],
            log: Logger.Global,
            leastUpperBound: Seq[JType.Cls] => Seq[JType.Cls],
            merge: Seq[IType] => IType) = {

    def computeMethodSig(sig: MethodSig,
                         invokeSpecial: Boolean,
                         inferredArgs: Seq[IType]): (IType, Boolean, Set[Int]) = {

      val subSigs = {
        (sig.static, invokeSpecial) match {
          case (true, false) =>
            def rec(currentCls: JType.Cls): Option[MethodSig] = {
              val currentSig = sig.copy(cls = currentCls)
              if (originalMethods.contains(currentSig)) Some(currentSig)
              else if (!classNodeMap.contains(currentCls)) None
              else rec(JType.Cls(classNodeMap(currentCls).superName))
            }

            if (sig.name == "<clinit>") Some(Seq(sig))
            else rec(sig.cls).map(Seq(_))
          case (false, true) => Some(Seq(sig))
          case (false, false) =>
            val subTypes = subtypeMap
              .getOrElse(sig.cls, Nil)
              .filter(c => leastUpperBound(Seq(c, inferredArgs(0).asInstanceOf[JType.Cls])) == Seq(inferredArgs(0)))
              .map(c => sig.copy(cls = c))

            Some(sig :: subTypes)
        }
      }

      subSigs match {
        case None => (sig.desc.ret, false, sig.desc.args.indices.toSet)
        case Some(subSigs) =>
          val rets = for (subSig <- subSigs) yield originalMethods.get(subSig) match {
            case Some(original) =>
              val res = visitedMethods(
                (subSig, inferredArgs.drop(if (sig.static) 0 else 1)),
              )
              (res.inferredReturn, res.pure, res.liveArgs)
            case None =>
              (sig.desc.ret, false, sig.desc.args.indices.toSet)
          }

          val (retTypes, retPurity, retLiveArgs) = rets.unzip3
          (merge(retTypes), retPurity.forall(identity), retLiveArgs.iterator.flatten.toSet)
      }
    }

    val newMethods = for(((sig, inferredArgs), result) <- visitedMethods.toList) yield {
      log.pprint(sig)
      val allVertices2 = result.program.getAllVertices()
      val originalNode = originalMethods(sig)

      val (mangledName, mangledDesc) =
        if (sig.name == "<init>") (sig.name, sig.desc)
        else Util.mangle(sig, inferredArgs, result.inferredReturn, result.liveArgs)

      val newNode = new MethodNode(
        Opcodes.ASM6,
        originalNode.access,
        mangledName,
        mangledDesc.unparse,
        originalNode.signature,
        originalNode.exceptions.asScala.toArray
      )

      originalNode.accept(newNode)

      if (result.program.allTerminals.isEmpty) newNode.instructions = new InsnList()
      else {

        val argMapping: Map[Int, Int] = {
          val liveArgs = visitedMethods((sig, inferredArgs)).liveArgs
          log.pprint(liveArgs)
          var originalIndex = if (sig.static) 0 else 1
          log.pprint(originalIndex)
          var finalIndex = originalIndex
          log.pprint(finalIndex)
          val output = mutable.Map.empty[Int, Int]
          for(arg <- sig.desc.args){
            if (liveArgs(originalIndex)){
              output(originalIndex) = finalIndex
              finalIndex += arg.size
            }
            originalIndex += arg.size
          }

          output.toMap
        }

        newNode.instructions = processMethodBody(
          sig,
          inferredArgs,
          result,
          allVertices2,
          log.inferredMethod(sig, inferredArgs),
          classNodeMap.contains,
          (originalSig, special, inferredArgs) => computeMethodSig(originalSig, special, inferredArgs)._3,
          argMapping
        )
      }
      newNode.desc = mangledDesc.unparse
      newNode.tryCatchBlocks = Nil.asJava

      classNodeMap(sig.cls) -> newNode
    }

    if (eliminateOldMethods) {
      for ((k, cn) <- classFileMap) {
        cn.methods.clear()
      }
    }

    val visitedInterfaces = Util.findSeenInterfaces(classNodeMap, newMethods.map(_._1))

    val grouped =
      (visitedInterfaces ++ visitedClasses.map(_.name)).map(classNodeMap(_) -> Nil).toMap ++
        newMethods.groupBy(_._1).mapValues(_.map(_._2))

    for((cn, mns) <- grouped) yield {
      if (cn.attrs != null) Util.removeFromJavaList(cn.attrs)(_.`type` == "ScalaSig")
      if (cn.attrs != null) Util.removeFromJavaList(cn.attrs)(_.`type` == "ScalaInlineInfo")
      if (cn.visibleAnnotations != null) {
        Util.removeFromJavaList(cn.visibleAnnotations )(_.desc == "Lscala/reflect/ScalaSignature;")
      }

      cn.methods.addAll(mns.asJava)
    }
    def ignore(s: String) = s.startsWith("java/") || s.startsWith("scala/")

    def findSupertypes(cls: JType.Cls) = {
      val output = mutable.Buffer(cls)
      while(classNodeMap.contains(output.last) && classNodeMap(output.last).superName != null && !ignore(classNodeMap(output.last).superName)){
        output.append(JType.Cls(classNodeMap(output.last).superName))
      }
      output
    }
    val outClasses = BytecodeDCE.apply(
      entrypoints,
      grouped.keys.toSeq,
      findSubtypes = subtypeMap.getOrElse(_, Nil),
      findSupertypes = findSupertypes,
      ignore = ignore
    )

    outClasses
  }

  def processMethodBody(originalSig: MethodSig,
                        inferredArgs: Seq[IType],
                        result: Analyzer.Result,
                        allVertices2: Set[SSA.Node],
                        log: Logger.InferredMethod,
                        classExists: JType.Cls => Boolean,
                        liveArgsFor: (MethodSig, Boolean, Seq[IType]) => Set[Int],
                        argMapping: Map[Int, Int]) = {



    log.pprint(argMapping)

    OptimisticSimplify.apply(
      originalSig.static,
      argMapping,
      result.program,
      result.inferred,
      result.liveBlocks,
      log,
      classExists,
      liveArgsFor
    )

    log.check(result.program.checkLinks(checkDead = false))
    result.program.removeDeadNodes()
    log.graph(Renderer.dumpSvg(result.program))
    log.check(result.program.checkLinks())

    val allVertices2 = result.program.getAllVertices()

//    pprint.log(originalSig)
//    pprint.log(result.program)
    val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
      Analyzer.analyzeBlockStructure(result.program)
    val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

    val dominators2 = Dominator.findDominators(blockEdges, allBlocks)

    { // Just for debugging
      val nodesToBlocks = Scheduler.apply(
        loopTree2, dominators2, startBlock,
        allVertices2
      )

      val postOptimisticNaming = Namer.apply(
        result.program,
        nodesToBlocks,
        allVertices2,
        log
      )

      log(Renderer.renderSSA(result.program, postOptimisticNaming, nodesToBlocks))
    }

    log.println("================ REGISTERS ALLOCATED ================")
    RegisterAllocator.apply(result.program, dominators2.immediateDominators)

    val nodesToBlocks = Scheduler.apply(
      loopTree2, dominators2, startBlock,
      allVertices2
    )

    val postRegisterAllocNaming = Namer.apply(
      result.program,
      nodesToBlocks,
      result.program.getAllVertices(),
      log
    )

    log(Renderer.renderSSA(result.program, postRegisterAllocNaming, nodesToBlocks))

    val (blockCode, finalInsns) = CodeGenMethod(
      result.program,
      allVertices2,
      nodesToBlocks,
      Analyzer.analyzeBlockStructure(result.program)._1,
      postRegisterAllocNaming,
      log
    )

    log.println("================ OUTPUT BYTECODE ================")
    log(Renderer.renderBlockCode(blockCode, finalInsns))
    finalInsns
  }
}
