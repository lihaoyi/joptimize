package joptimize.backend

import joptimize.{FileLogger, Logger, Util}
import joptimize.algorithms.{Dominator, MultiBiMap, Scheduler}
import joptimize.analyzer.{Analyzer, Namer, Renderer}
import joptimize.frontend.ClassManager
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import joptimize.optimize.OptimisticSimplify
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{ClassNode, InsnList, MethodNode}

import collection.JavaConverters._
import scala.collection.mutable

object Backend {
  def apply(analyzerRes: Analyzer.GlobalResult,
            entrypoints: scala.Seq[MethodSig],
            classManager: ClassManager.ReadOnly,
            eliminateOldMethods: Boolean,
            log: Logger.Global) = {

    val loadMethodCache = classManager.loadMethodCache.collect{case (k, Some(v)) => (k, v)}.toMap
    val loadClassCache = classManager.loadClassCache.collect{case (k, Some(v)) => (k, v)}.toMap
    //    pprint.log(loadMethodCache.keys)
    val combined = analyzerRes.visitedResolved.mapValues(Right(_)) ++ analyzerRes.visitedMethods.mapValues(Left(_))

    val highestMethodDefiners = for{
      (isig, _) <- combined
      if loadMethodCache.contains(isig.method)
    } yield {
      var parentClasses = List.empty[JType.Cls]
      var current = isig.method.cls
      while({
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
      })()

      val highestCls = parentClasses
        .find{ cls =>

          val key = isig.copy(method = isig.method.copy(cls = cls))
          val res = analyzerRes.visitedResolved.contains(key)
          res
        }
        .get

      (isig, highestCls)
    }
//    pprint.log(highestMethodDefiners.keys)

    val newMethods = for{
      (isig, result) <- combined.toList
      if loadClassCache.contains(isig.method.cls)
      if loadMethodCache.contains(isig.method)
    } yield {
      val liveArgs =
        if (entrypoints.contains(isig.method)) (_: Int) => true
        else{
          val highestSig =
            if (isig.method.static) isig.method
            else isig.method.copy(cls = highestMethodDefiners(isig))

          analyzerRes.visitedResolved(isig.copy(method = highestSig)).liveArgs
        }

      log.pprint(isig.method)

      val originalNode = loadMethodCache(isig.method)

      val props = result match{
        case Left(res) => res.props
        case Right(props) => props
      }
      val (mangledName, mangledDesc) =
        if (isig.method.name == "<init>") (isig.method.name, isig.method.desc)
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
      result match{
        case Right(props) => newNode.instructions = new InsnList()
        case Left(res) =>
          val argMapping = Util.argMapping(isig.method, liveArgs)

          newNode.instructions = processMethodBody(
            isig.method,
            res,
            log.inferredMethod(isig),
            loadClassCache.contains,
            (invokedSig, invokeSpecial) => {

              if (invokeSpecial){
                analyzerRes.visitedMethods.getOrElse(
                  invokedSig,
                  Analyzer.dummyResult(invokedSig.method, optimistic = false)
                ).props
              }else{

                val highestSig =
                  if (invokedSig.method.static || !loadMethodCache.contains(invokedSig.method)) invokedSig.method
                  else invokedSig.method.copy(cls = highestMethodDefiners(invokedSig))


                val res = analyzerRes.visitedResolved.getOrElse(
                  invokedSig.copy(method = highestSig),
                  Analyzer.dummyResult(highestSig, optimistic = false).props
                )

                res
              }
            },
            argMapping
          )


      }
      newNode.desc = mangledDesc.unparse
      newNode.tryCatchBlocks = Nil.asJava

      loadClassCache(isig.method.cls) -> newNode
    }

    if (eliminateOldMethods) {
      for ((k, cn) <- loadClassCache) {
        cn.methods.clear()
      }
    }

    val visitedInterfaces = Util.findSeenInterfaces(loadClassCache, newMethods.map(_._1))

    val grouped =
      (visitedInterfaces ++ analyzerRes.visitedClasses.map(_.name)).filter(s => loadClassCache.contains(JType.Cls(s))).map(loadClassCache(_) -> Nil).toMap ++
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
      while(loadClassCache.contains(output.last) && loadClassCache(output.last).superName != null && !ignore(loadClassCache(output.last).superName)){
        output.append(JType.Cls(loadClassCache(output.last).superName))
      }
      output
    }

//    val outClasses = BytecodeDCE.apply(
//      entrypoints,
//      grouped.keys.toSeq,
//      findSubtypes = classManager.subtypeMap.lookupKeyOpt(_).getOrElse(Nil),
//      findSupertypes = findSupertypes,
//      ignore = ignore
//    )
//
//    outClasses
    grouped.keys
  }

  def processMethodBody(originalSig: MethodSig,
                        result: Analyzer.Result,
                        log: Logger.InferredMethod,
                        classExists: JType.Cls => Boolean,
                        resolvedProperties: (InferredSig, Boolean) => Analyzer.Properties,
                        argMapping: Map[Int, Int]) = {



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
    log.graph(Renderer.dumpSvg(result.methodBody))
    log.check(result.methodBody.checkLinks())

    val allVertices2 = result.methodBody.getAllVertices()

//    pprint.log(originalSig)
//    pprint.log(result.program)
    val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
      Analyzer.analyzeBlockStructure(result.methodBody)
    val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

    val dominators2 = Dominator.findDominators(blockEdges, allBlocks)

    { // Just for debugging
      val nodesToBlocks = Scheduler.apply(
        loopTree2, dominators2, startBlock,
        allVertices2
      )

      val postOptimisticNaming = Namer.apply(
        result.methodBody,
        nodesToBlocks,
        allVertices2,
        log
      )

      log(Renderer.renderSSA(result.methodBody, postOptimisticNaming, nodesToBlocks))
    }

    log.println("================ REGISTERS ALLOCATED ================")
    RegisterAllocator.apply(result.methodBody, dominators2.immediateDominators)

    val nodesToBlocks = Scheduler.apply(
      loopTree2, dominators2, startBlock,
      allVertices2
    )

    val postRegisterAllocNaming = Namer.apply(
      result.methodBody,
      nodesToBlocks,
      result.methodBody.getAllVertices(),
      log
    )

    log(Renderer.renderSSA(result.methodBody, postRegisterAllocNaming, nodesToBlocks))

    val (blockCode, finalInsns) = CodeGenMethod(
      result.methodBody,
      allVertices2,
      nodesToBlocks,
      Analyzer.analyzeBlockStructure(result.methodBody)._1,
      postRegisterAllocNaming,
      log
    )

    log.println("================ OUTPUT BYTECODE ================")
    log(Renderer.renderBlockCode(blockCode, finalInsns))
    finalInsns
  }
}
