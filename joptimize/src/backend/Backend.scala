package joptimize.backend

import joptimize.Util
import joptimize.algorithms.{Dominator, Scheduler}
import joptimize.analyzer.{Analyzer, Namer}
import joptimize.graph.HavlakLoopTree
import joptimize.model.{IType, JType, MethodSig}
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{ClassNode, InsnList, MethodNode}

import collection.JavaConverters._
import scala.collection.mutable

object Backend {
  def apply(entrypoints: scala.Seq[MethodSig],
            originalMethods: Map[MethodSig, MethodNode],
            classNodeMap: Map[JType.Cls, ClassNode],
            visitedMethods: mutable.LinkedHashMap[(MethodSig, Seq[IType]), Analyzer.MethodResult],
            eliminateOldMethods: Boolean,
            classFileMap: Map[String, ClassNode],
            visitedClasses: mutable.LinkedHashSet[JType.Cls],
            subtypeMap: mutable.LinkedHashMap[JType.Cls, scala.List[JType.Cls]]) = {
    val newMethods = visitedMethods.toList.collect {
      case (
        (sig, inferredArgs),
        Analyzer.MethodResult(returnType, program, seenTryCatchBlocks, allVertices2)) =>

        val originalNode = originalMethods(sig)

        val (mangledName, mangledDesc) =
          if (sig.name == "<init>") (sig.name, sig.desc)
          else Util.mangle(sig, inferredArgs, returnType)

        val newNode = new MethodNode(
          Opcodes.ASM6,
          originalNode.access,
          mangledName,
          mangledDesc.unparse,
          originalNode.signature,
          originalNode.exceptions.asScala.toArray
        )

        originalNode.accept(newNode)

        if (program.allTerminals.isEmpty) newNode.instructions = new InsnList()
        else {
          val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
            Analyzer.analyzeBlockStructure(program)
          val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

          val dominators2 = Dominator.findDominators(blockEdges, allBlocks)

          { // Just for debugging
            val nodesToBlocks = Scheduler.apply(
              loopTree2, dominators2, startBlock,
              program.getAllVertices()
            )

            val postOptimisticNaming = Namer.apply(program, nodesToBlocks, program.getAllVertices())

            //        log(Renderer.renderSSA(program, postOptimisticNaming, nodesToBlocks))
          }

          //        log.println("================ REGISTERS ALLOCATED ================")
          RegisterAllocator.apply(program, dominators2.immediateDominators)

          val nodesToBlocks = Scheduler.apply(
            loopTree2, dominators2, startBlock,
            allVertices2
          )

          val postRegisterAllocNaming = Namer.apply(program, nodesToBlocks, program.getAllVertices())

          //        log(Renderer.renderSSA(program, postRegisterAllocNaming, nodesToBlocks))

          val (blockCode, finalInsns) = CodeGenMethod(
            program,
            allVertices2,
            nodesToBlocks,
            Analyzer.analyzeBlockStructure(program)._1,
            postRegisterAllocNaming
          )

          //        log.println("================ OUTPUT BYTECODE ================")
          //        log(Renderer.renderBlockCode(blockCode, finalInsns))

          newNode.instructions = finalInsns
        }
        newNode.desc = mangledDesc.unparse
        newNode.tryCatchBlocks = seenTryCatchBlocks.asJava

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
}
