package joptimize.backend

import joptimize.Util
import joptimize.algorithms.{Dominator, Scheduler}
import joptimize.analysis.{Namer, Walker}
import joptimize.graph.HavlakLoopTree
import joptimize.model.{IType, JType, MethodSig}
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{ClassNode, InsnList, MethodNode}

import collection.JavaConverters._
import scala.collection.mutable

object Backend {
  def apply(originalMethods: Map[MethodSig, MethodNode],
            classNodeMap: Map[JType.Cls, ClassNode],
            visitedMethods: mutable.LinkedHashMap[(MethodSig, Seq[IType]), Walker.MethodResult]) = {
    visitedMethods.toList.collect {
      case (
        (sig, inferredArgs),
        Walker.MethodResult(returnType, program, seenTryCatchBlocks, blockEdges, allBlocks, startBlock, allVertices2)) =>

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

        if (startBlock == null) newNode.instructions = new InsnList()
        else {
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
            Walker.analyzeBlockStructure(program)._1,
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
  }

}
