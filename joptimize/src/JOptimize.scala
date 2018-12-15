package joptimize

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes, Type}
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Analyzer, Frame}

import collection.JavaConverters._
import scala.collection.mutable
object JOptimize{
  def run(classFiles: Map[String, Array[Byte]],
          entrypoints: Seq[MethodSig],
          eliminateOldMethods: Boolean) = {

    val classFileMap = for((k, v) <- classFiles) yield {
      val cr = new ClassReader(v)
      val cn = new ClassNode()
      cr.accept(cn, ClassReader.SKIP_FRAMES)
      (k, cn)
    }

    val classNodeMap = classFileMap.map{case (k, v) => (v.name, v)}

    val originalMethods = for{
      (k, cls) <- classNodeMap
      m <- cls.methods.iterator().asScala
    } yield (MethodSig(cls.name, m.name, m.desc, (m.access & Opcodes.ACC_STATIC) != 0), m)

    val visitedMethods = collection.mutable.Map.empty[(MethodSig, Seq[Type]), (Type, InsnList)]

    val interp = new AbstractInterpret(
      s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
      sig => originalMethods(sig),
      visitedMethods
    )

    for(entrypoint <- entrypoints){
      val mn = originalMethods(entrypoint)
      interp.interpretMethod(
        entrypoint,
        mn.instructions,
        Type.getType(mn.desc).getArgumentTypes.map(Inferred(_)).toList,
        mn.maxLocals,
        mn.maxStack
      )
    }

    val newMethods = visitedMethods.toList.map{case ((sig, inferredTypes), (returnType, insns)) =>
      val argSelf = if (sig.static) Nil else Seq(Type.getObjectType(sig.clsName))
      val args = argSelf ++ Type.getMethodType(sig.desc).getArgumentTypes.toSeq

      val originalNode = originalMethods(sig)
      val (mangledName, mangledDesc) =
        if (args == inferredTypes) (originalNode.name, originalNode.desc)
        else Util.mangle(originalNode.name, if (sig.static) inferredTypes else inferredTypes.drop(1), returnType)

      val newNode = new MethodNode(
        Opcodes.ASM6,
        originalNode.access,
        mangledName,
        mangledDesc,
        originalNode.signature,
        originalNode.exceptions.asScala.toArray
      )

      originalNode.accept(newNode)
      newNode.instructions = insns
      newNode.desc = Type.getMethodDescriptor(
        returnType,
        (if (sig.static) inferredTypes else inferredTypes.drop(1)):_*
      )
      classNodeMap(sig.clsName) -> newNode
    }

    if (eliminateOldMethods) {
      for ((k, cn) <- classFileMap) {
        cn.methods.removeIf { mn => mn.name != "<init>" && mn.name != "<clinit>" }
      }
    }

    val grouped = newMethods.groupBy(_._1).mapValues(_.map(_._2)).toMap
    for((cn, mns) <- grouped){
      cn.methods.addAll(mns.asJava)
    }

    for((k, cn) <- classFileMap) yield {
      val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
      cn.accept(cw)
      (k, cw.toByteArray)
    }
  }
}
