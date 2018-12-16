package joptimize

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes, Type}
import org.objectweb.asm.tree._

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
    val subtypeMap = {
      val map = mutable.Map.empty[String, List[String]]
      for{
        (k, v) <- classNodeMap
        sup <- v.interfaces.asScala ++ Option(v.superName)
      } map(sup) = v.name :: map.getOrElse(sup, Nil)
      map
    }

    val originalMethods = for{
      (k, cls) <- classNodeMap
      m <- cls.methods.iterator().asScala
    } yield (MethodSig(cls.name, m.name, m.desc, (m.access & Opcodes.ACC_STATIC) != 0), m)

    val visitedMethods = collection.mutable.Map.empty[(MethodSig, Seq[Type]), (Type, InsnList)]

    val interp = new AbstractInterpreter(
      isInterface = s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
      lookupMethod = sig => originalMethods.get(sig),
      visitedMethods = visitedMethods,
      findSubtypes = subtypeMap.getOrElse(_, Nil),
      isConcrete = sig => originalMethods(sig).instructions.size != 0
    )

    for(entrypoint <- entrypoints){
      val mn = originalMethods(entrypoint)
      interp.walkMethod(
        entrypoint,
        mn.instructions,
        Type.getType(mn.desc).getArgumentTypes.map(Inferred(_)).toList,
        mn.maxLocals,
        mn.maxStack,
        Set()
      )
    }

    val newMethods = visitedMethods.toList.map{case ((sig, inferredTypes), (returnType, insns)) =>
      val args = Type.getMethodType(sig.desc).getArgumentTypes.toSeq

      val originalNode = originalMethods(sig)
      val (mangledName, mangledDesc) =
        if (args == inferredTypes) (originalNode.name, originalNode.desc)
        else Util.mangle(originalNode.name, inferredTypes, returnType)

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
      newNode.desc = Type.getMethodDescriptor(returnType, inferredTypes:_*)
      classNodeMap(sig.clsName) -> newNode
    }

    if (eliminateOldMethods) {
      for ((k, cn) <- classFileMap) {
        cn.methods.clear()
      }
    }

    // Discover all interfaces implemented by the visited classes and find all
    // their super-interfaces. We need to discover these separately as interfaces
    // do not have an <init> method and if unused won't be picked up by the
    // abstract interpreter, but still need to be present since they're
    // implemented by the classes we do use
    val visitedInterfaces = mutable.Set.empty[String]
    val queue = newMethods.flatMap(_._1.interfaces.asScala).distinct.to[mutable.Queue]
    while (queue.nonEmpty){
      val current = queue.dequeue()
      if (!visitedInterfaces.contains(current)){
        visitedInterfaces.add(current)
        queue.enqueue(classNodeMap(current).interfaces.asScala:_*)
      }
    }



    val grouped =
      visitedInterfaces.map(classNodeMap(_) -> Nil).toMap ++
      newMethods.groupBy(_._1).mapValues(_.map(_._2))

    for((cn, mns) <- grouped) yield {
      cn.methods.addAll(mns.asJava)
      val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
      cn.accept(cw)
      (cn.name + ".class", cw.toByteArray)
    }
  }
}
