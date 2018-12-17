package joptimize

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes}
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

    val classNodeMap = classFileMap.map{case (k, v) => (JType.Cls(v.name), v)}
    val subtypeMap = {
      val map = mutable.Map.empty[JType.Cls, List[JType.Cls]]
      for{
        (k, v) <- classNodeMap
        sup <- v.interfaces.asScala ++ Option(v.superName)
      } map(sup) = v.name :: map.getOrElse(sup, Nil)
      map
    }

    val originalMethods = for{
      (k, cls) <- classNodeMap
      m <- cls.methods.iterator().asScala
    } yield (MethodSig(cls.name, m.name, Desc.read(m.desc), (m.access & Opcodes.ACC_STATIC) != 0), m)

    val visitedMethods = collection.mutable.Map.empty[(MethodSig, Seq[IType]), (IType, InsnList)]

    def leastUpperBound(classes: Seq[JType.Cls]) = {
      Util.leastUpperBound(classes.toSet) { cls =>
        classNodeMap.get(cls) match{
          case None => Nil
          case Some(cn) => (cn.interfaces.asScala ++ Option(cn.superName)).map(JType.Cls(_))
        }
      }.toSeq
    }

    def merge(itypes: Seq[IType]): IType = {
      val flattened = itypes.flatMap{
        case IType.Intersect(values) => values
        case j: JType => Seq(j)
      }
      if (flattened.distinct.length == 1) flattened.head
      else{
        assert(flattened.forall(_.isRef))
        if (flattened.exists(_.isInstanceOf[JType.Arr])) JType.Cls("java/lang/Object")
        else leastUpperBound(flattened.map(_.asInstanceOf[JType.Cls])) match{
          case Seq(single) => single
          case many => IType.Intersect(many)
        }
      }
    }

    val interp = new Walker(
      isInterface = s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
      lookupMethod = sig => originalMethods.get(sig),
      visitedMethods = visitedMethods,
      findSubtypes = subtypeMap.getOrElse(_, Nil),
      isConcrete = sig => originalMethods(sig).instructions.size != 0,
      merge = merge
    )

    for(entrypoint <- entrypoints){
      val mn = originalMethods(entrypoint)
      interp.walkMethod(
        entrypoint,
        mn.instructions,
        Desc.read(mn.desc).args,
        mn.maxLocals,
        mn.maxStack,
        Set()
      )
    }

    val newMethods = visitedMethods.toList.map{case ((sig, inferredTypes), (returnType, insns)) =>

      val originalNode = originalMethods(sig)

      val (mangledName, mangledDesc) =
        if (Util.isCompatible(inferredTypes, sig.desc.args)) (originalNode.name, Desc.read(originalNode.desc))
        else Util.mangle(originalNode.name, inferredTypes, sig.desc.args, returnType, sig.desc.ret)

      val newNode = new MethodNode(
        Opcodes.ASM6,
        originalNode.access,
        mangledName,
        mangledDesc.unparse,
        originalNode.signature,
        originalNode.exceptions.asScala.toArray
      )

      originalNode.accept(newNode)
      newNode.instructions = insns
      newNode.desc = mangledDesc.unparse
      classNodeMap(sig.cls) -> newNode
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
