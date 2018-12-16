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

    val visitedMethods = collection.mutable.Map.empty[(MethodSig, Seq[IType]), (JType, InsnList)]

    def leastUpperBound(classes: Seq[JType.Cls]) = {
      Util.leastUpperBound(classes.toSet) { cls =>
        classNodeMap.get(cls) match{
          case None => Nil
          case Some(cn) => (cn.interfaces.asScala ++ Option(cn.superName)).map(JType.Cls(_))
        }
      }.toSeq
    }

    val interp = new AbstractInterpreter(
      isInterface = s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
      lookupMethod = sig => originalMethods.get(sig),
      visitedMethods = visitedMethods,
      findSubtypes = subtypeMap.getOrElse(_, Nil),
      isConcrete = sig => originalMethods(sig).instructions.size != 0,
      new Dataflow(merge0 = (lhs, rhs) => {
        if (lhs == rhs) lhs
        else {
          assert(lhs.isRef)
          assert(rhs.isRef)
          (lhs, rhs) match{
            case (l: JType.Cls, r: JType.Cls) => IType.Intersect(leastUpperBound(Seq(l, r)))
            case (l: IType.Intersect, r: JType.Cls) => IType.Intersect(leastUpperBound(l.classes ++ Seq(r)))
            case (l: JType.Cls, r: IType.Intersect) => IType.Intersect(leastUpperBound(Seq(l) ++ r.classes))
            case _ => JType.Cls("java/lang/Object")
          }
        }
      })
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
      val args = sig.desc.args

      val originalNode = originalMethods(sig)
      val (mangledName, mangledDesc) =
        if (args == inferredTypes) (originalNode.name, Desc.read(originalNode.desc))
        else Util.mangle(originalNode.name, inferredTypes.map(JType.fromIType), returnType)

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
      newNode.desc = Desc(inferredTypes.map(JType.fromIType), returnType).unparse
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
