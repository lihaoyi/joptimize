package joptimize

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes}
import org.objectweb.asm.tree._

import collection.JavaConverters._
import scala.collection.mutable
object JOptimize{
  def run(classFiles: Map[String, Array[Byte]],
          entrypoints: Seq[MethodSig],
          eliminateOldMethods: Boolean): Map[String, Array[Byte]] = {

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

    val visitedMethods = collection.mutable.Map.empty[(MethodSig, Seq[IType]), Walker.MethodResult]

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
        case j => Seq(j)
      }.distinct
      if (flattened.length == 1) flattened.head
      else if (flattened.forall(_.isRef)){
        if (flattened.exists(_.isInstanceOf[JType.Arr])) JType.Cls("java/lang/Object")
        else leastUpperBound(flattened.map(_.asInstanceOf[JType.Cls])) match{
          case Seq(single) => single
          case many => IType.Intersect(many)
        }
      }
      else if(flattened.forall(_.widen == JType.Prim.Z)) JType.Prim.Z
      else if(flattened.forall(_.widen == JType.Prim.B)) JType.Prim.B
      else if(flattened.forall(_.widen == JType.Prim.C)) JType.Prim.C
      else if(flattened.forall(_.widen == JType.Prim.S)) JType.Prim.S
      else if(flattened.forall(_.widen == JType.Prim.I)) JType.Prim.I
      else if(flattened.forall(_.widen == JType.Prim.F)) JType.Prim.F
      else if(flattened.forall(_.widen == JType.Prim.J)) JType.Prim.J
      else if(flattened.forall(_.widen == JType.Prim.D)) JType.Prim.D
      else throw new Exception(flattened.toString)
    }

    def ignore(s: String) = s.startsWith("java/") || s.startsWith("scala/")

    val visitedClasses = mutable.Set.empty[JType.Cls]
    val interp = new Walker(
      isInterface = s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
      lookupMethod = sig => originalMethods.get(sig),
      visitedMethods = visitedMethods,
      visitedClasses = visitedClasses,
      findSubtypes = subtypeMap.getOrElse(_, Nil),
      isConcrete = sig => originalMethods(sig).instructions.size != 0,
      merge = merge,
      dataflow = new Dataflow(merge),
      ignore = ignore
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

    val newMethods = visitedMethods.toList.collect{
      case ((sig, inferredTypes), Walker.MethodResult(liveArgs, returnType, insns, pure))
        if !(pure && returnType.isConstant)=>

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

    val outClasses = PostLivenessDCE(
      entrypoints,
      grouped.keys.toSeq,
      findSubtypes = subtypeMap.getOrElse(_, Nil),
      classNodeMap,
      ignore = ignore
    )

    outClasses
      .map{cn =>
        val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        cn.accept(cw)
        (cn.name + ".class", cw.toByteArray)
      }
      .toMap
  }
}
