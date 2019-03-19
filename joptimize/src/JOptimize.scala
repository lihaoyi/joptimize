package joptimize

import joptimize.analysis.{PostLivenessDCE, Walker}
import joptimize.model._
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
      val map = mutable.LinkedHashMap.empty[JType.Cls, List[JType.Cls]]
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
        case CType.Intersect(values) => values
        case j => Seq(j)
      }.distinct
      if (flattened.length == 1) flattened.head
      else if (flattened.forall(_.isRef)){
        if (flattened.exists(_.isInstanceOf[JType.Arr])) JType.Cls("java/lang/Object")
        else leastUpperBound(flattened.map(_.asInstanceOf[JType.Cls])) match{
          case Seq(single) => single
          case many => CType.Intersect(many)
        }
      }
      else if(flattened.forall(_.widen == JType.Prim.I)) JType.Prim.I
      else if(flattened.forall(_.widen == JType.Prim.F)) JType.Prim.F
      else if(flattened.forall(_.widen == JType.Prim.J)) JType.Prim.J
      else if(flattened.forall(_.widen == JType.Prim.D)) JType.Prim.D
      else if(flattened.forall(x => x.widen == JType.Prim.Z || x.getClass == classOf[CType.I])) JType.Prim.Z
      else if(flattened.forall(x => x.widen == JType.Prim.B || x.getClass == classOf[CType.I])) JType.Prim.B
      else if(flattened.forall(x => x.widen == JType.Prim.C || x.getClass == classOf[CType.I])) JType.Prim.C
      else if(flattened.forall(x => x.widen == JType.Prim.S || x.getClass == classOf[CType.I])) JType.Prim.S
      else throw new Exception(flattened.toString)
    }

    def ignore(s: String) = s.startsWith("java/") || s.startsWith("scala/")

    def findSupertypes(cls: JType.Cls) = {
      pprint.log(cls)
      val output = mutable.Buffer(cls)
      while(classNodeMap.contains(output.last) && classNodeMap(output.last).superName != null && !ignore(classNodeMap(output.last).superName)){
        output.append(JType.Cls(classNodeMap(output.last).superName))
      }
      pprint.log(output)
      output
    }
    val visitedClasses = mutable.LinkedHashSet.empty[JType.Cls]
    val walker = new Walker((lhs, rhs) => merge(Seq(lhs, rhs)))

    val visitedMethods = mutable.LinkedHashMap.empty[(MethodSig, Seq[IType]), Walker.MethodResult]

    def computeSideEffects(sig: MethodSig, args: Seq[IType]) = {
      visitedMethods.get((sig, args)) match{
        case None => SideEffects.Global
        case Some(x) => x.sideEffects
      }
    }
    def computeMethodSig(invoke: SSA.Invoke, inferredArgs: Seq[IType]): IType = {

      val sig = {
        if (invoke.isInstanceOf[SSA.InvokeStatic]){
          def rec(currentCls: JType.Cls): MethodSig = {
            val sig = MethodSig(invoke.cls, invoke.name, invoke.desc, true)
            if (originalMethods.contains(sig)) sig
            else rec(JType.Cls(classNodeMap(currentCls).superName))
          }
          rec(invoke.cls)
        }else{
          MethodSig(invoke.cls, invoke.name, invoke.desc, false)
        }
      }
      pprint.log(sig)
      visitedMethods.getOrElseUpdate(
        (sig, inferredArgs),
        {
          val (res, newVisitedClasses) = walker.walkMethod(
            sig.cls.name,
            originalMethods(sig),
            computeMethodSig,
            inferredArgs,
            computeSideEffects
          )
          newVisitedClasses.foreach(visitedClasses.add)
          res
        }
      ).inferredReturn
    }

    for(ep <- entrypoints){
      val (res, seenClasses) = walker.walkMethod(
        ep.cls.name,
        originalMethods(ep),
        computeMethodSig,
        ep.desc.args,
        computeSideEffects
      )

      visitedMethods((ep, ep.desc.args)) = res
      seenClasses.foreach(visitedClasses.add)
    }
    pprint.log(visitedMethods)

    val newMethods = visitedMethods.toList.collect{
      case ((sig, inferredArgs), Walker.MethodResult(liveArgs, returnType, insns, pure, seenTryCatchBlocks)) =>

        val originalNode = originalMethods(sig)

        val (mangledName, mangledDesc) =
          Util.mangle(originalNode.name, inferredArgs, sig.desc.args, returnType, sig.desc.ret)

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

      pprint.log(cn.name)
      pprint.log(mns.map(_.name))
      cn.methods.addAll(mns.asJava)
    }

    pprint.log(entrypoints)
    pprint.log(grouped.keys.map(_.name))
    val outClasses = PostLivenessDCE(
      entrypoints,
      grouped.keys.toSeq,
      findSubtypes = subtypeMap.getOrElse(_, Nil),
      findSupertypes = findSupertypes,
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
