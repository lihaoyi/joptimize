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
      val output = mutable.Buffer(cls)
      while(classNodeMap.contains(output.last) && classNodeMap(output.last).superName != null && !ignore(classNodeMap(output.last).superName)){
        output.append(JType.Cls(classNodeMap(output.last).superName))
      }
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

    val callerGraph = mutable.LinkedHashMap[MethodSig, mutable.LinkedHashSet[MethodSig]]()
    def computeMethodSig(invoke: SSA.Invoke,
                         inferredArgs: Seq[IType],
                         callStack: List[(MethodSig, Seq[IType])]): IType = {

      val subSigs = {
        invoke match{
          case _: SSA.InvokeStatic =>
            def rec(currentCls: JType.Cls): MethodSig = {
              val sig = invoke.sig
              if (originalMethods.contains(sig)) sig
              else rec(JType.Cls(classNodeMap(currentCls).superName))
            }
            Seq(rec(invoke.cls))
          case _: SSA.InvokeSpecial => Seq(invoke.sig)
          case _: SSA.InvokeVirtual | _: SSA.InvokeInterface =>
            invoke.sig :: subtypeMap.getOrElse(invoke.cls, Nil).map(c => invoke.sig.copy(cls = c))
        }
      }
      val rets = for(subSig <- subSigs) yield originalMethods.get(subSig) match{
        case Some(original) =>
          visitedMethods.getOrElseUpdate(
            (subSig, inferredArgs),
            {
              val (res, newVisitedClasses, calledMethods) = walker.walkMethod(
                subSig,
                original,
                computeMethodSig,
                inferredArgs,
                computeSideEffects,
                (inf, orig) => leastUpperBound(Seq(inf, orig)) == Seq(orig),
                callStack
              )
              newVisitedClasses.foreach(visitedClasses.add)
              for(m <- calledMethods){
                callerGraph.getOrElseUpdate(m, mutable.LinkedHashSet.empty).add(subSig)
              }
              res
            }
          ).inferredReturn
        case None =>
          invoke.desc.ret
      }

      merge(rets)
    }

    for(ep <- entrypoints){
      val (res, seenClasses, calledMethods) = walker.walkMethod(
        ep,
        originalMethods(ep),
        computeMethodSig,
        ep.desc.args,
        computeSideEffects,
        (inf, orig) => leastUpperBound(Seq(inf, orig)) == Seq(orig),
        Nil
      )
      for(m <- calledMethods){
        callerGraph.getOrElseUpdate(m, mutable.LinkedHashSet.empty).add(ep)
      }

      visitedMethods((ep, ep.desc.args)) = res
      seenClasses.foreach(visitedClasses.add)
    }
    pprint.log(visitedMethods, height=9999)

    val newMethods = visitedMethods.toList.collect{
      case ((sig, inferredArgs), Walker.MethodResult(liveArgs, returnType, insns, pure, seenTryCatchBlocks)) =>

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

      cn.methods.addAll(mns.asJava)
    }

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
