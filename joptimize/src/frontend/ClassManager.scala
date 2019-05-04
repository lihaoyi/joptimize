package joptimize.frontend

import joptimize.Util
import joptimize.algorithms.MultiBiMap
import joptimize.model.{CType, IType, JType, MethodSig, SSA}
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{ClassNode, MethodNode}

import collection.JavaConverters._
import scala.collection.mutable
object ClassManager{
  trait ReadOnly{
    def subtypeMap: mutable.LinkedHashMap[JType.Cls, List[JType.Cls]]
    def supertypeMap: mutable.LinkedHashMap[JType.Cls, List[JType.Cls]]
    def loadClassCache: collection.Map[JType.Cls, Option[ClassNode]]
    def loadMethodCache: collection.Map[MethodSig, Option[MethodNode]]
  }
}
class ClassManager(getClassFile: String => Option[Array[Byte]]) extends ClassManager.ReadOnly {

  val subtypeMap = mutable.LinkedHashMap.empty[JType.Cls, List[JType.Cls]]
  val supertypeMap = mutable.LinkedHashMap.empty[JType.Cls, List[JType.Cls]]


  val loadClassCache = mutable.LinkedHashMap.empty[JType.Cls, Option[ClassNode]]
  val loadMethodCache = mutable.LinkedHashMap.empty[MethodSig, Option[MethodNode]]

  val seenLambdas = mutable.Map.empty[MethodSig, Set[(SSA.InvokeDynamic, MethodSig)]]

  def resolveSuperTypes(current0: JType.Cls): Seq[JType.Cls] = {
    val supers = supertypeMap.get(current0) match{
      case None => Nil
      case Some(sup) => sup.flatMap(resolveSuperTypes)
    }
    Seq(current0) ++ supers
  }

  def resolvePossibleSigs(sig: MethodSig): Option[Seq[MethodSig]] = {
//    pprint.log(sig)
//    pprint.log(seenLambdas)
    if (sig.static) {
      def rec(currentCls: JType.Cls): Option[MethodSig] = {
        val currentSig = sig.copy(cls = currentCls)
        if (loadMethod(currentSig).nonEmpty) Some(currentSig)
        else loadClass(currentCls) match {
          case None => None
          case Some(cls) => rec(JType.Cls(cls.superName))
        }
      }

      if (sig.name == "<clinit>") Some(Seq(sig))
      else rec(sig.cls).map(s => Seq(s))
    }else{
      //      pprint.log(subtypeMap.items().toMap)
      val subTypes = subtypeMap
        .getOrElse(sig.cls, Nil)
        .map(c => sig.copy(cls = c))

      val lambdas = for{
        (msig, set) <- seenLambdas
        if msig.name == sig.name && msig.desc == sig.desc
        if resolveSuperTypes(sig.cls).contains(msig.cls)
        (node, sig)<- set
      } yield sig
//      pprint.log(lambdas)
      Some(sig :: lambdas.toList ::: subTypes)
    }
  }


  def loadClass(cls: JType.Cls): Option[ClassNode] = loadClassCache.getOrElseUpdate(
    cls,
    getClassFile(cls.name + ".class").map { file =>
      val cr = new ClassReader(file)
      val cn = new ClassNode()
      cr.accept(cn, ClassReader.SKIP_FRAMES)
      val uppers = cn.interfaces.asScala ++ Option(cn.superName)
      val upperClasses = uppers.map(clsName => JType.Cls(clsName))
      for(up <- upperClasses) {
        subtypeMap(up) = cls :: subtypeMap.getOrElse(up, Nil)
        supertypeMap(cls) = up :: supertypeMap.getOrElse(cls, Nil)
        loadClass(up)
      }
//      pprint.log(cn.name)
//      pprint.log(cn.methods.asScala.map(m => m.name -> m.desc))

      cn
    }
  )

  def loadMethod(sig: MethodSig): Option[MethodNode] = loadMethodCache.getOrElseUpdate(
    sig,
    loadClass(sig.cls).flatMap { cls =>
      cls.methods.iterator().asScala.find( mn =>
        mn.name == sig.name && mn.desc == sig.desc.unparse
      )
    }
  )


  def mergeClasses(classes: Seq[JType.Cls]) = {
    Util.leastUpperBound(classes.toSet) { cls =>
      loadClass(cls) match{
        case None => Nil
        case Some(cn) => (cn.interfaces.asScala ++ Option(cn.superName)).map(JType.Cls(_))
      }
    }.toSeq
  }
  def mergeTypes(itypes: Seq[IType]): IType = {
    mergeTypes0(itypes).getOrElse(IType.Bottom)
  }
  def mergeTypes0(itypes: Seq[IType]): Option[IType] = {
    val flattened = itypes.flatMap{
      case IType.Bottom => Nil
      case CType.Intersect(values) => values
      case j => Seq(j)
    }.distinct
    if (flattened.length == 1) Some(flattened.head)
    else if(flattened.length == 0) None
    else if(flattened.forall(_.widen == JType.Prim.I)) Some(JType.Prim.I)
    else if(flattened.forall(_.widen == JType.Prim.F)) Some(JType.Prim.F)
    else if(flattened.forall(_.widen == JType.Prim.J)) Some(JType.Prim.J)
    else if(flattened.forall(_.widen == JType.Prim.D)) Some(JType.Prim.D)
    else if(flattened.forall(x => x.widen == JType.Prim.Z || x.getClass == classOf[CType.I])) Some(JType.Prim.Z)
    else if(flattened.forall(x => x.widen == JType.Prim.B || x.getClass == classOf[CType.I])) Some(JType.Prim.B)
    else if(flattened.forall(x => x.widen == JType.Prim.C || x.getClass == classOf[CType.I])) Some(JType.Prim.C)
    else if(flattened.forall(x => x.widen == JType.Prim.S || x.getClass == classOf[CType.I])) Some(JType.Prim.S)
    else if (flattened.forall(_.isInstanceOf[JType.Cls])) {
      mergeClasses(flattened.map(_.asInstanceOf[JType.Cls])) match {
        case Seq(single) => Some(single)
        case many => Some(CType.Intersect(many))
      }
    }
    else if (flattened.forall(_.isInstanceOf[JType.Arr])){
      Some(

        mergeTypes0(flattened.map{case a: JType.Arr => a.innerType}) match{
          case None => JType.Cls("java.lang.Object")
          case Some(merged) =>
            merged match{
              case j: JType => JType.Arr(j)
              case _ => JType.Arr(JType.Cls("java.lang.Object"))
            }
        }
      )
    }
    else None
  }
}
