package joptimize.frontend

import joptimize.Util
import joptimize.algorithms.MultiBiMap
import joptimize.model.{CType, IType, JType, MethodSig}
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{ClassNode, MethodNode}
import collection.JavaConverters._
import scala.collection.mutable
object ClassManager{
  trait ReadOnly{
    def subtypeMap: MultiBiMap[JType.Cls, JType.Cls]
    def loadClassCache: collection.Map[JType.Cls, Option[ClassNode]]
    def loadMethodCache: collection.Map[MethodSig, Option[MethodNode]]
  }
}
class ClassManager(getClassFile: String => Option[Array[Byte]]) extends ClassManager.ReadOnly {

  val subtypeMap = new MultiBiMap.Mutable[JType.Cls, JType.Cls]

  val loadClassCache = mutable.LinkedHashMap.empty[JType.Cls, Option[ClassNode]]
  val loadMethodCache = mutable.LinkedHashMap.empty[MethodSig, Option[MethodNode]]

  def resolvePossibleSigs(sig: MethodSig): Option[Seq[MethodSig]] = {
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
        .lookupKeyOpt(sig.cls)
        .getOrElse(Nil)
        .map(c => sig.copy(cls = c))
        .toList

      Some(sig :: subTypes)
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
        subtypeMap.add(up, cls)
        loadClass(up)
      }

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


  def leastUpperBound(classes: Seq[JType.Cls]) = {
    Util.leastUpperBound(classes.toSet) { cls =>
      loadClass(cls) match{
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
}
