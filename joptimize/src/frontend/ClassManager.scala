package joptimize.frontend

import joptimize.Util
import joptimize.model.{CType, IType, JType, MethodSig}
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{ClassNode, MethodNode}
import collection.JavaConverters._
import scala.collection.mutable
object ClassManager {

  def loadClassNode(classFileOpt: Option[Array[Byte]]) = {
    classFileOpt.map { file =>
      val cr = new ClassReader(file)
      val cn = new ClassNode()
      cr.accept(cn, ClassReader.SKIP_FRAMES)
      cn
    }
  }

  class Frozen(
    subtypeMap: mutable.LinkedHashMap[JType.Cls, List[JType.Cls]],
    supertypeMap: mutable.LinkedHashMap[JType.Cls, List[JType.Cls]],
    loadClassCache: mutable.LinkedHashMap[JType.Cls, Option[ClassNode]],
    loadMethodCache: mutable.LinkedHashMap[MethodSig, Option[MethodNode]]
  ) extends ClassManager {
    def getSubtypes(cls: JType.Cls) = subtypeMap.get(cls)
    def getSupertypes(cls: JType.Cls) = supertypeMap.get(cls)
    def loadClass(cls: JType.Cls) = loadClassCache.getOrElse(cls, None)
    def loadMethod(sig: MethodSig) = loadMethodCache.getOrElse(sig, None)
    def allClasses = loadClassCache.valuesIterator.flatten
  }

  class Dynamic(getClassFile: String => Option[ClassNode]) extends ClassManager {

    private val subtypeMap = mutable.LinkedHashMap.empty[JType.Cls, List[JType.Cls]]
    private val supertypeMap = mutable.LinkedHashMap.empty[JType.Cls, List[JType.Cls]]

    private val loadClassCache = mutable.LinkedHashMap.empty[JType.Cls, Option[ClassNode]]
    private val loadMethodCache = mutable.LinkedHashMap.empty[MethodSig, Option[MethodNode]]

    def getSubtypes(cls: JType.Cls) = subtypeMap.get(cls)
    def getSupertypes(cls: JType.Cls) = supertypeMap.get(cls)
    def loadClass(cls: JType.Cls): Option[ClassNode] = loadClassCache.getOrElseUpdate(
      cls,
      getClassFile(cls.name).map { cn =>
        val uppers = cn.interfaces.asScala ++ Option(cn.superName)
        val upperClasses = uppers.map(clsName => JType.Cls(clsName))
        for (up <- upperClasses) {
          subtypeMap(up) = cls :: subtypeMap.getOrElse(up, Nil)
          supertypeMap(cls) = up :: supertypeMap.getOrElse(cls, Nil)
          loadClass(up)
        }
        cn
      }
    )

    def loadMethod(sig: MethodSig): Option[MethodNode] = loadMethodCache.getOrElseUpdate(
      sig,
      loadClass(sig.cls).flatMap { cls =>
        cls
          .methods
          .iterator()
          .asScala
          .find(mn => mn.name == sig.name && mn.desc == sig.desc.render)
      }
    )

    def freeze() = new Frozen(subtypeMap, supertypeMap, loadClassCache, loadMethodCache)
  }

}
trait ClassManager {

  def getSubtypes(cls: JType.Cls): Option[List[JType.Cls]]
  def getSupertypes(cls: JType.Cls): Option[List[JType.Cls]]
  def loadClass(cls: JType.Cls): Option[ClassNode]
  def loadMethod(sig: MethodSig): Option[MethodNode]
  def getLinearSuperclasses(cls: JType.Cls): Seq[JType.Cls] = {
    def rec(currentCls: JType.Cls): List[JType.Cls] = {
      loadClass(currentCls) match {
        case None => Nil
        case Some(cls) => JType.Cls(cls.name) :: rec(JType.Cls(cls.superName))
      }
    }
    rec(cls)
  }

  def getAllSupertypes(current0: JType.Cls): Seq[JType.Cls] = {
    loadClass(current0)
    val supers = getSupertypes(current0) match {
      case None => Nil
      case Some(sup) => sup.flatMap(getAllSupertypes)
    }
    Seq(current0) ++ supers
  }

  def resolvePossibleSigs(sig: MethodSig): Option[Seq[MethodSig]] = {
    def superDefiner =
      getLinearSuperclasses(sig.cls)
        .iterator
        .map(c => sig.copy(cls = c))
        .find(loadMethod(_).nonEmpty)

    if (sig.static) {
      if (sig.name == "<clinit>") Some(Seq(sig))
      else superDefiner.map(Seq(_))
    } else {
      Some(
        superDefiner.toSeq ++
        getAllSubtypes(sig.cls).map(c => sig.copy(cls = c))
      )
    }
  }

  def getAllSubtypes(cls: JType.Cls): Seq[JType.Cls] = {
    Seq(cls) ++ getSubtypes(cls).getOrElse(Nil).flatMap(getAllSubtypes)
  }

  def mergeClasses(classes: Seq[JType.Cls]) = {
    Util
      .leastUpperBound(classes.toSet) { cls =>
        loadClass(cls) match {
          case None => Nil
          case Some(cn) => (cn.interfaces.asScala ++ Option(cn.superName)).map(JType.Cls(_))
        }
      }
      .toSeq
  }
  def mergeTypes(itypes: Seq[IType]): Option[IType] = {
    val flattened = itypes.flatMap {
      case JType.Bottom => Nil
      case CType.Null => Nil
      case CType.Intersect(values) => values
      case j => Seq(j)
    }.distinct
    if (flattened.length == 1) Some(flattened.head)
    else if (flattened.length == 0) Some(JType.Bottom)
    else if (flattened.forall(_.widen == JType.Prim.V)) Some(JType.Prim.V)
    else if (flattened.forall(_.widen == JType.Prim.I)) Some(JType.Prim.I)
    else if (flattened.forall(_.widen == JType.Prim.F)) Some(JType.Prim.F)
    else if (flattened.forall(_.widen == JType.Prim.J)) Some(JType.Prim.J)
    else if (flattened.forall(_.widen == JType.Prim.D)) Some(JType.Prim.D)
    else if (flattened.forall(x => x.widen == JType.Prim.Z || x.getClass == classOf[CType.I]))
      Some(JType.Prim.Z)
    else if (flattened.forall(x => x.widen == JType.Prim.B || x.getClass == classOf[CType.I]))
      Some(JType.Prim.B)
    else if (flattened.forall(x => x.widen == JType.Prim.C || x.getClass == classOf[CType.I]))
      Some(JType.Prim.C)
    else if (flattened.forall(x => x.widen == JType.Prim.S || x.getClass == classOf[CType.I]))
      Some(JType.Prim.S)
    else if (flattened.forall(_.isInstanceOf[JType.Cls])) {
      mergeClasses(flattened.map(_.asInstanceOf[JType.Cls])) match {
        case Seq(single) => Some(single)
        case many => Some(CType.Intersect(many))
      }
    } else if (flattened.forall(_.isInstanceOf[JType.Arr])) {
      Some(
        mergeTypes(flattened.map { case a: JType.Arr => a.innerType }) match {
          case None => JType.Cls("java.lang.Object")
          case Some(merged) =>
            merged match {
              case j: JType => JType.Arr(j)
              case _ => JType.Arr(JType.Cls("java.lang.Object"))
            }
        }
      )
    } else if (flattened.forall(t => t.isInstanceOf[JType.Cls] || t.isInstanceOf[JType.Arr])) {
      Some(JType.Cls("java.lang.Object"))
    } else None
  }
}
