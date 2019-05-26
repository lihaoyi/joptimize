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

    if (sig.static) {
      if (sig.name == "<clinit>") Some(Seq(sig))
      else getLinearSuperclasses(sig.cls)
        .iterator
        .map(c => sig.copy(cls = c))
        .find(loadMethod(_).nonEmpty)
        .map(Seq(_))
    } else {
      val superDefiner = getAllSupertypes(sig.cls)
        .iterator
        .map(c => sig.copy(cls = c))
        .find(loadMethod(_).nonEmpty)
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
  def mergeTypes0(lhs: IType, rhs: IType): Option[IType] = {
    if (lhs == rhs) Some(lhs)
    else (lhs, rhs) match{
      case (lhs: JType.Cls, rhs: JType.Cls) =>
        mergeClasses(Seq(lhs, rhs)) match{
          case Seq(merged) => Some(merged)
          case _ => Some(JType.Cls("java/lang/Object"))
        }
      case (lhs: JType.Arr, rhs: JType.Arr) =>
        mergeTypes0(lhs.innerType, rhs.innerType) match{
          case Some(mergedInner: JType) => Some(JType.Arr(mergedInner))
          case _ => Some(JType.Cls("java/lang/Object"))
        }

      case (_: JType.Cls, _: JType.Arr) => Some(JType.Cls("java/lang/Object"))
      case (_: JType.Arr, _: JType.Cls) => Some(JType.Cls("java/lang/Object"))

      case (JType.Bottom | CType.Null, rhs) => Some(rhs)
      case (lhs, JType.Bottom | CType.Null) => Some(lhs)

      case (JType.Prim.Z, JType.Prim.I) => Some(JType.Prim.Z)
      case (JType.Prim.Z, CType.I(_)) => Some(JType.Prim.Z)
      case (JType.Prim.I, JType.Prim.Z) => Some(JType.Prim.Z)
      case (CType.I(_), JType.Prim.Z) => Some(JType.Prim.Z)

      case (JType.Prim.B, JType.Prim.I) => Some(JType.Prim.B)
      case (JType.Prim.B, CType.I(_)) => Some(JType.Prim.B)
      case (JType.Prim.I, JType.Prim.B) => Some(JType.Prim.B)
      case (CType.I(_), JType.Prim.B) => Some(JType.Prim.B)

      case (JType.Prim.C, JType.Prim.I) => Some(JType.Prim.C)
      case (JType.Prim.C, CType.I(_)) => Some(JType.Prim.C)
      case (JType.Prim.I, JType.Prim.C) => Some(JType.Prim.C)
      case (CType.I(_), JType.Prim.C) => Some(JType.Prim.C)

      case (JType.Prim.S, JType.Prim.I) => Some(JType.Prim.S)
      case (JType.Prim.S, CType.I(_)) => Some(JType.Prim.S)
      case (JType.Prim.I, JType.Prim.S) => Some(JType.Prim.S)
      case (CType.I(_), JType.Prim.S) => Some(JType.Prim.S)

      case (JType.Prim.I, CType.I(_)) => Some(JType.Prim.I)
      case (CType.I(_), JType.Prim.I) => Some(JType.Prim.I)
      case (CType.I(_), CType.I(_)) => Some(JType.Prim.I)

      case (JType.Prim.J, CType.J(_)) => Some(JType.Prim.J)
      case (CType.J(_), JType.Prim.J) => Some(JType.Prim.J)
      case (CType.J(_), CType.J(_)) => Some(JType.Prim.J)

      case (JType.Prim.F, CType.F(_)) => Some(JType.Prim.F)
      case (CType.F(_), JType.Prim.F) => Some(JType.Prim.F)
      case (CType.F(_), CType.F(_)) => Some(JType.Prim.F)

      case (JType.Prim.D, CType.D(_)) => Some(JType.Prim.D)
      case (CType.D(_), JType.Prim.D) => Some(JType.Prim.D)
      case (CType.D(_), CType.D(_)) => Some(JType.Prim.D)

      case _ => None
    }
  }

  def mergeTypes(lhs: IType, rhs: IType): IType = {
    mergeTypes0(lhs, rhs).getOrElse(throw new Exception(s"Unable to merge $lhs $rhs"))
  }

  def mergeTypes(itypes: Seq[IType]): IType = {
    if (itypes.isEmpty) JType.Bottom
    else itypes.tail.foldLeft[Option[IType]](Some(itypes.head)){
      case (None, _) => None
      case (Some(x), y) => mergeTypes0(x, y)
    }.getOrElse(throw new Exception("Unable to merge " + itypes.mkString(" ")))
  }
}
