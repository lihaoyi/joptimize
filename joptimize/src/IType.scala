package joptimize

import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.analysis.Value

import scala.collection.mutable

trait Frameable[T <: Frameable[T]] extends Value{
  def widen: T
  def isEmpty: Boolean
  def internalName: String

}
class LValue(val tpe: IType,
             val insn: Either[Int, AbstractInsnNode],
             val upstream: mutable.Buffer[Seq[LValue]]) extends Frameable[LValue]{
  def getSize = tpe.size
  def widen = new LValue(tpe.widen, insn, upstream)
  def isEmpty = tpe.isEmpty
  def internalName = tpe.internalName

  def mergeIntoThis(other: LValue) = {
    assert(tpe == other.tpe)
    upstream ++= other.upstream
    this
  }

  override def toString = s"LValue@${Integer.toHexString(System.identityHashCode(this))}($tpe, $insn)"
}
/**
  * Represents an inferred type in the joptimize type system.
  *
  * Can be a concrete JType, an intersection type, or a concrete value
  */
trait IType extends Frameable[IType]{
  def size: Int
  def isRef: Boolean
  def internalName: String
  def getSize: Int = size
  def name: String

  def isEmpty = this == JType.Null
  /**
    * Forget any information about concrete values encoded in this type, and
    * widen it into a type as represented purely by JVM classes
    */
  def widen: IType
}

object IType{
  case class Intersect(classes: Seq[JType.Cls]) extends IType{
    def size = 1
    def internalName: String = "?"
    def name = s"R${classes.length}${classes.map(_.name.replace('/', '_')).mkString("__")}"
    def isRef = true
    def widen = this
  }
  case class I(value: Int) extends IType{
    def size = 1
    def isRef = false
    def internalName = s"TI$value;"
    def name = s"TI$value"
    def widen = JType.Prim.I
  }
  case class J(value: Long) extends IType{
    def size = 2
    def isRef = false
    def internalName = s"TJ$value;"
    def name = s"TJ$value"
    def widen = JType.Prim.J
  }
  case class F(value: Float) extends IType{
    def size = 1
    def isRef = false
    def internalName = s"TF$value;"
    def name = s"TF$value"
    def widen = JType.Prim.F
  }
  case class D(value: Double) extends IType{
    def size = 2
    def isRef = false
    def internalName = s"TD$value;"
    def name = s"TD$value"
    def widen = JType.Prim.D
  }
}
