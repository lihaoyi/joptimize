package joptimize

import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.analysis.Value

import scala.collection.mutable

trait Frameable[T <: Frameable[T]] extends Value{
  def widen: T
  def isEmpty: Boolean
  def internalName: String

}

/**
  * Models an abstract or concrete value. Unlike a simple [[IType]], a [[LValue]]
  * also tracks the provenance of this value: what instruction computed it, which
  * other values it was computed from, any dataflow merges. This allows you to
  * analyze the dataflow of LValues to figure out exactly how this value was
  * computed
  */
class LValue(val tpe: IType,
             val insn: Either[Int, AbstractInsnNode],
             val upstream: Seq[LValue],
             val merges: mutable.Buffer[LValue]) extends Frameable[LValue]{
  def getSize = tpe.size
  def widen = new LValue(tpe.widen, insn, upstream, merges)
  def isEmpty = tpe.isEmpty
  def internalName = tpe.internalName

  /**
    * Merge another LValue as an upstream dependency of this one.
    *
    * Note that merging is asymmetric, and the `other` is is kept in `this`'s
    * `merges` list, but not the other way around.
    */
  def mergeIntoThis(other: LValue) = {
    assert(tpe == other.tpe)
    merges += other
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
  def isConstant: Boolean
}

object IType{
  case class Intersect(classes: Seq[JType.Cls]) extends IType{
    def size = 1
    def internalName: String = "?"
    def name = s"R${classes.length}${classes.map(_.name.replace('/', '_')).mkString("__")}"
    def isRef = true
    def widen = this
    def isConstant = false
  }
  trait Constant extends IType{
    def isConstant = true
  }
  case class I(value: Int) extends Constant{
    def size = 1
    def isRef = false
    def internalName = s"TI$value;"
    def name = s"TI$value"
    def widen = JType.Prim.I
  }
  case class J(value: Long) extends Constant{
    def size = 2
    def isRef = false
    def internalName = s"TJ$value;"
    def name = s"TJ$value"
    def widen = JType.Prim.J
  }
  case class F(value: Float) extends Constant{
    def size = 1
    def isRef = false
    def internalName = s"TF$value;"
    def name = s"TF$value"
    def widen = JType.Prim.F
  }
  case class D(value: Double) extends Constant{
    def size = 2
    def isRef = false
    def internalName = s"TD$value;"
    def name = s"TD$value"
    def widen = JType.Prim.D
  }
}
