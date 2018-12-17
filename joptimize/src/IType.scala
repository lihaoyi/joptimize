package joptimize

import org.objectweb.asm.tree.analysis.Value

/**
  * Represents an inferred type in the joptimize type system.
  *
  * Can be a concrete JType, an intersection type, or a concrete value
  */
trait IType extends Value{
  def size: Int
  def isRef: Boolean
  def internalName: String
  def getSize: Int = size

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
    def isRef = true
    def widen = this
  }
  case class I(value: Int) extends IType{
    def size = 1
    def isRef = false
    def internalName = s"TI$value;"
    def widen = JType.Prim.I
  }
  case class J(value: Long) extends IType{
    def size = 2
    def isRef = false
    def internalName = s"TJ$value;"
    def widen = JType.Prim.J
  }
  case class F(value: Float) extends IType{
    def size = 1
    def isRef = false
    def internalName = s"TF$value;"
    def widen = JType.Prim.F
  }
  case class D(value: Double) extends IType{
    def size = 2
    def isRef = false
    def internalName = s"TD$value;"
    def widen = JType.Prim.D
  }
}
