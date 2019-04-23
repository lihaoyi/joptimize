package joptimize.model

/**
  * Represents an inferred type in the joptimize type system.
  *
  * Can be a concrete JType, an intersection type, or a concrete value
  */
trait IType extends java.io.Serializable{

  def name: String

  /**
    * Forget any information about concrete values encoded in this type, and
    * widen it into a type as represented purely by JVM classes
    */
  def widen: IType
  def isConstant: Boolean
}
