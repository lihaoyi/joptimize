package joptimize.model

/**
  * Represents an inferred type in the joptimize type system.
  *
  * Can be a concrete JType, an intersection type, or a concrete value
  */
trait IType extends java.io.Serializable{
  def name: String
}
