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
}

object IType{
  case class Intersect(classes: Seq[JType.Cls]) extends IType{
    def size = 1
    def internalName: String = "?"
    def isRef = true
  }
}
