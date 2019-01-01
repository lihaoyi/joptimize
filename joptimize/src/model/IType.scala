package joptimize.model

import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.analysis.Value

import scala.collection.mutable

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
  def name: String

  /**
    * Forget any information about concrete values encoded in this type, and
    * widen it into a type as represented purely by JVM classes
    */
  def widen: IType
  def isConstant: Boolean
}
