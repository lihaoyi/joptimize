package joptimize.analyzer

import joptimize.model._

trait Lattice[T] {
  def transferValue(node: SSA.Val, inferences: SSA.Val => T): T
  def join(lhs: T, rhs: T): T
}
