package joptimize.model

object SideEffects{
  case object Pure extends SideEffects
  case class Args(indices: Seq[Int]) extends SideEffects
  case object Global extends SideEffects
}
sealed trait SideEffects
