package joptimize.examples.opt

object BooleanWidening{
  def simple(b: Boolean): Int = {
    if(invert(b)) 1 else 2
  }
  def invert(b: Boolean): Boolean = {
    !b
  }
}
