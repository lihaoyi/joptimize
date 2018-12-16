package joptimize.examples.opt

object BooleanWidening{
  def simple(x: Int, y: Int): Int = {
    if(invert(x > y)) x else y
  }
  def invert(b: Boolean): Boolean = {
    !b
  }
}
