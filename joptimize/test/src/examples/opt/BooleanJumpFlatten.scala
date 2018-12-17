package joptimize.examples.opt

object BooleanJumpFlatten {
  def simpleTrue(x: Int, y: Int): Int = {
    call(true, x, y)
  }
  def simpleFalse(x: Int, y: Int): Int = {
    call(false, x, y)
  }
  def call(b: Boolean, x: Int, y: Int) = {
    if (b) leaf1(x)
    else leaf2(y)
  }
  def leaf1(x: Int): Int = x + 1
  def leaf2(y: Int): Int = y + 2
}
