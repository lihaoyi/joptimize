package joptimize.examples.opt

object BooleanPropagation {
  def simple(x: Int, y: Int): Int = {
    call(true, x, y)
  }
  def call(b: Boolean, x: Int, y: Int) = {
    if (b) leaf1(x)
    else leaf2(y)
  }
  def leaf1(x: Int): Int = x + 1
  def leaf2(y: Int): Int = y + 2
}
