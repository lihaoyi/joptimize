package examples.opt

object BooleanPropagation {
  def simple(x: Int, y: Int): Int = {
    val b = true
    val b1 = !b
    if (b1) call1(x)
    else call2(y)
  }
  def invert(b: Boolean): Boolean = !b
  def call1(x: Int): Int = x + 1
  def call2(y: Int): Int = y + 2
}
