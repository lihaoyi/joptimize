package joptimize.examples.narrow

object Parametric {
  def main(x: Int, y: Int): Int = {
    call(Bar, x) + call(Qux, y)
    // (x + 1) + (y + 2)
  }

  def call[T <: Foo](f: T, n: Int): Int = f.inc(n)
}
