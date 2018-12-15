package joptimize.examples.narrow

object Supertype{
  def main(x: Int, y: Int): Int = {
    call(Bar, x) + call(Qux, y)
    // (x + 1) + (y + 2)
  }

  def call(f: Foo, n: Int): Int = f.inc(n)
}