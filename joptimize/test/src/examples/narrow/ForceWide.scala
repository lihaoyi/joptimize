package joptimize.examples.narrow

object ForceWide{
  val f1: Foo = Bar
  val f2: Foo = Qux
  def main(x: Int, y: Int): Int = {
    call(f1, x) + call(f2, y)
    // (x + 1) + (y + 2)
  }

  def call(f: Foo, n: Int): Int = f.inc(n)
}

