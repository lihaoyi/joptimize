package joptimize.examples.narrow

object NarrowReturn {
  def main(x: Int, y: Int): Int = {
    call(wrap(Bar), x) + call(wrap(Qux), y)
  }

  def call(f: Foo, n: Int): Int = f.inc(n)

  def wrap(f: Foo): Foo = f
}
