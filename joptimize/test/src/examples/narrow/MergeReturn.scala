package joptimize.examples.narrow

object MergeReturn {
  def main(x: Int, y: Int): Int = {
    if (call(x, y) == Bar) 1 else 2
  }

  def call(x: Int, y: Int): Foo = {
    if (x > y) Bar
    else Qux
  }
}
