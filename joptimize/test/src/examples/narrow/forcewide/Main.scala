package joptimize.examples.narrow.forcewide

object Main {
  val f1: Foo = Bar
  val f2: Foo = Qux
  def main(x: Int, y: Int): Int = {
    call(f1, x) + call(f2, y)
    // (x + 1) + (y + 2)
  }

  def call(f: Foo, n: Int): Int = f.inc(n)
}

trait Foo{
  def inc(n: Int): Int
}
object Bar extends Foo{
  def inc(n: Int) = n + 1
}
object Qux extends Foo{
  def inc(n: Int) = n + 2
}