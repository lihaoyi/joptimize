package joptimize.examples.supertype

object Main {
  def main(x: Int, y: Int): Int = {
    call(Bar, x) + call(Qux, y)
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