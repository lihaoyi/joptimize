package joptimize.examples.narrow.narrowreturn

object Main {
  def main(x: Int, y: Int): Int = {
    call(wrap(Bar), x) + call(wrap(Qux), y)
  }

  def call(f: Foo, n: Int): Int = f.inc(n)

  def wrap(f: Foo): Foo = f
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