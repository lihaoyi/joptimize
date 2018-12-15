package joptimize.examples.narrow

trait Foo{
  def inc(n: Int): Int
}
object Bar extends Foo{
  def inc(n: Int) = n + 1
}
object Qux extends Foo{
  def inc(n: Int) = n + 2
}