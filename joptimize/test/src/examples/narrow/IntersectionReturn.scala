package joptimize.examples.narrow

object IntersectionReturn {
  def main(x: Int, y: Int): Int = {
    if (call(x, y) == Bar) 1 else 2
  }

  def call(x: Int, y: Int): Foo = {
    if (x > y) Bar
    else Qux
  }

  trait Foo{
    def inc(n: Int): Int
  }
  trait Foo1 extends Foo
  trait Foo2 extends Foo
  object Bar extends Foo1 with Foo2{
    def inc(n: Int) = n + 1
  }
  object Qux extends Foo1 with Foo2{
    def inc(n: Int) = n + 2
  }

}

