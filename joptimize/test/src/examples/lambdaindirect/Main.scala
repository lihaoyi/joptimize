package joptimize.examples.lambdaindirect

object Main {
  def main(x: Int, y: Int): Int = {
    foo(x, _ + y) + foo(y, _ * x)
    // (x + y) + (y * x)
  }
  def foo(n: Int, f: Int => Int): Int = bar(n, f)
  def bar(n: Int, f: Int => Int): Int = f(n)
}
