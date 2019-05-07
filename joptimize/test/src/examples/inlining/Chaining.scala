package joptimize.examples.inlining

object Chaining {
  @joptimize.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello(a: Int, b: Int, c: Int) = {
    val xOpt = if (a % 2 == 0) Some(1) else None
    val yOpt = if (b % 2 == 0) Some(2) else None
    val zOpt = if (c % 2 == 0) Some(3) else None
    val res = for{
      x <- xOpt
      y <- yOpt
      z <- zOpt
    } yield x + y + z

    res.getOrElse(-1)
  }
}