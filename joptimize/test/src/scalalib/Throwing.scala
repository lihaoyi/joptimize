package test.scalalib

object Throwing {

  @test.Test(inputs = Array(0, 1, 2, 3))
  def tryCatch(n: Int): Int = {
    try {
      if (n < 10) return 123
      else return 456 / n
    } catch { case e: Throwable =>
      return 789
    }
  }

  @test.Test()
  def lazyVal(): Int = {
    lazy val inner =  1337
    inner
  }


  def thrower(): Any = throw new Exception("boom")

  @test.Test(inputs = Array(1, 2, 3))
  def throwingInBranch1(n: Int): Int = {
    if (n == 0) thrower().toString.length
    else 123
  }

  def identity[T](f: T): T = f

  @test.Test(inputs = Array(2, 3, 4))
  def throwingInBranch2(n: Int): Int = {
    if (n == 0) identity(thrower()).toString.length
    else if (n == 1) {
      val m = n + n
      val s = identity(thrower()).toString
      identity(s).length + s.length + m
    }
    else 123
  }
}
