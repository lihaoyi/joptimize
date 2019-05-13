package test.inlining

object Chaining {
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def noneGet(a: Int): Int = {
    if (a > 100) None.get else a
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def someGet(a: Int): Int = {
    if (a % 2 == 0) Some(123).get else a
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def castSomeGet(a: Int): Int = {
    val res = if (a % 2 == 0) Some(1) else None

    if (res.isEmpty) -1 else res.asInstanceOf[Some[Int]].get
  }

  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def get(a: Int): Int = {
    val res = if (a % 2 == 0) Some(1) else None

    if (res.isEmpty) -1 else res.get
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def map(a: Int): Int = {
    val xOpt = if (a == 1) Some(1) else None

    val res = xOpt.map(x => x + a)

    if (res.isEmpty) -1 else res.get
  }

  class NamedFunction(a: Int) extends Function1[Integer, Integer]{
    def apply(x: Integer) = Integer.valueOf(x.intValue() + a)
  }

  @test.Test(
    inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    checkClassRemoved = Array("Chaining$NamedFunction")
  )
  def mapInnerClass(a: Int): Int = {

    val res = None.map(new NamedFunction(a))

    if (res.isEmpty) -1 else 1
  }

  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def flatMap(a: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None

    val res = xOpt.flatMap(x => Some(x + 1))

    if (res.isEmpty) -1 else res.get
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def flatMapMap(a: Int, b: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None
    val yOpt = if (b % 3 == 0) Some(1) else None

    val res = xOpt.flatMap(x => yOpt.map(y => x + y))

    if (res.isEmpty) -1 else res.get
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def mapTwice(a: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None

    val res1 = xOpt.map(_ + 1)
    val res2 = xOpt.map(_ + 2)

    if (res1.isEmpty && res2.isEmpty) 1 else 0
  }

  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def mapTwice2(a: Int, b: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None
    val yOpt = if (b % 3 == 0) Some(1) else None

    val res1 = xOpt.map(x => x + 1)
    val res2 = yOpt.map(y => y + 2)

    (res1.isEmpty, res2.isEmpty) match{
      case (true, true) => -1
      case (false, true) => res1.get
      case (true, false) => res2.get
      case (false, false) => res1.get + res2.get
    }
  }

  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def flatMapMapChain(a: Int, b: Int, c: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None
    val yOpt = if (b % 2 == 0) Some(2) else None
    val zOpt = if (c % 2 == 0) Some(3) else None
    val res = for{
      x <- xOpt
      y <- yOpt
      z <- zOpt
    } yield x + y + z

    if (res.isEmpty) -1 else res.get
  }
}