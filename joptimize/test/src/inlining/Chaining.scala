package test.inlining

object Chaining {
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello0(a: Int): Int = {
    if (a > 100) None.get else a
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello1(a: Int): Int = {
    val res = if (a % 2 == 0) Some(1) else None

    if (res.isEmpty) -1 else res.asInstanceOf[Some[Int]].get
  }

  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello2(a: Int): Int = {
    val res = if (a % 2 == 0) Some(1) else None

    if (res.isEmpty) -1 else res.get
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello3(a: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None

    val res = xOpt.map(x => x + 1)

    if (res.isEmpty) -1 else res.get
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello4(a: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None

    val res = xOpt.flatMap(x => Some(x + 1))

    if (res.isEmpty) -1 else res.get
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello5(a: Int, b: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None
    val yOpt = if (b % 3 == 0) Some(1) else None

    val res = xOpt.flatMap(x => yOpt.map(y => x + y))

    if (res.isEmpty) -1 else res.get
  }
  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello6(a: Int): Int = {
    val xOpt = if (a % 2 == 0) Some(1) else None

    val res1 = xOpt.map(_ + 1)
    val res2 = xOpt.map(_ + 2)

    if (res1.isEmpty && res2.isEmpty) 1 else 0
  }

  @test.Test(inputs = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  def hello7(a: Int, b: Int): Int = {
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
  def hello8(a: Int, b: Int, c: Int): Int = {
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