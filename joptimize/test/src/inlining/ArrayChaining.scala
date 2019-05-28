package test.inlining


object ArrayChaining {
  @test.Test(inputs = Array(1, 2, 3))
  def manualIterator4(n: Int): Int = {
    val box = Array(0)
    Array(1, 2, 3, 4).foreach{ n =>
      box(0) += n
    }
    box(0)
  }
}
