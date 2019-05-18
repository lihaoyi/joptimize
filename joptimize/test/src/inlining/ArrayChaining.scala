package test.inlining

object ArrayChaining {

  @test.Test()
  def equals(): Int = {
    if (classOf[Int] == java.lang.Byte.TYPE) 1
    else 2
  }

  @test.Test(inputs = Array(0, 1, 2))
  def builder(a: Int): Array[Int] = {
    val elems = new Array[Int](0)
    val newelems = new Array[Int](0)
    if (a > 0) Array.copy(elems, 0, newelems, 0, a)
    newelems
  }
}