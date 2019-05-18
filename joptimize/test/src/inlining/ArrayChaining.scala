package test.inlining

object ArrayChaining {

  @test.Test()
  def equals(): Int = {
    if (classOf[Int] == java.lang.Byte.TYPE) 1
    else 2
  }

  @test.Test(inputs = Array(0, 2, 4))
  def builder(a: Int): Array[Int] = {
    val elems = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    if (elems.getClass.isArray) {
      java.lang.System.arraycopy(elems, 0, elems, 5, a)
    }
    elems
  }
}