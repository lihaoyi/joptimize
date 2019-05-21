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

  @test.Test(inputs = Array(0, 2, 4))
  def builder2(a: Int): Array[Int] = {
    new collection.mutable.ArrayBuilder.ofInt().result()
  }


  @test.Test(inputs = Array(0, 2, 4))
  def builder3(a: Int): Array[String] = {
    java.util.Arrays.copyOf[String](new Array[String](a), 0)
  }

  object Test {
    val x = new Array[Test](0)
  }
  class Test

  @test.Test()
  def initArray(): Int = {
    Test.toString.length
  }

  @test.Test(inputs = Array(0, 2, 4))
  def pkg(a: Int): Integer = {
    scala.Predef.int2Integer(a)
  }

  @test.Test(inputs = Array(0, 2, 4))
  def foreach(a: Int): Array[Int] = {
    val holder = Array(1)
    Array(1, 2, 3).foreach(x => holder(0) += x)
    holder
  }
}