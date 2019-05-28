package test.scalalib

object Small {

  object Test {
    val x = new Array[Test](0)
  }
  class Test

  @test.Test()
  def createArrayInStaticInitializer1(): String = {
    Test.toString.substring(0, Test.toString.indexOf('@'))
  }


  object TestCls {
    new Array[TestCls](1)
  }

  final class TestCls() extends TestTrait

  trait TestTrait

  @test.Test()
  def createArrayInStaticInitializer2(): String = {
    TestCls.toString.substring(0, TestCls.toString.indexOf('@'))
  }

  object TestCls3 {
    new Array[TestCls3](1)
  }

  class TestCls3() extends TestAbstractCls3

  abstract class TestAbstractCls3

  @test.Test()
  def createArrayInStaticInitializer3(): String = {
    TestCls3.toString.substring(0, TestCls3.toString.indexOf('@'))
  }

  object TestCls4 {
    new Array[Array[TestCls3]](1)
  }

  class TestCls4() extends TestAbstractCls4

  abstract class TestAbstractCls4

  @test.Test()
  def createNestedArrayInStaticInitializer(): String = {
    val s = TestCls4.toString
    s.substring(0, s.indexOf('@'))
  }


  @test.Test()
  def equals(): Int = {
    if (classOf[Int] == java.lang.Byte.TYPE) 1
    else 2
  }

  @test.Test(inputs = Array(0, 2, 4))
  def arraycopy(a: Int): Array[Int] = {
    val elems = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    if (elems.getClass.isArray) {
      java.lang.System.arraycopy(elems, 0, elems, 5, a)
    }
    elems
  }




  @test.Test(inputs = Array(0, 2, 4))
  def arraysCopyOf(a: Int): Array[String] = {
    java.util.Arrays.copyOf[String](new Array[String](a), 0)
  }

  def hasNext: Boolean = false

  @test.Test()
  def loopAtStartOfMethod(): Int = {
    while (hasNext) identity(123)
    456
  }

}
