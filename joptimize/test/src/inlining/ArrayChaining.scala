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
  def initArray(): String = {
    Test.toString.substring(0, Test.toString.indexOf('@'))
  }


  object TestCls {
    new Array[TestCls](1)
  }

  final class TestCls() extends TestTrait

  trait TestTrait

  @test.Test()
  def initArray2(): String = {
    TestCls.toString.substring(0, TestCls.toString.indexOf('@'))
  }

  object TestCls3 {
    new Array[TestCls3](1)
  }

  class TestCls3() extends TestAbstractCls3

  abstract class TestAbstractCls3

  @test.Test()
  def initArray3(): String = {
    TestCls3.toString.substring(0, TestCls3.toString.indexOf('@'))
  }

  object TestCls4 {
    new Array[Array[TestCls3]](1)
  }

  class TestCls4() extends TestAbstractCls4

  abstract class TestAbstractCls4

  @test.Test()
  def initArray4(): String = {
    TestCls4.toString.substring(0, TestCls3.toString.indexOf('@'))
  }

  @test.Test()
  def manifestFactory(): String = {
    scala.reflect.ManifestFactory.toString.substring(
      0,
      scala.reflect.ManifestFactory.toString.indexOf('@')
    )
  }

  @test.Test()
  def pkg(): String = {
    scala.Predef.int2Integer(123).toString
  }

  sealed abstract class TestList[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: TestList[A]

    def foreach[U](f: A => U) {
      var these = this
      while (!these.isEmpty) {
        f(these.head)
        these = these.tail
      }
    }
  }

  case object TestNil extends TestList[Nothing] {
    def isEmpty = true
    def head = throw new Exception()
    def tail = throw new Exception()
  }

  case class TestCons[B](head: B, tl: TestList[B]) extends TestList[B] {
    def tail = tl
    def isEmpty = false
  }

  @test.Test()
  def foreach(): Array[Int] = {
    val holder = Array(1)

    TestCons(1, TestCons(2, TestCons(3, TestNil))).foreach(x => holder(0) += x)
    TestCons(1L, TestCons(2L, TestCons(3L, TestNil))).foreach(x => holder(0) += x.toInt)
    holder
  }
}