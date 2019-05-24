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
    val s = TestCls4.toString
    s.substring(0, s.indexOf('@'))
  }

  object TestManifestFactory {
    val Nothing = new PhantomManifest()
    class PhantomManifest() extends ClassTypeManifest(None)
    class ClassTypeManifest(prefix: Option[Int])
  }

  @test.Test()
  def testManifestFactory(): String = {
    val s = TestManifestFactory.toString
    s.substring(0, s.indexOf('@'))
  }


  @test.Test()
  def manifestFactory(): String = {
    val s = scala.reflect.ManifestFactory.toString
    s.substring(0, s.indexOf('@'))
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

  class TestArraySeq[T](inner: Array[T]){
    def foreach[V](f: T => V) = {
      var i = 0
      while (i < inner.length){
        f(inner(i))
        i += 1
      }
    }
  }
  @test.Test()
  def foreach2(): Array[Int] = {
    val holder = Array(1)
    val arr = new TestArraySeq[String](Array("a", "bb", "CCC"))
    arr.foreach(x => holder(0) += x.length)

    holder
  }


  abstract class TestFunc[-T1, +R] {
    def apply(v1: T1): R
  }
  class TestFuncOne() extends  TestFunc[String, Integer]{
    def apply(v1: String): Integer = Integer.valueOf(v1.length)
  }
  class TestFuncTwo() extends TestFunc[Integer, String]{
    def apply(v1: Integer): String = v1.toString
  }
  @test.Test()
  def foreach3(): Integer = {


    val f1 = new TestFuncOne()
    val f2 = new TestFuncTwo()

    Integer.valueOf(f1("abcd").intValue() + f2(Integer.valueOf(Integer.SIZE)).length)
  }

  abstract class TestFunction[-T1, +R] {
    def apply(v1: T1): R
  }
  class TestFunctionOne() extends TestFunction[Integer, String]{
    def apply(v1: Integer): String = v1.toString
  }
  class TestFunctionTwo() extends TestFunction[String, Integer]{
    def apply(v1: String): Integer = Integer.valueOf(v1.length)
  }
  @test.Test()
  def foreach4(): Integer = {
    val f1 = new TestFunctionOne()
    val f2: TestFunction[String, Integer] = new TestFunctionTwo()
//    val f2 = new TestFunctionTwo()

    f2("abcd")
  }

  @test.Test(inputs = Array(0, 1, 2, 3))
  def foreach5(n: Int): Int = {
    try {
      if (n < 10) return 123
      else return 456 / n
    } catch { case e: Throwable =>
      return 789
    }
  }

  @test.Test()
  def foreach6(): Int = {
    lazy val inner =  1337
    inner
  }


  def thrower(): Any = throw new Exception("boom")

  @test.Test(inputs = Array(1, 2, 3))
  def foreach7(n: Int): Int = {
    if (n == 0) thrower().toString.length
    else 123
  }

  def identity[T](f: T): T= f

  @test.Test(inputs = Array(2, 3, 4))
  def foreach8(n: Int): Int = {
    if (n == 0) identity(thrower()).toString.length
    else if (n == 1) {
      val m = n + n
      val s = identity(thrower()).toString
      identity(s).length + s.length + m
    }
    else 123
  }

  def hasNext: Boolean = false

  @test.Test()
  def foreach9(): Int = {
    while (hasNext) identity(123)
    456
  }


  trait TestIter[+A]{
    def hasNext: Boolean
    def next(): A
    def foreach[U](f: A => U) = { while (hasNext) f(next()) }
  }
  class SubTestIter() extends TestIter[Nothing]{
    def hasNext: Boolean = false
    def next(): Nothing = throw new NoSuchElementException("next on empty iterator")
  }

  @test.Test()
  def foreach10(): Array[Int] = {
    val iterator = new SubTestIter()
    val holder = Array(1)
    iterator.foreach(x => holder(0) = x)
    holder
  }
}
