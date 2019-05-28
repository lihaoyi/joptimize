package test.scalalib

import scala.collection.AbstractIterator

object FauxLibrary{

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
  def simpleLinkedListForeach(): Array[Int] = {
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
  def simpleArraySeqForeach(): Array[Int] = {
    val holder = Array(1)
    val arr = new TestArraySeq[String](Array("a", "bb", "CCC"))
    arr.foreach(x => holder(0) += x.length)

    holder
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
  def minimizedIterator(): Array[Int] = {
    val iterator = new SubTestIter()
    val holder = Array(1)
    iterator.foreach(x => holder(0) = x)
    holder
  }



  class TestElements() extends TestIterator

  trait TestIterator {
    def run(f: TestCallback[Int, Int]): Int = f(123)
  }
  abstract class TestCallback[T, V]{
    def apply(x: T): V
  }
  class TestCallbackImpl extends TestCallback[Int, Int]{
    def apply(x: Int): Int = x + 1
  }

  @test.Test()
  def staticSpecialInterfaceMethods(): Int = {
    new TestElements().run(new TestCallbackImpl())
  }


  class Elements[T](arr: Array[T]) extends AbstractIterator[T] {
    val end = arr.length
    var index = 0

    def hasNext: Boolean = index < end

    def next(): T = {
      val x = arr(index)
      index += 1
      x
    }
  }

  @test.Test(inputs = Array(2, 3, 4))
  def manualIterator(n: Int): Int = {
    val iter = new Elements(Array(0, 1, 2, 3))
    iter.map(_ + 1).next()
  }

  @test.Test(inputs = Array(1, 2, 3, 4))
  def manualIterator2(n: Int): Int = {
    val box = Array(0)
    val iter = new Elements(Array(0, 1, 2, 3))
    iter.map(_ + n).foreach(x => box(0) += x)
    box(0)
  }


  class SingletonBigTestIterator[T](a: T) extends BigTestIterator[T] {
    var ready = true
    def hasNext: Boolean = ready
    def next(): T = {
      ready = false
      a
    }
  }

  trait BigTestIterator[+A] {
    def hasNext: Boolean

    def next(): A

    def filter(p: A => Boolean): BigTestIterator[A] = new FilterBigTestIterator[A](this, p)
  }

  class FilterBigTestIterator[A](parent: BigTestIterator[A], pred: A => Boolean) extends BigTestIterator [A]{
    private[this] var hd: A = _
    private[this] var hdDefined: Boolean = false

    def hasNext: Boolean = hdDefined || {
      do {
        if (!parent.hasNext) return false
        hd = parent.next()
      } while (!pred(hd))
      hdDefined = true
      true
    }

    def next() = {
      hdDefined = false; hd
    }
  }

  @test.Test(inputs = Array(1, 2, 3))
  def manualIterator3(n: Int): Int = {
    val iter = new SingletonBigTestIterator(n).filter(_ % 2 == 0)
    if (iter.hasNext) iter.next()
    else n
  }
}
