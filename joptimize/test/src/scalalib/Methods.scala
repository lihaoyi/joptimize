package test.scalalib

object Methods{


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
  def multipleSubtypesOfGeneric(): Integer = {


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
  def multipleTypesOfGeneric(): Integer = {
    val f1 = new TestFunctionOne()
    val f2: TestFunction[String, Integer] = new TestFunctionTwo()
    //    val f2 = new TestFunctionTwo()

    f2("abcd")
  }


  class Foo(repr: Int) extends Bar {
    def apply(index: Int): Int = repr
  }

  trait Bar extends Qux {
    def next(): Int = apply(0)
  }

  trait Qux  {
    def apply(idx: Int): Int
  }

  @test.Test(inputs = Array(2, 3, 4))
  def superDefineSubImplement(n: Int): Int = {
    val box = Array(0)
    val iter = new Foo(n)
    box(0) = iter.next()
    box(0)
  }


  abstract class FooX{
    def apply(): Top
  }
  abstract class BarX extends FooX{
    def apply(): Left
  }
  abstract class QuxX extends FooX{
    def apply(): Right
  }
  class BarX2 extends BarX{
    def apply() = new Left()
  }
  class QuxX2 extends QuxX{
    def apply() = new Right()
  }
  @test.Test(inputs = Array(1, 2, 3, 4))
  def narrowingAbstractMethod(n: Int): Int = {
    val bar: FooX = new BarX2()
    val qux = new QuxX2()
    bar.apply().num
  }




  abstract class Foo4(){
    def apply(): Top
  }

  class Qux4 extends Foo4(){
    def apply(): Top = new Right
  }

  abstract class Baz4() extends Foo4

  class Bar4() extends Baz4(){
    def apply(): Top = new Left
  }

  @test.Test(inputs = Array(1, 2, 3))
  def inferredHighestDefinerReturnDiffer(n: Int): Int = {
    val quxNum = new Qux4().apply().num
    val bar: Baz4 = new Bar4()
    val barNum = bar.apply().num
    quxNum + barNum
  }

  trait Foo5{
    def apply(): Top
  }

  class Qux5 extends Foo5{
    def apply(): Top = new Right
  }

  trait Baz5 extends Foo5

  class Bar5() extends Outer5() with Baz5

  class Outer5(){
    def apply(): Top = new Left
  }

  @test.Test(inputs = Array(1, 2, 3))
  def inheritFromOutsideHierarchy(n: Int): Int = {
    val bar: Baz5 = new Bar5()
    val barNum = bar.apply().num
    barNum
  }


  abstract class Top{
    def num: Int
  }
  class Left extends Top{
    def num = 123
  }
  class Right extends Top{
    def num = 456
  }

}
