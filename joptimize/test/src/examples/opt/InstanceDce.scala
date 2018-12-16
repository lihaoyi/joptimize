package joptimize.examples.opt

object InstanceDce {
  val unknown1: FooTwo = new BarTwo()
  val unknown2: FooTwo = new QuxTwo()
  def simple1(x: Int, y: Int): Int = new BarTwo().incA(x) + new QuxTwo().incB(y)
  def simple2(x: Int, y: Int): Int = new BarTwo().incA(x) + new BarTwo().incB(y)
  def simple3(x: Int, y: Int): Int = new QuxTwo().incA(x) + new QuxTwo().incB(y)

  def single1(x: Int, y: Int): Int = new BarTwo().incA(x) + new BarTwo().incA(y)
  def single2(x: Int, y: Int): Int = new BarTwo().incB(x) + new BarTwo().incB(y)
  def single3(x: Int, y: Int): Int = new QuxTwo().incA(x) + new QuxTwo().incA(y)
  def single4(x: Int, y: Int): Int = new QuxTwo().incB(x) + new QuxTwo().incB(y)

  def unknown1(x: Int, y: Int): Int = unknown1.incA(x) + unknown2.incA(y)
  def unknown2(x: Int, y: Int): Int = unknown1.incB(x) + unknown2.incB(y)
  def unknown3(x: Int, y: Int): Int = unknown1.incA(x) + unknown2.incB(y)

}
trait FooTwo{
  def incA(n: Int): Int
  def incB(n: Int): Int
}
class BarTwo extends FooTwo{
  def incA(n: Int) = n + 1
  def incB(n: Int) = n + 2
}
class QuxTwo extends FooTwo{
  def incA(n: Int) = n + 3
  def incB(n: Int) = n + 4
}