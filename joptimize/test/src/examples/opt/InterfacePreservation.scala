package joptimize.examples.opt

object InterfacePreservation {
  def shallow(i: Int) = new Bar().run(i)
  def deep(i: Int) = new Qux().run(i)

}


trait Foo
class Bar() extends Foo{
  def run(i: Int) = i + 1
}
trait Baz extends Foo
class Qux() extends Baz{
  def run(i: Int) = i + 2
}