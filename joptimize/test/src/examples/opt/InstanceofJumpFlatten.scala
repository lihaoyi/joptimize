package joptimize.examples.opt

object InstanceofJumpFlatten {
  def simpleBar(x: Int): Int = call(new InstanceImplA, x)

  def simpleBaz(x: Int): Int = call(new InstanceImplB, x)

  def simpleQux(x: Int): Int = call(new InstanceImplC, x)

  def call(b: InstanceTrait, x: Int) = {
    if (b.isInstanceOf[InstanceImplA]) leaf1(x)
    else if (b.isInstanceOf[InstanceImplB]) leaf2(x)
    else if (b.isInstanceOf[InstanceImplC]) leaf3(x)
    else 0
  }

  def leaf1(x: Int): Int = x + 1
  def leaf2(x: Int): Int = x + 2
  def leaf3(x: Int): Int = x + 3
}


trait InstanceTrait
class InstanceImplA extends InstanceTrait
class InstanceImplB extends InstanceTrait
class InstanceImplC extends InstanceTrait