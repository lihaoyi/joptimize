package joptimize.examples.opt

object ConstantMethod {
  def int(b: Boolean): Int = {
    if (b) callInt(true)
    else callInt(false)
  }
  def callInt(b: Boolean): Int = {
    if (b) 1 else 2
  }

  def bool(b: Boolean): Boolean = {
    if (b) callBool(true)
    else callBool(false)
  }
  def callBool(b: Boolean): Boolean = {
    if (b) !b else !b
  }

  def impure(b: Boolean): Boolean = {
    if (b) callImpure(true)
    else callImpure(false)
  }
  def callImpure(b: Boolean): Boolean = {
    System.currentTimeMillis()
    if (b) !b else !b
  }
}