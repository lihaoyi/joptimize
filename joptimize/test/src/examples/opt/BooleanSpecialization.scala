package joptimize.examples.opt

object BooleanSpecialization{
  def simple(b: Boolean): Int = {
    if (b) call(true)
    else call(false)
  }
  def call(b: Boolean): Int = {
    if (b) 1 else 2
  }
}
