package joptimize.examples.opt

object SimpleDce {
  def main(x: Int, y: Int): Int = call1(x) + call2(y)
  def call1(x: Int): Int = x + 1
  def call2(y: Int): Int = y + 2
  def call3(z: Int): Int = z + 2
}
