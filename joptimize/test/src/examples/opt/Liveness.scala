package joptimize.examples.opt

object Liveness {
  def main(i: Int): Int = {
    pureButNotConstant(i)
    i + 1
  }
  def pureButNotConstant(i: Int) = i - 1

}
