package joptimize.examples.opt

object Liveness {
  def main(i: Int): Int = {
    pureButNotConstant(i)
    i + 1
  }
  def pureButNotConstant(i: Int) = i - 1
  def pureButNotConstant2(i: Int) = i + 1


  def main2a(i: Int): Int = {
    terminal(true, pureButNotConstant(i), pureButNotConstant2(i))
  }

  def main2b(i: Int): Int = {
    terminal(false, pureButNotConstant(i), pureButNotConstant2(i))
  }

  def terminal(b: Boolean, i: Int, j: Int) = if (b) i else j

}
