package joptimize.analysis

import joptimize.model.SSA

import scala.collection.mutable

abstract class Scheduler(dominatorDepth: Map[SSA.Control, Int],
                         immediateDominator: Map[SSA.Control, SSA.Control],
                         phiMerges:  Map[SSA.Phi, (SSA.Control, Set[(SSA.Control, SSA)])],
                         mapping: Map[SSA.Control, String]) {
  def downstream(ssa: SSA.Token): Seq[SSA.Token]
  def upstream(ssa: SSA.Token): Seq[SSA.Token]
  def isPinned(ssa: SSA.Token): Boolean
  def loopNest(block: SSA.Token): Int
  val visited = new java.util.IdentityHashMap[SSA.Token, Unit]()

  val control = mutable.Map.empty[SSA.Token, SSA.Control]

  def scheduleEarly(n: SSA.Token): Unit = {
    for(in <- upstream(n)){
      if (!control.contains(in)) scheduleEarly(in)
    }

    if (!control.contains(n)){
      val b = upstream(n).flatMap(control.get).minBy(dominatorDepth)
      control(n) = b
    }
  }
  def scheduleLate(n: SSA.Token): Unit = {
    if (!visited.containsKey(n)){
      visited.put(n, ())
      for(out <- downstream(n)){
        if (!isPinned(out)) scheduleLate(out)
      }
      if (!isPinned(n)){
        var lca: SSA.Control = null
        for(out <- downstream(n)){

          out match{
            case phi: SSA.Phi =>
              for((ctrl, value) <- phiMerges(phi)._2){
                if (value eq n) lca = findLca(lca, ctrl)
              }
            case _ =>
              val outb = control(out)
              lca = findLca(lca, outb)
          }
        }
        var best = lca
        while(lca != control(n)){
          if (loopNest(lca) < loopNest(best)) best = lca
          lca = immediateDominator(lca)
        }
        control(n) = best

      }
    }
  }
  def findLca(a0: SSA.Control, b0: SSA.Control): SSA.Control = {
    if (a0 == null) b0
    else{
      var a = a0
      var b = b0
      while(dominatorDepth(a) > dominatorDepth(b)) a = immediateDominator(a)
      while(dominatorDepth(b) > dominatorDepth(a)) b = immediateDominator(b)
      while(a != b){
        a = immediateDominator(a)
        b = immediateDominator(b)
      }
      a
    }
  }
}
