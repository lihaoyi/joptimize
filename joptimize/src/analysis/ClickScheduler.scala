package joptimize.analysis

import joptimize.model.SSA

import scala.collection.mutable

abstract class ClickScheduler(dominatorDepth: Map[SSA.Ctrl, Int],
                              immediateDominator: Map[SSA.Ctrl, SSA.Ctrl],
                              phiMerges:  Map[SSA.Phi, (SSA.Ctrl, Set[(SSA.Ctrl, SSA.Val)])],
                              mapping: Map[SSA.Ctrl, String]) {
  def downstream(ssa: SSA.Node): Seq[SSA.Node]
  def upstream(ssa: SSA.Node): Seq[SSA.Val]
  def isPinned(ssa: SSA.Node): Boolean
  def loopNest(block: SSA.Node): Int
  val visited = new java.util.IdentityHashMap[SSA.Val, Unit]()

  val control = mutable.Map.empty[SSA.Val, SSA.Ctrl]

  def scheduleEarlyRoot(n: SSA.Node): Unit = {
    for(in <- upstream(n)){
      if (!control.contains(in)) scheduleEarly(in)
    }
  }

  def scheduleEarly(n: SSA.Val): Unit = {
    scheduleEarlyRoot(n)
    control(n) = upstream(n).map(control).minBy(dominatorDepth)
  }

  def scheduleLateRoot(n: SSA.Node): Unit = {
    downstream(n).foreach{
      case v: SSA.Val => if (!isPinned(v)) scheduleLate(v)
      case _ => //do nothing
    }
  }

  def scheduleLate(n: SSA.Val): Unit = {
    if (!visited.containsKey(n)){
      visited.put(n, ())
      scheduleLateRoot(n)
      if (!isPinned(n)){
        var lca: SSA.Ctrl = null
        for(out <- downstream(n)){

          out match{
            case phi: SSA.Phi =>
              for((ctrl, value) <- phiMerges(phi)._2){
                if (value eq n) lca = findLca(lca, ctrl)
              }
            case c: SSA.Ctrl => lca = findLca(lca, c)
            case v: SSA.Val => lca = findLca(lca, control(v))
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
  def findLca(a0: SSA.Ctrl, b0: SSA.Ctrl): SSA.Ctrl = {
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
