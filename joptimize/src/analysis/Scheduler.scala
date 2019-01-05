package joptimize.analysis

import joptimize.model.SSA

import scala.collection.mutable

abstract class Scheduler(dominatorDepth: Map[SSA.Token, Int],
                         immediateDominator: Map[SSA.Token, SSA.Token],
                         phiMerges:  Map[SSA.Phi, (SSA.Control, Set[(SSA.Control, SSA)])],
                         mapping: Map[SSA.Token, String]) {
  def downstream(ssa: SSA.Token): Seq[SSA.Token]
  def upstream(ssa: SSA.Token): Seq[SSA.Token]
  def isPinned(ssa: SSA.Token): Boolean
  def loopNest(block: SSA.Token): Int
  val visited = new java.util.IdentityHashMap[SSA.Token, Unit]()

  val control = mutable.Map.empty[SSA.Token, SSA.Token]

  def scheduleEarly(n: SSA.Token): Unit = {
    pprint.log(n)
    pprint.log(upstream(n))

    for(in <- upstream(n)){
      if (!control.contains(in)) scheduleEarly(in)
    }

    if (!control.contains(n) && upstream(n).nonEmpty){
      val b = upstream(n).flatMap(control.get).minBy(dominatorDepth)
      control(n) = b
    }
  }
  def scheduleLate(n: SSA.Token): Unit = {
    if (!visited.containsKey(n)){
      pprint.log(mapping.get(n))
      pprint.log(n)
      visited.put(n, ())
      for(out <- downstream(n)){
        if (!isPinned(out)) scheduleLate(out)
      }
      if (!isPinned(n)){
        var lca: SSA.Token = null
        for(out <- downstream(n) if !isPinned(out)){
//          pprint.log(out)

          out match{
            case phi: SSA.Phi =>
              for((ctrl, value) <- phiMerges(phi)._2){
                if (value eq n) lca = findLca(lca, ctrl)
              }
            case _ =>
              val outb: SSA.Token = control(out)
              lca = findLca(lca, outb)
          }
        }
        var best = lca
        if (control.contains(n)){
          while(lca ne control(n)){
            pprint.log(lca)
            pprint.log(best)
            if (loopNest(lca) < loopNest(best)) best = lca
            lca = immediateDominator(lca)
          }
        }
        control(n) = best

      }
    }
  }
  def findLca(a0: SSA.Token, b0: SSA.Token): SSA.Token = {
    if (a0 == null) b0
    else{
      var a: SSA.Token = a0
      var b: SSA.Token = b0
      while(dominatorDepth(a) < dominatorDepth(b)) a = immediateDominator(a)
      while(dominatorDepth(b) < dominatorDepth(a)) b = immediateDominator(b)
      while(a != b){
        a = immediateDominator(a)
        b = immediateDominator(b)
      }
      a
    }
  }
}
