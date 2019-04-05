package joptimize.algorithms

import joptimize.model.SSA

import scala.collection.mutable

abstract class ClickScheduler(dominators: Dominator.Result[SSA.Block],
                              mapping: Map[SSA.Node, String]) {
  def downstream(ssa: SSA.Node): Seq[SSA.Node]
  def upstream(ssa: SSA.Node): Seq[SSA.Val]
  def isPinned(ssa: SSA.Node): Boolean
  def loopNest(block: SSA.Node): Int
  val visited = mutable.Map[SSA.Val, Unit]()

  val block = mutable.LinkedHashMap.empty[SSA.Val, SSA.Block]

  def scheduleEarlyRoot(n: SSA.Node): Unit = {
    for(in <- upstream(n)){
      if (!block.contains(in)) scheduleEarly(in)
    }
  }

  def scheduleEarly(n: SSA.Val): Unit = {
    assert(!n.isInstanceOf[SSA.Phi])
    scheduleEarlyRoot(n)
    block(n) = upstream(n).map(block).maxBy(dominators.dominatorDepth)
  }

  def scheduleLateRoot(n: SSA.Node): Unit = {
    downstream(n).foreach{
      case v: SSA.Val if !isPinned(v) => scheduleLate(v)
      case _ => //do nothing
    }
  }

  def scheduleLate(n: SSA.Val): Unit = {
    if (!visited.contains(n) && !n.isInstanceOf[SSA.Phi]){
      visited.put(n, ())
      scheduleLateRoot(n)
      if (!isPinned(n)){
        var lca: SSA.Block = null

        for(out <- downstream(n)){
          out match{
            case phi: SSA.Phi =>
              for((block, value) <- phi.incoming){
                if (value eq n) lca = findLca(lca, block)
              }
            case c: SSA.SimpleBlock => lca = findLca(lca, c.block)
            case c: SSA.Jump => lca = findLca(lca, c.block)
            case v: SSA.Val => lca = findLca(lca, block(v))
          }
        }

        var best = lca
        while(lca != block(n)){
          if (loopNest(lca) < loopNest(best)) best = lca
          lca = dominators.immediateDominators(lca)
        }
        block(n) = best

      }
    }
  }
  def findLca(a0: SSA.Block, b0: SSA.Block): SSA.Block = {
    if (a0 == null) b0
    else{
      var a = a0
      var b = b0
      while(dominators.dominatorDepth(a) > dominators.dominatorDepth(b)) {
        a = dominators.immediateDominators(a)
      }
      while(dominators.dominatorDepth(b) > dominators.dominatorDepth(a)) {
        b = dominators.immediateDominators(b)
      }
      while(a != b){
        a = dominators.immediateDominators(a)
        b = dominators.immediateDominators(b)
      }
      a
    }
  }
}
