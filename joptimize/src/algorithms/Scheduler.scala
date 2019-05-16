package joptimize.algorithms

import joptimize.Logger
import joptimize.graph.HavlakLoopTree
import joptimize.model.SSA

import scala.collection.mutable

object Scheduler {
  def apply(loopTree: HavlakLoopTree.Loop[SSA.Block],
            dominators: Dominator.Result[SSA.Block],
            startBlock: SSA.Block,
            allVertices: Set[SSA.Node],
            log: Logger.InferredMethod): Map[SSA.ValOrState, SSA.Block] = {

    val loopNestMap = mutable.LinkedHashMap.empty[SSA.Node, Int]
    def recLoop(loop: HavlakLoopTree.Loop[SSA.Block], depth: Int): Unit = {
      loop.basicBlocks.foreach(loopNestMap(_) = depth)
      loop.children.foreach(recLoop(_, depth + 1))
    }

    recLoop(loopTree, 0)

    val scheduler = new ClickScheduler(dominators, log) {
      override def downstream(ssa: SSA.Node) = ssa.downstreamList.toSeq

      override def upstream(ssa: SSA.Node) = ssa.upstream.collect{case ssa: SSA.ValOrState => ssa}

      override def isPinned(ssa: SSA.Node) = ssa.isInstanceOf[SSA.Control]

      override def loopNest(block: SSA.Node) = {
        assert(block != null)
        loopNestMap(block)
      }
    }

    allVertices.collect{
      case c: SSA.Phi => scheduler.block(c) = c.block
      case c: SSA.Val if c.upstream.isEmpty => scheduler.block(c) = startBlock
      case c: SSA.State =>
        c.parent match{
          case b: SSA.Block => scheduler.block(c) = b
          case _ =>
        }
    }

    allVertices.collect{
      case scheduleRoot: SSA.Phi => scheduler.scheduleEarlyRoot(scheduleRoot)
      case scheduleRoot: SSA.Control => scheduler.scheduleEarlyRoot(scheduleRoot)
    }

    //    pprint.log(scheduler.block.map{case (k, v) => (k, mapping(v))}, height=9999)

    allVertices.collect{
      case scheduleRoot: SSA.Phi => scheduler.scheduleLateRoot(scheduleRoot)
      case scheduleRoot: SSA.Control => scheduler.scheduleLateRoot(scheduleRoot)
    }

    //    pprint.log(scheduler.block.map{case (k, v) => (k, mapping(v))}, height=9999)

    //    ???
    scheduler.block.filter{case (k, v) => v != null}.toMap
  }
}
