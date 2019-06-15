package joptimize.algorithms

import joptimize.graph.LengauerTarjanDominatorTree

object Dominator {
  case class Result[T](immediateDominators: Map[T, T], dominatorDepth: Map[T, Int])
  def findDominators[T](edges: collection.Seq[(T, T)], allNodes0: collection.Seq[T]): Result[T] = {
    val allNodes = allNodes0.distinct
    val indices = allNodes.zipWithIndex.toMap
    val nodes = indices.map(_.swap)

    val successorMap =
      edges.groupBy(_._1).map { case (k, v) => (indices(k), v.map(_._2).map(indices)) }
    val predecessorMap =
      edges.groupBy(_._2).map { case (k, v) => (indices(k), v.map(_._1).map(indices)) }

    val immediateDominators = new LengauerTarjanDominatorTree {
      def successors(v: Int) = successorMap.getOrElse(v, Nil)
      def predecessors(v: Int) = predecessorMap.getOrElse(v, Nil)
      def numNodes = indices.size
    }.computeDominatorTree()

    val dominatorDepth = {
      Array.tabulate(immediateDominators.length) { i =>
        var current = i
        var n = 0
        while (immediateDominators(current) != -1) {
          current = immediateDominators(current)
          n += 1
        }
        n
      }
    }

    Result(
      immediateDominators
        .zipWithIndex
        .collect { case (v, i) if v != -1 => (nodes(i), nodes(v)) }
        .toMap,
      dominatorDepth.zipWithIndex.map { case (v, i) => (nodes(i), v) }.toMap
    )
  }
}
