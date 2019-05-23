package joptimize.model

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import joptimize.Util

import scala.collection.mutable

/**
  * Represents the combined control/data-flow graph of a SSA program.
  */
case class MethodBody(var args: Seq[SSA.Arg], var allTerminals: Seq[SSA.Jump]) {
  def getAllVertices() = Util.breadthFirstSeen[SSA.Node](allTerminals.toSet)(_.upstream)

  def checkLinks(checkDead: Boolean = true) = {
    val allVertices = getAllVertices()
    allVertices.foreach(_.checkLinks())

    val starts = allVertices.collect { case s: SSA.Start => s }
    assert(starts.size == 1, "Incorrect number of SSA.Start nodes: " + starts.mkString(", "))
    if (checkDead) {
      val missing = for {
        v <- allVertices
        down <- v.downstreamList
        if !allVertices.contains(down)
      } yield (v, down)

      assert(missing.isEmpty, "Downstream nodes not part of allVertices: " + missing.mkString(", "))

      val allEdges = allVertices
        .toSeq
        .flatMap { v =>
          v.upstream.map(_ -> v) ++ v.downstreamList.map(_ -> v)
        }

      val allEdgeCount = allEdges.groupBy(x => x).map { case (k, v) => (k, v.size) }

      for ((src, dest) <- allEdgeCount.keysIterator) {
        val downstream = allEdgeCount((src, dest))
        val upstream = allEdgeCount((dest, src))
        assert(
          downstream == upstream,
          s"Unmatched upstream and downstream edges between ($src, $dest): downstream $downstream vs upstream $upstream"
        )
      }
    }

  }

  def transform(visit: PartialFunction[SSA.Node, Seq[SSA.Node]]) = {
    val queue = getAllVertices().to[mutable.LinkedHashSet]
    while (queue.nonEmpty) {
      val current = queue.head
      queue.remove(current)
      for (next <- visit.lift(current)) {
        for (n <- next) queue.add(n)
      }
    }
  }

  def removeDeadNodes() = {
    // Remove dead phi nodes that may have been inserted during SSA construction
    val allVertices = getAllVertices()
    for (v <- allVertices) {
      for (down <- v.downstreamList) {
        v match {
          case n: SSA.Val =>
            if (!allVertices.contains(down)) {
              n.downstreamRemoveAll(down)
            }
          case n: SSA.Merge =>
            if (!allVertices.contains(down)) {
              n.phis = n.phis.filter(_ != down)
              n.nextPhis = n.nextPhis.filter(_ != down)
            }
          case n: SSA.SimpleBlock =>
            if (!allVertices.contains(down)) {
              n.nextPhis = n.nextPhis.filter(_ != down)
            }
          case _ =>
        }
      }
    }
  }
}
