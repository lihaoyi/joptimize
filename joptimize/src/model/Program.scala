package joptimize.model

import joptimize.Util

import scala.collection.mutable


/**
  * Represents the combined control/data-flow graph of a SSA program.
  */
case class Program(var args: Seq[SSA.Arg], var allTerminals: Seq[SSA.Control]){
  def getAllVertices() = Util.breadthFirstSeen[SSA.Node](allTerminals.toSet)(_.upstream)

  def checkLinks(checkDead: Boolean = true) = {
    val allVertices = getAllVertices()
    allVertices.foreach(_.checkLinks())

    if (checkDead) {
      val missing = for {
        v <- allVertices
        down <- v.downstreamList
        if !allVertices.contains(down)
      } yield (v, down)

      assert(missing.isEmpty, "Downstream nodes not part of allVertices: " + missing.mkString(", "))
    }
  }

  def transform(visit: PartialFunction[SSA.Node, Seq[SSA.Node]]) = {
    val queue = getAllVertices().to[mutable.LinkedHashSet]
    while (queue.nonEmpty) {
      val current = queue.head
      queue.remove(current)
      for(next <- visit.lift(current)) {
        for(n <- next) queue.add(n)
      }
    }
  }

  def removeDeadNodes() = {
    // Remove dead phi nodes that may have been inserted during SSA construction
    val allVertices = getAllVertices()
    for(v <- allVertices){
      for(down <- v.downstreamList){
        v match{
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