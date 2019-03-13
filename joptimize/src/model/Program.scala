package joptimize.model

import joptimize.Util

import scala.collection.mutable


/**
  * Represents the combined control/data-flow graph of a SSA program.
  */
case class Program(args: Seq[SSA.Arg], allTerminals: Seq[SSA.Control]){
  def getAllVertices() = Util.breadthFirstSeen[SSA.Node](allTerminals.toSet)(_.upstream)

  def checkLinks() = {
    val allVertices = getAllVertices()
    allVertices.foreach(_.checkLinks())

    val missing = for{
      v <- allVertices
      down <- v.downstreamList
      if !allVertices.contains(down)
    } yield (v, down)

    assert(missing.isEmpty, "Downstream nodes not part of allVertices: " + missing.mkString(", "))
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
}