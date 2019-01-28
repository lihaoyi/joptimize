package joptimize.model

import joptimize.Util


/**
  * Represents the combined control/data-flow graph of a SSA program.
  */
case class Program(args: Seq[SSA.Arg], allTerminals: Seq[SSA.Control]){
  def getAllVertices() = {
    Util.breadthFirstAggregation[SSA.Node](allTerminals.toSet)(_.upstream)._1
  }
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
}