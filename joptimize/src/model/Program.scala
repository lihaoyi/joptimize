package joptimize.model

import joptimize.Util


/**
  * Represents the combined control/data-flow graph of a SSA program.
  */
case class Program(allTerminals: Seq[SSA.Control]){
  def getAllVertices() = {
    Util.breadthFirstAggregation[SSA.Node](allTerminals.toSet)(_.upstream)._1
  }
}