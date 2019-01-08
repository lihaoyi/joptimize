package joptimize.model



/**
  * Represents the combined control/data-flow graph of a SSA program.
  */
case class Program(allTerminals: Seq[SSA.Block])