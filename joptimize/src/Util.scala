package joptimize

import java.io.PrintWriter
import java.io.StringWriter

import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.mutable

object Util{
  private val printer = new Textifier
  private val methodPrinter = new TraceMethodVisitor(printer)

  def prettyprint(insnNode: AbstractInsnNode) = {
    insnNode.accept(methodPrinter)
    val sw = new StringWriter
    printer.print(new PrintWriter(sw))
    printer.getText.clear
    sw.toString.stripSuffix("\n")
  }
  def mangle(name: String, stackTypes: Seq[JType], narrowReturnType: JType) = {
    val mangledName = name + "__" + stackTypes.mkString("__").replace('/', '_').replace(';', '_')
    val mangledDesc = Desc(stackTypes, narrowReturnType)
    (mangledName, mangledDesc)
  }

  def leastUpperBound[T](starts: Set[T])(edges: T => Seq[T]) = {
    // Walk up the graph from all starting locations
    val (seens, terminalss, backEdgess) =
      starts.map(start => walk(Set(start), edges)).unzip3

    // Find the set of nodes which overlap all the transitive closures
    val overlap = seens.reduce(_.intersect(_))
    // Find the overlap of the terminal nodes
    val overlapTerminals = terminalss.reduce(_.intersect(_))

    // Walk the graph backwards, from the terminal nodes, to find the
    // reverse-terminal nodes which are part of the overlap
    val backMap = mutable.Map.empty[T, List[T]]
    for{
      backEdges <- backEdgess
      (src, dest) <- backEdges
      if overlap.contains(dest)
    } backMap(src) = dest :: backMap.getOrElse(src, Nil)

    val (backSeen, backTerminals, backBackEdges) =
      walk[T](overlapTerminals, backMap.getOrElse(_, Nil))

    backTerminals
  }

  def walk[T](start: Set[T], edges: T => Seq[T]): (Set[T], Set[T], Set[(T, T)]) = {
    val queue = start.to[mutable.Queue]
    val seen = mutable.Set.empty[T]
    val terminals = mutable.Set.empty[T]
    val backEdges = mutable.Set.empty[(T, T)]
    while(queue.nonEmpty){
      val current = queue.dequeue()
      if (!seen(current)){
        seen.add(current)
        val next = edges(current)
        if (next.isEmpty) terminals.add(current)
        for(n <- next){
          queue.enqueue(n)
          backEdges.add((n, current))
        }
      }
    }
    (seen.toSet, terminals.toSet, backEdges.toSet)
  }
}
