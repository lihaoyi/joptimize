package joptimize

import java.io.PrintWriter
import java.io.StringWriter

import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.mutable

object Util{
  private val printer = new Textifier
  private val methodPrinter = new TraceMethodVisitor(printer)

  def removeFromJavaList[T](list: java.util.List[T])(pred: T => Boolean) = {
    import collection.JavaConverters._
    list.iterator.asScala.indexWhere(pred) match{
      case -1 => // do nothing
      case n => list.remove(n)
    }
  }
  def prettyprint(insnNode: AbstractInsnNode) = {
    insnNode.accept(methodPrinter)
    val sw = new StringWriter
    printer.print(new PrintWriter(sw))
    printer.getText.clear
    sw.toString.stripSuffix("\n")
  }
  def mangle(name: String,
             inferredTypes: Seq[IType],
             originalTypes: Seq[JType],
             narrowReturnType: IType,
             originalReturnType: JType) = {
    val mangledName = name + "__" + inferredTypes.map(_.name).mkString("__").replace('/', '_')
    val jTypeArgs = inferredTypes.zip(originalTypes).map(t => JType.fromIType(t._1, t._2))
    val jTypeRet = JType.fromIType(narrowReturnType, originalReturnType)
    val mangledJTypeDesc = Desc(jTypeArgs, jTypeRet)
    (mangledName, mangledJTypeDesc)
  }

  def leastUpperBound[T](starts: Set[T])(edges: T => Seq[T]) = {
    // Walk up the graph from all starting locations
    val (seens, terminalss, backEdgess) =
      starts.map(start => breadthFirstAggregation(Set(start), edges)).unzip3

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
      breadthFirstAggregation[T](overlapTerminals, backMap.getOrElse(_, Nil))

    backTerminals
  }

  def breadthFirstAggregation[T](start: Set[T], edges: T => Seq[T]): (Set[T], Set[T], Set[(T, T)]) = {
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
  def isCompatible(inferredTypes: Seq[IType], originalTypes: Seq[JType]): Boolean = {
    inferredTypes.length == originalTypes.length &&
    inferredTypes.iterator.zip(originalTypes.iterator).forall{
      case (JType.Prim.I, JType.Prim.Z | JType.Prim.B | JType.Prim.S) => true
      case (IType.I(_), x) => x == JType.Prim.I
      case (IType.J(_), x) => x == JType.Prim.J
      case (IType.F(_), x) => x == JType.Prim.F
      case (IType.D(_), x) => x == JType.Prim.D
      case (inf: JType, orig: JType) => inf == orig
      case (IType.Intersect(classes), orig: JType) => ???
    }
  }
}
