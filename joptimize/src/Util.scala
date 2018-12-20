package joptimize

import java.io.PrintWriter
import java.io.StringWriter

import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.mutable
import collection.JavaConverters._
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
  def clone(input: AbstractInsnNode) = input match{
    case i: FieldInsnNode => new FieldInsnNode(i.getOpcode, i.owner, i.name, i.desc)
    case i: FrameNode => new FrameNode(i.`type`, i.local.size, i.local.asScala.toArray, i.stack.size, i.stack.asScala.toArray)
    case i: IincInsnNode => new IincInsnNode(i.`var`, i.incr)
    case i: InsnNode => new InsnNode(i.getOpcode)
    case i: IntInsnNode => new IntInsnNode(i.getOpcode, i.operand)
    case i: InvokeDynamicInsnNode => new InvokeDynamicInsnNode(i.name, i.desc, i.bsm, i.bsmArgs.clone())
    case i: JumpInsnNode => new JumpInsnNode(i.getOpcode, i.label)
    case i: LdcInsnNode => new LdcInsnNode(i.getOpcode, i.cst)
    case i: LineNumberNode => new LineNumberNode(i.line, i.start)
    case i: LookupSwitchInsnNode => new LookupSwitchInsnNode(i.dflt, i.keys.asScala.toArray.map(_.intValue()), i.labels.asScala.toArray)
    case i: MethodInsnNode => new MethodInsnNode(i.getOpcode, i.owner, i.name, i.desc, i.itf)
    case i: MultiANewArrayInsnNode => new MultiANewArrayInsnNode(i.desc, i.dims)
    case i: TableSwitchInsnNode => new TableSwitchInsnNode(i.min, i.max, i.dflt, i.labels.asScala.toArray:_*)
    case i: VarInsnNode => new VarInsnNode(i.getOpcode, i.`var`)
    case i: TypeInsnNode => new TypeInsnNode(i.getOpcode, i.desc)
  }
}
