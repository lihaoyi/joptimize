package joptimize

import java.io.PrintWriter
import java.io.StringWriter

import joptimize.graph.TarjansStronglyConnectedComponents
import joptimize.model._
import org.objectweb.asm.{Handle, Opcodes}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.mutable
import collection.JavaConverters._
object Util{

  def removeFromJavaList[T](list: java.util.List[T])(pred: T => Boolean) = {
    import collection.JavaConverters._
    list.iterator.asScala.indexWhere(pred) match{
      case -1 => // do nothing
      case n => list.remove(n)
    }
  }

  def mangle(name: String,
             inferredTypes: Seq[IType],
             originalTypes: Seq[JType],
             narrowReturnType: IType,
             originalReturnType: JType) = {
    val mangledName = name + "__" + inferredTypes.map(_.name).mkString("__").replace('/', '_')
    val jTypeArgs = inferredTypes.zip(originalTypes).map(t => CType.toJType(t._1, t._2))
    val jTypeRet = CType.toJType(narrowReturnType, originalReturnType)
    val mangledJTypeDesc = Desc(jTypeArgs, jTypeRet)
    (mangledName, mangledJTypeDesc)
  }

  def leastUpperBound[T](starts: Set[T])(edges: T => Seq[T]) = {
    // Walk up the graph from all starting locations
    val (seens, terminalss, backEdgess) =
      starts.map(start => breadthFirstAggregation(Set(start))(edges)).unzip3

    // Find the set of nodes which overlap all the transitive closures
    val overlap = seens.reduce(_.intersect(_))
    // Find the overlap of the terminal nodes
    val overlapTerminals = terminalss.reduce(_.intersect(_))

    // Walk the graph backwards, from the terminal nodes, to find the
    // reverse-terminal nodes which are part of the overlap
    val backMap = mutable.LinkedHashMap.empty[T, List[T]]
    for{
      backEdges <- backEdgess
      (src, dest) <- backEdges
      if overlap.contains(dest)
    } backMap(src) = dest :: backMap.getOrElse(src, Nil)

    val (backSeen, backTerminals, backBackEdges) =
      breadthFirstAggregation[T](overlapTerminals)(backMap.getOrElse(_, Nil))

    backTerminals
  }

  def breadthFirstAggregation[T](start: Set[T])(edges: T => Seq[T]): (Set[T], Set[T], Seq[(T, T)]) = {
    val queue = start.to[mutable.Queue]
    val seen = mutable.LinkedHashSet.empty[T]
    val terminals = mutable.LinkedHashSet.empty[T]
    val backEdges = mutable.Buffer.empty[(T, T)]
    while(queue.nonEmpty){
      val current = queue.dequeue()
      if (!seen(current)){
        seen.add(current)
        val next = edges(current)
        if (next.isEmpty) terminals.add(current)
        for(n <- next){
          queue.enqueue(n)
          backEdges.append((n, current))
        }
      }
    }
    (seen.toSet, terminals.toSet, backEdges)
  }
  def isCompatible(inferredTypes: Seq[IType], originalTypes: Seq[JType]): Boolean = {
    inferredTypes.length == originalTypes.length &&
    inferredTypes.iterator.zip(originalTypes.iterator).forall{
      case (JType.Prim.I, JType.Prim.Z | JType.Prim.B | JType.Prim.S) => true
      case (CType.I(_), x) => x == JType.Prim.I
      case (CType.J(_), x) => x == JType.Prim.J
      case (CType.F(_), x) => x == JType.Prim.F
      case (CType.D(_), x) => x == JType.Prim.D
      case (inf: JType, orig: JType) => inf == orig
      case (CType.Intersect(classes), orig: JType) => ???
    }
  }
  def clone(input: AbstractInsnNode, labelMapping: mutable.LinkedHashMap[AbstractInsnNode, AbstractInsnNode]) = {
    labelMapping.getOrElseUpdate(input,
      input match{
        case i: FieldInsnNode => new FieldInsnNode(i.getOpcode, i.owner, i.name, i.desc)
        case i: FrameNode => new FrameNode(i.`type`, i.local.size, i.local.asScala.toArray, i.stack.size, i.stack.asScala.toArray)
        case i: IincInsnNode => new IincInsnNode(i.`var`, i.incr)
        case i: InsnNode => new InsnNode(i.getOpcode)
        case i: IntInsnNode => new IntInsnNode(i.getOpcode, i.operand)
        case i: InvokeDynamicInsnNode => new InvokeDynamicInsnNode(i.name, i.desc, i.bsm, i.bsmArgs.clone():_*)
        case i: JumpInsnNode => new JumpInsnNode(i.getOpcode, i.label)
        case i: LabelNode =>  new LabelNode()
        case i: LdcInsnNode => new LdcInsnNode(i.cst)
        case i: LineNumberNode => new LineNumberNode(i.line, i.start)
        case i: LookupSwitchInsnNode => new LookupSwitchInsnNode(i.dflt, i.keys.asScala.toArray.map(_.intValue()), i.labels.asScala.toArray)
        case i: MethodInsnNode => new MethodInsnNode(i.getOpcode, i.owner, i.name, i.desc, i.itf)
        case i: MultiANewArrayInsnNode => new MultiANewArrayInsnNode(i.desc, i.dims)
        case i: TableSwitchInsnNode => new TableSwitchInsnNode(i.min, i.max, i.dflt, i.labels.asScala.toArray:_*)
        case i: VarInsnNode => new VarInsnNode(i.getOpcode, i.`var`)
        case i: TypeInsnNode => new TypeInsnNode(i.getOpcode, i.desc)
      }
    )
  }

  def findSeenInterfaces(classNodeMap: Map[JType.Cls, ClassNode], classNodes: List[ClassNode]) = {
    // Discover all interfaces implemented by the visited classes and find all
    // their super-interfaces. We need to discover these separately as interfaces
    // do not have an <init> method and if unused won't be picked up by the
    // abstract interpreter, but still need to be present since they're
    // implemented by the classes we do use
    val visitedInterfaces = mutable.LinkedHashSet.empty[String]
    val queue = classNodes.flatMap(_.interfaces.asScala).distinct.to[mutable.Queue]
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (!visitedInterfaces.contains(current)) {
        visitedInterfaces.add(current)
        queue.enqueue(classNodeMap(current).interfaces.asScala: _*)
      }
    }
    visitedInterfaces
  }


  def constantToInstruction(tpe: CType.Constant[_]) = {
    tpe match{
      case CType.I(v) =>
        v match{
          case -1 => new InsnNode(ICONST_M1)
          case 0 => new InsnNode(ICONST_0)
          case 1 => new InsnNode(ICONST_1)
          case 2 => new InsnNode(ICONST_2)
          case 3 => new InsnNode(ICONST_3)
          case 4 => new InsnNode(ICONST_4)
          case 5 => new InsnNode(ICONST_5)
          case _ => new LdcInsnNode(java.lang.Integer.valueOf(v))
        }
      case CType.J(v) =>
        v match{
          case 0 => new InsnNode(LCONST_0)
          case 1 => new InsnNode(LCONST_1)
          case _ => new LdcInsnNode(java.lang.Long.valueOf(v))
        }
      case CType.F(v) =>
        v match{
          case 0 => new InsnNode(FCONST_0)
          case 1 => new InsnNode(FCONST_1)
          case 2 => new InsnNode(FCONST_2)
          case _ => new LdcInsnNode(java.lang.Float.valueOf(v))
        }
      case CType.D(v) =>
        v match{
          case 0 => new InsnNode(DCONST_0)
          case 1 => new InsnNode(DCONST_1)
          case _ => new LdcInsnNode(java.lang.Double.valueOf(v))
        }
    }
  }


  def edgeListToIndexMap[T](edges: Seq[(T, T)], vertexToIndex: Map[T, Int]) = {
    edges
      .map { case (k, v) => (vertexToIndex(k), vertexToIndex(v)) }
      .groupBy(_._1)
      .map { case (k, vs) => (k, vs.map(_._2)) }
  }

  def mapToAdjacencyLists(indexMap: Map[Int, Seq[Int]], size: Int) = {
    Range(0, size).map(indexMap.getOrElse(_, Nil))
  }

  val metafactory = new Handle(
    Opcodes.H_INVOKESTATIC,
    "java/lang/invoke/LambdaMetafactory",
    "metafactory",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
  )
  val altMetafactory = new Handle(
    Opcodes.H_INVOKESTATIC,
    "java/lang/invoke/LambdaMetafactory",
    "altMetafactory",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;"
  )
  val makeConcatWithConstants = new Handle(
    Opcodes.H_INVOKESTATIC,
    "java/lang/invoke/StringConcatFactory",
    "makeConcatWithConstants",
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;"
  )


  /**
    * We order vertices in two ways:
    *
    * 1) Between cycles, in topological data/control-flow order
    * 2) Within cycles, in dataflow order *ignoring phis/regions* to break the cycle
    *
    * That gives us roughly a sequence of instructions that start from top to
    * bottom, and within each cycle data/control always flows downwards except for
    * jumps which may return to an earlier phi/region node.
    */
  def sortVerticesForPrinting[T](allVertices: Set[T],
                                 downstreamEdges: Seq[(T, T)])
                                (backEdge: (T, T) => Boolean) = {
    val vertexToIndex = allVertices.zipWithIndex.toMap
    val indexToVertex = vertexToIndex.map(_.swap)


    val brokenEdgeLists = Util.edgeListToIndexMap(
      downstreamEdges.filter(x => !backEdge(x._1, x._2)),
      vertexToIndex
    )

    val brokenOrderingList = TarjansStronglyConnectedComponents(Util.mapToAdjacencyLists(brokenEdgeLists, allVertices.size)).map { case Seq(x) => x }

    val brokenOrdering = brokenOrderingList.zipWithIndex.toMap

    val groupedEdgeLists = Util.edgeListToIndexMap(downstreamEdges, vertexToIndex)

    val groupedOrdering = TarjansStronglyConnectedComponents(Util.mapToAdjacencyLists(groupedEdgeLists, allVertices.size))

    val orderingList = groupedOrdering.flatMap(_.sortBy(brokenOrdering)).map(indexToVertex)

    val finalOrderingMap = orderingList.reverse.zipWithIndex.toMap
    finalOrderingMap
  }

  def findSaveable(program: Program, scheduledVals: Map[SSA.Val, SSA.Control]) = {
    val allTerminals = program.allTerminals
    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[SSA.Node](allTerminals.toSet) { x =>
        val addedControl = x match {
          case v: SSA.Val => scheduledVals.get(v).toSeq
          case _ => Nil
        }
        x.upstream ++ addedControl
      }

    val finalOrderingMap = sortVerticesForPrinting(allVertices, downstreamEdges) {
      case (_, _: SSA.Phi | _: SSA.Merge) => true
      case (v: SSA.Val, c: SSA.Control) => true
      case _ => false
    }

    val saveable =
      allVertices.filter { k =>
        val x = k.downstreamList
        val scheduled = k match {
          case v: SSA.Val =>
            val downstreamControls = x.collect {
              case t: SSA.Val => scheduledVals.get(t)
              case t: SSA.SimpleBlock => Some(t)
            }.flatten
            scheduledVals.get(v).exists(c => downstreamControls.exists(_ != c))
          case _ => false
        }
        pprint.log((k, k.downstreamSize, k.downstreamList))
        k.upstream.nonEmpty && (k.downstreamSize > 1 || allTerminals.contains(k) || scheduled || k.isInstanceOf[SSA.Copy])
      } ++
      allVertices.collect {
        case k: SSA.Phi => k
        case b: SSA.Control => b
      }

    val savedLocals = mutable.Map[SSA.Val, (Int, String)]()

    allVertices.collect{case a: SSA.Arg =>
      savedLocals.update(a, (a.index, "arg" + a.index))
    }

    saveable.collect{case r: SSA.Val =>
      savedLocals.update(
        r,
        (
          savedLocals.size,
          if (r.downstreamSize > 1) "local" + savedLocals.size
          else "stack" + savedLocals.size
        )
      )
    }

    pprint.log(saveable)
    pprint.log(savedLocals)

    (finalOrderingMap, saveable, savedLocals)
  }


  def findControlFlowGraph(program: Program) = {
    val controlFlowEdges = mutable.Buffer.empty[(SSA.Control, SSA.Control)]
    val visited = mutable.LinkedHashSet.empty[SSA.Control]

    def rec(current: SSA.Control): Unit = if (!visited(current)){
      visited.add(current)

      for(control <- current.controls){
        rec(control)
        controlFlowEdges.append(control -> current)
      }
    }

    program.allTerminals.foreach(rec)
    controlFlowEdges
  }
}
