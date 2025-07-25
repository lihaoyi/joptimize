package joptimize


import joptimize.graph.TarjansStronglyConnectedComponents
import joptimize.model._
import org.objectweb.asm.{Handle, Opcodes}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.collection.mutable
import collection.JavaConverters._
import scala.util.control.NoStackTrace
class LabeledException(label: String, e: Throwable) extends Exception(label, e) with NoStackTrace
object Util{
  def labelExceptions[T](label: String)(t: => T): T = {
    try t
    catch{ case e: Throwable =>
      throw new LabeledException(label, e)
    }
  }
  def removeFromJavaList[T](list: java.util.List[T])(pred: T => Boolean) = {
    import collection.JavaConverters._
    list.iterator.asScala.indexWhere(pred) match{
      case -1 => // do nothing
      case n => list.remove(n)
    }
  }

  def mangle(isig: InferredSig,
             narrowReturnType: IType,
             liveArgs: Int => Boolean) = {

    val jTypeRet = CType.toJType(narrowReturnType)

    if (isManglingCompatible(isig.inferred, isig.method.desc.args)) {

      val narrowedDesc = Desc(
        mangleLiveArgList(isig.method.desc.args, isig.method.static, liveArgs),
        jTypeRet
      )

      val mangledName = (jTypeRet, isig.method.desc.ret) match {
        case (JType.Prim.I, JType.Prim.Z) => isig.method.name
        case (JType.Prim.I, JType.Prim.S) => isig.method.name
        case (JType.Prim.I, JType.Prim.B) => isig.method.name
        case (lhs, rhs) =>
          if (lhs == rhs) isig.method.name
          else mangleName0(isig.method, isig.inferred)
      }
      (mangledName, narrowedDesc)
    }
    else{
      val mangledName = mangleName0(isig.method, isig.inferred)

      val jTypeArgs = isig.inferred.map(CType.toJType)

      val mangledJTypeDesc = Desc(
        mangleLiveArgList(jTypeArgs, isig.method.static, liveArgs),
        jTypeRet
      )
      (mangledName, mangledJTypeDesc)
    }
  }

  def mangleLiveArgList(args: Seq[JType], static: Boolean, liveArgs: Int => Boolean) = {
    val offset = if (static) 0 else 1
    args.zipWithIndex.flatMap { case (a, i) =>
      val offsetIndex = i + offset
      if ((!static && offsetIndex == 0) || liveArgs(offsetIndex)) Some(a)
      else None
    }
  }

  def mangleName(originalSig: MethodSig, inferredTypes: Seq[IType]) = {
    if (isManglingCompatible(inferredTypes, originalSig.desc.args)) originalSig.name
    else mangleName0(originalSig, inferredTypes)
  }

  def mangleName0(originalSig: MethodSig, inferredTypes: Seq[IType]) = {
    originalSig.name + "__" + mangleArgs(inferredTypes) + "__" + mangleArgs(Seq(originalSig.desc.ret))
  }

  def mangleArgs(inferredTypes: Seq[IType]) =
    inferredTypes.map(_.name)
      .mkString("__")
      .replace('/', '_')
      .replace('[', 'A')
      .replace(";", "")

  def leastUpperBound[T](starts: Set[T])(edges: T => collection.Seq[T]) = {
    // Walk up the graph from all starting locations
    val (seens, terminalss, backEdgess) =
      starts.map(start => breadthFirstAggregation0(Set(start))(edges(_).map(_ -> ()))).unzip3

    // Find the set of nodes which overlap all the transitive closures
    val overlap = seens.map(_.keySet).reduce(_.intersect(_))
    // Find the overlap of the terminal nodes
    val overlapTerminals = terminalss.reduce(_.intersect(_))

    // Walk the graph backwards, from the terminal nodes, to find the
    // reverse-terminal nodes which are part of the overlap
    val backMap = mutable.LinkedHashMap.empty[T, List[T]]
    for {
      backEdges <- backEdgess
      (src, dest, _) <- backEdges
      if overlap.contains(dest)
    } backMap(src) = dest :: backMap.getOrElse(src, Nil)

    breadthFirstTerminals[T](overlapTerminals)(backMap.getOrElse(_, Nil))
  }
  def breadthFirstSeen[T](start: Set[T])(edges: T => Seq[T]): Set[T] = {
    val (seen, terminals, backEdges) = breadthFirstAggregation0(start, trackBackEdges = false, trackTerminals = false)(edges(_).map(_ -> ()))
    seen.keySet
  }
  def breadthFirstTerminals[T](start: Set[T])(edges: T => Seq[T]): Set[T] = {
    val (seen, terminals, backEdges) = breadthFirstAggregation0(start, trackBackEdges = false)(edges(_).map(_ -> ()))
    terminals
  }
  def breadthFirstBackEdges[T](start: Set[T])(edges: T => Seq[T]): collection.Seq[(T, T)] = {
    val (seen, terminals, backEdges) = breadthFirstAggregation0(start, trackTerminals = false)(edges(_).map(_ -> ()))
    backEdges.map{case (a, b, c) => (a, b)}
  }
  def breadthFirstAggregation0[T, V](start: Set[T],
                                     trackBackEdges: Boolean = true,
                                     trackTerminals: Boolean = true)
                                    (edges: T => collection.Seq[(T, V)]): (Map[T, List[T]], Set[T], collection.Seq[(T, T, V)]) = {
    val queue = start.to(mutable.Queue).map((_, List.empty[T]))
    val seen = mutable.LinkedHashMap.empty[T, List[T]]
    val terminals = if (trackTerminals) mutable.LinkedHashSet.empty[T] else null
    val backEdges = if (trackBackEdges) mutable.Buffer.empty[(T, T, V)] else null
    while(queue.nonEmpty){
      val (current, path) = queue.dequeue()
      if (!seen.contains(current)){
        seen(current) = path
        val next = edges(current)
        if (trackTerminals && next.isEmpty) terminals.add(current)
        for((n, metadata) <- next){
          queue.enqueue((n, current :: path))
          if (trackBackEdges) backEdges.append((n, current, metadata))
        }
      }
    }
    (seen.toMap, if (terminals != null) terminals.toSet else null, backEdges)
  }

  def isValidationCompatible(inferredTypes: Seq[IType],
                             originalSig: MethodSig,
                             checkSubclass: (JType.Cls, JType.Cls) => Boolean): Boolean = {
    val originalTypes = originalSig.desc.args
    val sameLength = inferredTypes.length == originalTypes.length

    val sameItems = inferredTypes.iterator.zip(originalTypes.iterator).forall(x => isValidationCompatible0(x._1, x._2, checkSubclass))

    sameLength && sameItems
  }
  def isValidationCompatible0(inferredType: IType,
                              originalType: JType,
                              checkSubclass: (JType.Cls, JType.Cls) => Boolean): Boolean = (inferredType, originalType) match{
    case (CType.I(_) | JType.Prim.I, JType.Prim.Z | JType.Prim.B | JType.Prim.S | JType.Prim.I | JType.Prim.C) => true
    case (JType.Prim.Z | JType.Prim.B | JType.Prim.S | JType.Prim.I | JType.Prim.C, JType.Prim.I) => true
    case (CType.J(_), JType.Prim.J) => true
    case (CType.F(_), JType.Prim.F) => true
    case (CType.D(_), JType.Prim.D) => true
    case (inf: JType.Cls, orig: JType.Cls) => checkSubclass(inf, orig)
    case (lhs, rhs) => lhs == rhs
  }

  def isManglingCompatible(inferredTypes: Seq[IType], originalTypes: Seq[JType]): Boolean = {
    val sameLength = inferredTypes.length == originalTypes.length
    val sameItems = inferredTypes.iterator.zip(originalTypes.iterator).forall(x => isManglingCompatible0(x._1, x._2))
    sameLength && sameItems
  }
  def isManglingCompatible0(inferredType: IType, originalType: JType): Boolean = (inferredType, originalType) match{
    case (JType.Prim.I, JType.Prim.Z | JType.Prim.B | JType.Prim.S | JType.Prim.I | JType.Prim.C) => true
    case (JType.Prim.Z | JType.Prim.B | JType.Prim.S | JType.Prim.I | JType.Prim.C, JType.Prim.I) => true
    case (inf: JType, orig: JType) => inf == orig
    case (lhs, rhs) => lhs == rhs
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

  def findSeenInterfaces(loadClass: JType.Cls => Option[ClassNode], classNodes: Seq[ClassNode]) = {
    // Discover all interfaces implemented by the visited classes and find all
    // their super-interfaces. We need to discover these separately as interfaces
    // do not have an <init> method and if unused won't be picked up by the
    // abstract interpreter, but still need to be present since they're
    // implemented by the classes we do use
    val visitedInterfaces = mutable.LinkedHashSet.empty[String]
    val queue = classNodes.flatMap(_.interfaces.asScala).distinct.to(mutable.Queue)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (!visitedInterfaces.contains(current)) {
        visitedInterfaces.add(current)
        loadClass(current).foreach{cn =>
          queue.enqueueAll(cn.interfaces.asScala)
        }
      }
    }
    visitedInterfaces
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

  val metafactory = SSA.InvokeDynamic.Bootstrap(
    Opcodes.H_INVOKESTATIC,
    "java/lang/invoke/LambdaMetafactory",
    "metafactory",
    Desc.read(
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
    )
  )
  val altMetafactory =  SSA.InvokeDynamic.Bootstrap(
    Opcodes.H_INVOKESTATIC,
    "java/lang/invoke/LambdaMetafactory",
    "altMetafactory",
    Desc.read(
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;"
    )
  )
  val makeConcatWithConstants = SSA.InvokeDynamic.Bootstrap(
    Opcodes.H_INVOKESTATIC,
    "java/lang/invoke/StringConcatFactory",
    "makeConcatWithConstants",
    Desc.read(
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;"
    )
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


  def replace(current: SSA.Val, replacement: SSA.Val): Seq[SSA.Node] = {
    current.upstream.collect{
      case v: SSA.Val =>
        v.downstreamRemoveAll(current)
      case v: SSA.Block => v.nextPhis = v.nextPhis.filter(_ != current)
    }

    for (down <- current.downstreamList if down != current) {
      replacement.downstreamAdd(down)
      down.replaceUpstream(current, replacement)
    }
    replacement +: replacement.downstreamList
  }

  def argMapping(sig: MethodSig, filter: Int => Boolean) = {
    var originalIndex = if (sig.static) 0 else 1
    var finalIndex = originalIndex
    val output = mutable.Map.empty[Int, Int]
    if (!sig.static) {
      output(0) = 0
    }
    for(arg <- sig.desc.args){
      if (filter(originalIndex)){
        output(originalIndex) = finalIndex
        finalIndex += arg.size
      }
      originalIndex += 1
    }

    output.toMap
  }
}
