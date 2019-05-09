package joptimize.analyzer
import collection.JavaConverters._
import joptimize.frontend.{ClassManager, Frontend}
import joptimize.{Logger, Util}
import joptimize.model._
import joptimize.viewer.model.LogMessage
import org.objectweb.asm.{Handle, Opcodes}

import scala.collection.mutable

/**
  * Optimistically walks an entire program, comprised of many methods linked together
  * via a call graph. Constructs the method call graph in memory as `callGraph`, and only
  * maintains the current set of analysis-in-progress methods, relying on the `callGraph`
  * to decide where to return to after each method analysis is complete.
  */
class ProgramAnalyzer(entrypoints: Seq[MethodSig],
                      classManager: ClassManager,
                      globalLog: Logger.Global,
                      frontend: Frontend){

  /**
    * The current queue of methods to analyze. Can have multiple methods in it at the same
    * time, e.g. an analyzed method may call multiple other methods, or upon returning may
    * enqueue both the caller method as well as any methods that were invalidated.
    */
  val current = mutable.LinkedHashSet.empty[InferredSig]

  /**
    * The currently set of properties known for each analyzed method. May be invalidated
    * and recomputed for a method if new classes are loaded or recursive calls complete
    */
  val methodProps = mutable.LinkedHashMap.empty[InferredSig, ProgramAnalyzer.Properties]

  /**
    * The set of all called method signatures.
    */
  val calledSignatures = mutable.LinkedHashSet.empty[InferredSig]

  /**
    * Aggregate the list of classes who have static fields loaded separately from the
    * method analysis, since this is the one way you can reference a class and force
    * it to be loaded without calling a method or constructor
    */
  val staticFieldReferencedClasses = mutable.LinkedHashSet.empty[JType.Cls]

  /**
    * Edges from the caller to the called. Used to guide the traversal from an analyzed
    * method to others that depend on it. A call site may have multiple edges leaving it
    * if it is a virtual call resolving to multiple implementations, and more edges may
    * be added to a callsite as more subclassing implementations are discovered.
    *
    * This is constructed dynamically as the program is traversed, and the set of edges
    * grows monotonically.
    */
  val callGraph = mutable.LinkedHashSet.empty[ProgramAnalyzer.CallEdge]

  /**
    * Work in progress optimistic analyses of the various methods in play. These analyses
    * may not be complete, as they may be awaiting the metadata of called functions to
    * proceed or may be invalidated and re-visited if a called function widens its
    * signature
    */
  val analyses = mutable.LinkedHashMap.empty[InferredSig, MethodAnalyzer[IType]]

  /**
    * An aggregation of the various call stacks that can reach a particular method. Grows
    * monotonically, and used to check for recursion. We do not care which call stack
    * reaching a method indicates it is recursive, only that some call stack does so we
    * can treat that recursive call optimistically
    */
  val callStackSets = mutable.Map.empty[InferredSig, Set[InferredSig]]

  def addToCallSet(k: InferredSig, v: Set[InferredSig]) = {
    if (callStackSets.contains(k)) callStackSets(k) ++= v
    else callStackSets(k) = v
  }

  def addCallEdge(edge: ProgramAnalyzer.CallEdge) = {
    callGraph.add(edge)
    addToCallSet(edge.called, callStackSets(edge.caller) + edge.caller)
  }

  def apply() = globalLog.block {
    for (ep <- entrypoints) {
      val isig = InferredSig(ep, ep.desc.args)
      current.add(isig)
      callStackSets(isig) = Set()
      calledSignatures.add(isig)
    }

    while(current.nonEmpty){
      step()
    }

    for((k, v) <- analyses){
      assert(v.evaluateWorkList.isEmpty, (k, v.evaluateWorkList))
      assert(v.invalidateWorkList.isEmpty, (k, v.invalidateWorkList))
    }
    for(m <- methodProps.keysIterator){
      calledSignatures.add(m)
    }
    val visitedResolved = for{
      isig <- calledSignatures
      resolved <- resolveProps(isig)
    } yield (isig, resolved)

    val visitedMethods = mutable.LinkedHashMap.empty[InferredSig, ProgramAnalyzer.MethodResult]
    for((k, props) <- methodProps) {
      visitedMethods(k) = ProgramAnalyzer.MethodResult(
        frontend.cachedMethodBodies(k).get,
        analyses(k).evaluated,
        analyses(k).liveBlocks.toSet,
        props
      )
    }

    classManager.loadClass("scala/runtime/Nothing$")
    globalLog.graph("PROGRAM CALL GRAPH") {
      val allNodes = callGraph.flatMap(edge => Seq(edge.called, edge.caller)).toArray
      val allNodeIndices = allNodes.zipWithIndex.toMap
      LogMessage.Graph(
        allNodes.map(n =>
          LogMessage.Graph.Node(
            n.method.toString + "\n" +
            n.inferred.map(_.name).mkString("(", ", ", ")") + methodProps(n).inferredReturn,
            "cyan",
            live = true
          )
        ),
        callGraph.toSeq.map(edge =>
          LogMessage.Graph.Edge(
            allNodeIndices(edge.caller),
            allNodeIndices(edge.called),
            forwardArrow = true,
            dashed = false,
            thick = false
          )
        )
      )
    }
    globalLog.pprint(visitedMethods.map{case (k, v) => (k.toString, v.props)})
    for((m, props) <- visitedMethods){
      globalLog.inferredMethod(m).pprint(analyses(m).evaluated)
      globalLog.inferredMethod(m).pprint(props.props.inferredReturn)
      globalLog.inferredMethod(m).pprint(props.props.liveArgs)
      globalLog.inferredMethod(m).pprint(props.props.pure)
    }
    ProgramAnalyzer.ProgramResult(
      visitedMethods,
      visitedResolved.toMap,
      staticFieldReferencedClasses
    )
  }

  def step() = {
//    println()
//    println(pprint.apply(current.map(_.method.toString.stripPrefix("joptimize.examples.simple."))))
    val isig = current.maxBy(callStackSets(_).size)

    current.remove(isig)
//    println(isig.toString)

    val methodLog = globalLog.method(isig.method)
    val inferredLog = globalLog.inferredMethod(isig)
    val methodBody = frontend.loadMethodBody(isig, methodLog).get
    val currentAnalysis = analyses.getOrElseUpdate(
      isig,
      optimisticAnalyze(
        (if (isig.method.static) Nil else Seq(isig.method.cls)) ++ isig.inferred,
        inferredLog,
        methodBody,
        Nil
      )
    )

    val step = currentAnalysis.step()

    globalLog.pprint(step)
    step match {
      case MethodAnalyzer.Step.Continue(nodes) =>
        val results: Seq[ProgramAnalyzer.StepResult] = nodes.map{
          case n: SSA.GetField => ProgramAnalyzer.handleFieldReference(isig, n.owner, false, analyzeClinits)
          case n: SSA.PutField => ProgramAnalyzer.handleFieldReference(isig, n.owner, false, analyzeClinits)
          case n: SSA.GetStatic => ProgramAnalyzer.handleFieldReference(isig, n.cls, true, analyzeClinits)
          case n: SSA.PutStatic => ProgramAnalyzer.handleFieldReference(isig, n.cls, true, analyzeClinits)

          case n: SSA.New => ProgramAnalyzer.handleNew(isig, n, classManager, callGraph)
          case invoke: SSA.Invoke =>
            ProgramAnalyzer.handleInvoke(
              isig,
              currentAnalysis,
              invoke,
              classManager,
              analyzeClinits,
              callStackSets(_)(_),
              analyses(isig).evaluated.get(invoke),
              methodProps.get
            )
          case indy: SSA.InvokeDynamic =>
            if (indy.bootstrap == Util.metafactory || indy.bootstrap == Util.altMetafactory) ???
            else if(indy.bootstrap == Util.makeConcatWithConstants){
              currentAnalysis.evaluated(indy) = JType.Cls("java/lang/String")
              ProgramAnalyzer.StepResult()
            } else ???

          case _ => ProgramAnalyzer.StepResult()
        }

        for(result <- results){
          for(e <- result.edges){
            addCallEdge(e)
            current.add(e.called)
          }
          for(s <- result.staticFieldReferencedClasses) staticFieldReferencedClasses.add(s)
          for(s <- result.calledSignatures) calledSignatures.add(s)
          for(s <- result.evaluated) analyses(s._1).evaluated(s._2) = s._3
        }

        current.add(isig)

      case MethodAnalyzer.Step.Done() =>
        val next = handleReturn(isig, inferredLog, currentAnalysis)
        next.foreach(current.add)
    }
  }

  def handleReturn(isig: InferredSig,
                   inferredLog: Logger.InferredMethod,
                   currentAnalysis: MethodAnalyzer[IType]) = {
//    println("DONE")
    val optimisticResult = currentAnalysis.apply()
    val retTypes = currentAnalysis.apply()
      .inferredReturns
      .flatMap(_._2)
      .toSeq

    val inferredReturn = classManager.mergeTypes(retTypes)

    val props = ProgramAnalyzer.Properties(
      inferredReturn,
      computePurity(optimisticResult, callStackSets(isig)) &&
      !(isig.method.static && classManager.loadMethod(MethodSig(isig.method.cls, "<clinit>", Desc(Nil, JType.Prim.V), true)).nonEmpty),
      optimisticResult.inferred.collect { case (a: SSA.Arg, _) => a.index }.toSet
    )

    val returnProps = fansi.Color.Green(isig.toString) ++ " -> " ++ props.toString
    globalLog.apply(returnProps)

    methodProps(isig) = props

    val methodsToEnqueue = mutable.LinkedHashSet.empty[InferredSig]

    for (edge <- callGraph if edge.called == isig) {
      for (node <- edge.node) {
        val mergedType = classManager.mergeTypes(
          analyses(edge.caller).evaluated.get(node).toSeq ++ Seq(props.inferredReturn)
        )
        if (!analyses(edge.caller).evaluated.get(node).contains(mergedType)) {
          analyses(edge.caller).invalidateWorkList.add(MethodAnalyzer.Invalidate.Invoke(node))
          analyses(edge.caller).evaluated(node) = mergedType
          methodsToEnqueue.add(edge.caller)
        }
      }
    }

    methodsToEnqueue.toSeq
  }


  def resolveProps(isig: InferredSig) = {
    classManager.resolvePossibleSigs(isig.method).map{ resolved =>
      val copied = resolved.map(m => isig.copy(method = m))

      val resolvedProps = copied.flatMap(methodProps.get)
      ProgramAnalyzer.Properties(
        classManager.mergeTypes(resolvedProps.map(_.inferredReturn)),
        resolvedProps.forall(_.pure),
        resolvedProps.flatMap(_.liveArgs).toSet
      )
    }
  }

  def analyzeClinits(cls: JType.Cls) = {
    val clinit = MethodSig(cls, "<clinit>", Desc(Nil, JType.Prim.V), true)
    if (classManager.loadMethod(clinit).isDefined && !analyses.contains(InferredSig(clinit, Nil))){
      Seq(InferredSig(clinit, Nil))
    } else{
      Nil
    }
  }

  def optimisticAnalyze(inferredArgs: Seq[IType],
                        log: Logger.InferredMethod,
                        methodBody: MethodBody,
                        innerStack: List[InferredSig]) = {
    new MethodAnalyzer[IType](
      methodBody,
      Map.empty,
      methodBody.getAllVertices().collect { case b: SSA.Block if b.upstream.isEmpty => b }.head,
      new ITypeLattice((x, y) => classManager.mergeTypes(Seq(x, y)), inferredArgs),
      log,
      ITypeBrancher,
      IType.Bottom,
      JType.Prim.V
    )
  }

  def computePurity(optResult: MethodAnalyzer.Result[IType],
                    callSet: Set[InferredSig]) = {
    optResult.inferred.keysIterator.forall {
      case n: SSA.New => false
      case n: SSA.CheckCast => false
      case n: SSA.InstanceOf => true

      case n: SSA.ChangedState => true
      case n: SSA.Arg => true

      case n: SSA.ConstI => true
      case n: SSA.ConstJ => true
      case n: SSA.ConstF => true
      case n: SSA.ConstD => true
      case n: SSA.ConstStr => true
      case n: SSA.ConstNull => true
      case n: SSA.ConstCls => true

      case n: SSA.ArrayLength => false

      case n: SSA.GetField => false
      case n: SSA.PutField => false

      case n: SSA.GetStatic => false
      case n: SSA.PutStatic => false

      case n: SSA.GetArray => false
      case n: SSA.PutArray => false

      case n: SSA.NewArray => false
      case n: SSA.MultiANewArray => false

      case n: SSA.BinOp =>
        n.opcode match {
          case SSA.BinOp.IDIV | SSA.BinOp.IREM | SSA.BinOp.LDIV | SSA.BinOp.LREM => false
          case _ => true
        }
      case n: SSA.UnaOp => true

      case n: SSA.InvokeDynamic => n.bootstrap == Util.makeConcatWithConstants
      case n: SSA.Invoke =>
        if (n.srcs.exists(!optResult.inferred.contains(_))) true
        else {

          val key = n.inferredSig(optResult.inferred)
          val default = callSet(key)
          if (classManager.loadClass(key.method.cls).isEmpty) false
          else if (n.isInstanceOf[SSA.InvokeSpecial]) methodProps.get(key).fold(default)(_.pure)
          else resolveProps(key).fold(default)(_.pure)
        }

      case p: SSA.Phi => true
    }
  }

  def checkSubclass(cls1: JType.Cls, cls2: JType.Cls) = classManager.mergeTypes(Seq(cls1, cls2)) == cls2
}

object ProgramAnalyzer {
  case class StepResult(edges: Seq[CallEdge] = Nil,
                        staticFieldReferencedClasses: Seq[JType.Cls] = Nil,
                        calledSignatures: Seq[InferredSig] = Nil,
                        evaluated: Seq[(InferredSig, SSA.Invoke, IType)] = Nil)
  case class CallEdge(caller: InferredSig, node: Option[SSA.Invoke], called: InferredSig){
    if (called.method.name == "<clinit>") assert(node.isEmpty)
  }
  case class ProgramResult(visitedMethods: collection.Map[InferredSig, MethodResult],
                           visitedResolved: collection.Map[InferredSig, Properties],
                           staticFieldReferencedClasses: collection.Set[JType.Cls])
  case class MethodResult(methodBody: MethodBody,
                          inferred: mutable.LinkedHashMap[SSA.Val, IType],
                          liveBlocks: Set[SSA.Block],
                          props: Properties)

  case class Properties(inferredReturn: IType,
                        pure: Boolean,
                        liveArgs: Set[Int])


  def handleNew(isig: InferredSig,
                n: SSA.New,
                classManager: ClassManager,
                callGraph: Iterable[CallEdge]): ProgramAnalyzer.StepResult = {
    classManager.loadClass(n.cls)
    val superClasses = classManager.getAllSupertypes(n.cls).toSet

    val superClassCallList = for {
      edge <- callGraph
      node <- edge.node
      if !node.isInstanceOf[SSA.InvokeSpecial]
      if superClasses.contains(node.cls)
    } yield (node, edge)

    val superClassMethodMap = superClassCallList
      .groupBy(t => (t._1.name, t._1.desc))
      .map{case (k, v) => (k, v.map(_._2))}
    ProgramAnalyzer.StepResult(
      for {
        loadedClsNode <- classManager.loadClass(n.cls).toSeq
        mn <- loadedClsNode.methods.asScala
        if (mn.access & (Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC)) == 0
        edge <- superClassMethodMap.getOrElse((mn.name, Desc.read(mn.desc)), Nil)
        node <- edge.node
      } yield {
        val msig = edge.called.method.copy(cls = n.cls)
        ProgramAnalyzer.CallEdge(edge.caller, Some(node), edge.called.copy(method = msig))
      }
    )
  }

  def handleFieldReference(isig: InferredSig,
                           cls: JType.Cls,
                           static: Boolean,
                           analyzeClinits: JType.Cls => Seq[InferredSig]): ProgramAnalyzer.StepResult = {

    val clinits = analyzeClinits(cls)
    ProgramAnalyzer.StepResult(
      edges = clinits.map(ProgramAnalyzer.CallEdge(isig, None, _)),
      staticFieldReferencedClasses = if (static) Seq(cls) else Nil
    )
  }

  def handleInvoke(isig: InferredSig,
                   currentAnalysis: MethodAnalyzer[IType],
                   invoke: SSA.Invoke,
                   classManager: ClassManager,
                   analyzeClinits: JType.Cls => Seq[InferredSig],
                   isCalledFrom: (InferredSig, InferredSig) => Boolean,
                   invokeEvaluated: Option[IType],
                   getProp: InferredSig => Option[Properties]): ProgramAnalyzer.StepResult = {
    val calledSig = invoke.inferredSig(currentAnalysis.evaluated(_))

    if (isCalledFrom(isig, calledSig)) {
      ProgramAnalyzer.StepResult(
        edges = Seq(ProgramAnalyzer.CallEdge(calledSig, Some(invoke), isig)),
        calledSignatures = Seq(calledSig),
        evaluated = Seq((isig, invoke, IType.Bottom))
      )
    } else if (classManager.loadClass(calledSig.method.cls).isEmpty) {
      ProgramAnalyzer.StepResult(
        edges = Nil,
        calledSignatures = Nil,
        evaluated = Seq((isig, invoke, calledSig.method.desc.ret))
      )
    } else if (invoke.isInstanceOf[SSA.InvokeSpecial]) {
      val calledSig2 =
        if (invoke.name != "<init>") calledSig
        else calledSig.copy(inferred = calledSig.method.desc.args)

      ProgramAnalyzer.StepResult(
        edges = Seq(ProgramAnalyzer.CallEdge(isig, Some(invoke), calledSig2)),
        calledSignatures = Seq(calledSig2)
      )
    } else classManager.resolvePossibleSigs(calledSig.method) match {
      case None =>
        ProgramAnalyzer.StepResult(
          edges = Seq(ProgramAnalyzer.CallEdge(isig, Some(invoke), calledSig)),
          calledSignatures = Seq(calledSig)
        )

      case Some(subSigs0) =>
        val loaded = subSigs0.map(classManager.loadMethod)
        if (loaded.forall(_.isEmpty)){
          ProgramAnalyzer.StepResult(
            evaluated = Seq((isig, invoke, calledSig.method.desc.ret)),
            calledSignatures = Seq(calledSig)
          )
        }else {
          val subSigs = subSigs0.filter { subSig =>
            classManager.loadMethod(subSig).exists(_.instructions.size() != 0)
          }

          assert(subSigs.nonEmpty)

          val clinits = subSigs.flatMap(subSig => analyzeClinits(subSig.cls))
          val subs = subSigs.map(subSig => InferredSig(subSig, calledSig.inferred))

          val merged = classManager.mergeTypes(
            invokeEvaluated.toSeq ++
            subs.flatMap(getProp(_).toSeq).map(_.inferredReturn)
          )

          ProgramAnalyzer.StepResult(
            edges =
              subs.map(ret => ProgramAnalyzer.CallEdge(isig, Some(invoke), ret)) ++
              clinits.map(ret => ProgramAnalyzer.CallEdge(isig, None, ret)),
            evaluated = Seq((isig, invoke, merged)),
            calledSignatures = Seq(calledSig)
          )

        }
    }
  }

  def dummyProps(originalSig: MethodSig, optimistic: Boolean) = ProgramAnalyzer.Properties(
    if (optimistic) IType.Bottom else originalSig.desc.ret,
    optimistic,
    if (optimistic) Set.empty
    else Range.inclusive(0, originalSig.desc.args.length + (if (originalSig.static) 0 else 1)).toSet
  )

  def analyzeBlockStructure(methodBody: MethodBody) = {
    val controlFlowEdges = Renderer.findControlFlowGraph(methodBody)
    val startBlock = (controlFlowEdges.map(_._1).toSet -- controlFlowEdges.map(_._2)).head.asInstanceOf[SSA.Block]
    val allBlocks = controlFlowEdges
      .flatMap { case (k, v) => Seq(k, v) }
      .collect { case b: SSA.Block => b }

    val blockEdges = controlFlowEdges.flatMap {
      case (k: SSA.Block, v: SSA.Jump) => Nil
      case (k: SSA.Jump, v: SSA.Block) => Seq(k.block -> v)
      case (k: SSA.Block, v: SSA.Block) => Seq(k -> v)
    }

    (controlFlowEdges, startBlock, allBlocks, blockEdges)
  }
}
