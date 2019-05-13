package joptimize.analyzer
import collection.JavaConverters._
import joptimize.frontend.{ClassManager, Frontend}
import joptimize.{Logger, Util}
import joptimize.model._
import joptimize.viewer.model.LogMessage
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.{Handle, Opcodes}

import scala.collection.mutable

/**
  * Optimistically walks an entire program, comprised of many methods linked together
  * via a call graph. Constructs the method call graph in memory as `callGraph`, and only
  * maintains the current set of analysis-in-progress methods, relying on the `callGraph`
  * to decide where to return to after each method analysis is complete.
  */
class ProgramAnalyzer(entrypoints: Seq[MethodSig],
                      val classManager: ClassManager,
                      globalLog: Logger.Global,
                      frontend: Frontend) extends ProgramAnalyzer.HandlerApi {

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

  def addCallEdge(edge: ProgramAnalyzer.CallEdge) = {
    callGraph.add(edge)
    current.add(edge.called)
    val newCallSet = callStackSets(edge.caller) + edge.caller
    if (callStackSets.contains(edge.called)) callStackSets(edge.called) ++= newCallSet
    else callStackSets(edge.called) = newCallSet
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
      globalLog.inferredMethod(k).pprint(analyses(k).inferredReturns.keys)
      globalLog.inferredMethod(k).pprint(analyses(k).inferredThrows.keys)
      visitedMethods(k) = ProgramAnalyzer.MethodResult(
        frontend.cachedMethodBodies(k).get,
        analyses(k).evaluated,
        analyses(k).liveBlocks.toSet,
        props,
        analyses(k).inferredReturns.keys.toSet ++ analyses(k).inferredThrows.keys.toSet
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
      staticFieldReferencedClasses,
      callGraph.toSeq
    )
  }

  /**
    * Performs a single step of the whole-program analysis.
    *
    * Delegates most of the business logic to static `ProgramAnalyzer.handle*` functions,
    * which are provided a limited [[ProgramAnalyzer.HandlerApi]] of utilities they can use
    * and return a [[ProgramAnalyzer.StepResult]] struct dictating what actions should be
    * performed at each step. This helps keep the messy `handle*` logic separate from the
    * also-messy mutable state and related code in the [[ProgramAnalyzer]] instance.
    */
  def step() = {
//    println()
//    println(pprint.apply(current.map(_.method.toString.stripPrefix("joptimize.examples.simple."))))
    val isig = current.maxBy(callStackSets(_).size)

    current.remove(isig)
    globalLog.pprint(isig.toString)
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

//    globalLog.pprint(step)
    val results: Seq[ProgramAnalyzer.StepResult] = step match {
      case MethodAnalyzer.Step.Continue(nodes) =>
        nodes.map{
          case n: SSA.GetField => ProgramAnalyzer.handleFieldReference(isig, n.owner, false, this)
          case n: SSA.PutField => ProgramAnalyzer.handleFieldReference(isig, n.owner, false, this)
          case n: SSA.GetStatic => ProgramAnalyzer.handleFieldReference(isig, n.cls, true, this)
          case n: SSA.PutStatic => ProgramAnalyzer.handleFieldReference(isig, n.cls, true, this)

          case n: SSA.New => ProgramAnalyzer.handleNew(isig, n, this)
          case invoke: SSA.Invoke => ProgramAnalyzer.handleInvoke(isig, currentAnalysis, invoke, this)
          case indy: SSA.InvokeDynamic =>
            if (indy.bootstrap == Util.metafactory || indy.bootstrap == Util.altMetafactory) ???
            else if(indy.bootstrap == Util.makeConcatWithConstants){
              ProgramAnalyzer.StepResult(evaluated = Seq((isig, indy, JType.Cls("java/lang/String"))))
            } else ???

          case _ => ProgramAnalyzer.StepResult()
        }


      case MethodAnalyzer.Step.Done() =>
        Seq(ProgramAnalyzer.handleReturn(isig, inferredLog, currentAnalysis, this))
    }

    for(result <- results){
      result.edges.foreach(addCallEdge)
      result.staticFieldReferencedClasses.foreach(staticFieldReferencedClasses.add)
      result.calledSignatures.foreach(calledSignatures.add)
      for((sig, invoke, tpe) <- result.evaluated) {
        val merged = classManager.mergeTypes(Seq(tpe) ++ analyses(sig).evaluated.get(invoke))
        if (!analyses(sig).evaluated.get(invoke).contains(merged)){
          analyses(sig).evaluated(invoke) = merged
          invoke match{
            case n: SSA.Invoke =>
              analyses(sig).invalidateWorkList.add(MethodAnalyzer.Invalidate.Invoke(n))
            case _ => // do nothing
          }
          current.add(sig)
        }
      }
      for((sig, props) <- result.setProps) {
        methodProps(sig) = props
      }
    }

    if (!step.isInstanceOf[MethodAnalyzer.Step.Done[_]]) current.add(isig)
  }
  def getMethodProps(isig: InferredSig): Option[ProgramAnalyzer.Properties] = methodProps.get(isig)

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

  def isCalledFrom(isig: InferredSig, calledSig: InferredSig) = callStackSets(isig)(calledSig)

  def getAnalysesEvaluated(isig: InferredSig, invoke: SSA.Invoke) = analyses(isig).evaluated.get(invoke)
}

object ProgramAnalyzer {
  case class StepResult(edges: Seq[CallEdge] = Nil,
                        staticFieldReferencedClasses: Seq[JType.Cls] = Nil,
                        calledSignatures: Seq[InferredSig] = Nil,
                        evaluated: Seq[(InferredSig, SSA.Val, IType)] = Nil,
                        setProps: Seq[(InferredSig, Properties)] = Nil)
  case class CallEdge(caller: InferredSig, node: Option[SSA.Invoke], called: InferredSig){
    if (called.method.name == "<clinit>") assert(node.isEmpty)
  }
  case class ProgramResult(visitedMethods: mutable.LinkedHashMap[InferredSig, MethodResult],
                           visitedResolved: collection.Map[InferredSig, Properties],
                           staticFieldReferencedClasses: collection.Set[JType.Cls],
                           callGraph: Seq[CallEdge])
  case class MethodResult(methodBody: MethodBody,
                          inferred: mutable.LinkedHashMap[SSA.Val, IType],
                          liveBlocks: Set[SSA.Block],
                          props: Properties,
                          liveTerminals: Set[SSA.Jump])

  case class Properties(inferredReturn: IType,
                        pure: Boolean,
                        liveArgs: Set[Int])

  trait HandlerApi{
    def classManager: ClassManager
    def callGraph: Iterable[CallEdge]
    def analyzeClinits(cls: JType.Cls): Seq[InferredSig]
    def getMethodProps(isig: InferredSig): Option[Properties]
    def resolveProps(isig: InferredSig): Option[Properties]
    def isCalledFrom(isig: InferredSig, calledSig: InferredSig): Boolean
    def getAnalysesEvaluated(isig: InferredSig, invoke: SSA.Invoke): Option[IType]
  }

  def handleNew(isig: InferredSig, n: SSA.New, api: HandlerApi): StepResult = {
    api.classManager.loadClass(n.cls)
    val superClasses = api.classManager.getAllSupertypes(n.cls).toSet

    val superClassCallList = for {
      edge <- api.callGraph
      node <- edge.node
      if !node.isInstanceOf[SSA.InvokeSpecial]
      if superClasses.contains(node.cls)
    } yield (node, edge)

    val superClassMethodMap = superClassCallList
      .groupBy(t => (t._1.name, t._1.desc))
      .map{case (k, v) => (k, v.map(_._2))}
    StepResult(
      for {
        loadedClsNode <- api.classManager.loadClass(n.cls).toSeq
        mn <- loadedClsNode.methods.asScala
        if (mn.access & (Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC)) == 0
        edge <- superClassMethodMap.getOrElse((mn.name, Desc.read(mn.desc)), Nil)
        node <- edge.node
      } yield {
        val msig = edge.called.method.copy(cls = n.cls)
        CallEdge(edge.caller, Some(node), edge.called.copy(method = msig))
      }
    )
  }

  def handleFieldReference(isig: InferredSig,
                           cls: JType.Cls,
                           static: Boolean,
                           api: HandlerApi): StepResult = {

    val clinits = api.analyzeClinits(cls)
    StepResult(
      edges = clinits.map(CallEdge(isig, None, _)),
      staticFieldReferencedClasses = if (static) Seq(cls) else Nil
    )
  }

  def handleInvoke(isig: InferredSig,
                   currentAnalysis: MethodAnalyzer[IType],
                   invoke: SSA.Invoke,
                   api: HandlerApi): StepResult = {
    val calledSig = invoke.inferredSig(currentAnalysis.evaluated(_))

    if (api.isCalledFrom(isig, calledSig)) {
      StepResult(
        edges = Seq(CallEdge(isig, Some(invoke), calledSig)),
        calledSignatures = Seq(calledSig),
        evaluated = Seq((isig, invoke, IType.Bottom))
      )
    } else if (api.classManager.loadClass(calledSig.method.cls).isEmpty) {
      StepResult(evaluated = Seq((isig, invoke, calledSig.method.desc.ret)))
    } else if (invoke.isInstanceOf[SSA.InvokeSpecial]) {
      val calledSig2 =
        if (invoke.name != "<init>") calledSig
        else calledSig.copy(inferred = calledSig.method.desc.args)

      StepResult(
        edges = Seq(CallEdge(isig, Some(invoke), calledSig2)),
        calledSignatures = Seq(calledSig2)
      )
    } else api.classManager.resolvePossibleSigs(calledSig.method) match {
      case None =>
        StepResult(
          edges = Seq(CallEdge(isig, Some(invoke), calledSig)),
          calledSignatures = Seq(calledSig)
        )

      case Some(subSigs0) =>
        val loaded = subSigs0.map(api.classManager.loadMethod)
        if (loaded.forall(_.isEmpty)){
          StepResult(
            evaluated = Seq((isig, invoke, calledSig.method.desc.ret)),
            calledSignatures = Seq(calledSig)
          )
        }else {
          val subSigs = subSigs0.filter { subSig =>
            api.classManager.loadMethod(subSig).exists(_.instructions.size() != 0)
          }

          assert(subSigs.nonEmpty)

          val clinits = subSigs.flatMap(subSig => api.analyzeClinits(subSig.cls))
          val subs = subSigs.map(subSig => InferredSig(subSig, calledSig.inferred))

          val merged = api.classManager.mergeTypes(
            api.getAnalysesEvaluated(isig, invoke).toSeq ++
            subs.flatMap(api.getMethodProps(_).toSeq).map(_.inferredReturn)
          )

          StepResult(
            edges =
              subs.map(ret => CallEdge(isig, Some(invoke), ret)) ++
              clinits.map(ret => CallEdge(isig, None, ret)),
            evaluated = Seq((isig, invoke, merged)),
            calledSignatures = Seq(calledSig)
          )

        }
    }
  }

  def handleReturn(isig: InferredSig,
                   inferredLog: Logger.InferredMethod,
                   currentAnalysis: MethodAnalyzer[IType],
                   api: HandlerApi) = {
    //    println("DONE")

    val retTypes = currentAnalysis.inferredReturns
      .valuesIterator
      .toSeq

    val inferredReturn = api.classManager.mergeTypes(retTypes)

    val computedPurity = computePurity(isig, currentAnalysis.evaluated, api)

    val clinitSig = MethodSig(isig.method.cls, "<clinit>", Desc(Nil, JType.Prim.V), true)
    val props = Properties(
      inferredReturn,
      computedPurity && !(isig.method.static && api.classManager.loadMethod(clinitSig).nonEmpty),
      currentAnalysis.evaluated.collect { case (a: SSA.Arg, _) => a.index }.toSet
    )

    val evaluated = for {
      edge <- api.callGraph
      if edge.called == isig
      node <- edge.node
    } yield (edge.caller, node, props.inferredReturn)

    StepResult(
      evaluated = evaluated.toSeq,
      setProps = Seq(isig -> props)
    )
  }

  def computePurity(isig: InferredSig,
                    inferred: mutable.LinkedHashMap[SSA.Val, IType],
                    api: HandlerApi) = {
    inferred.keysIterator.forall {
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
        if (n.srcs.exists(!inferred.contains(_))) true
        else {

          val key = n.inferredSig(inferred)
          val default = api.isCalledFrom(isig, key)
          if (api.classManager.loadClass(key.method.cls).isEmpty) false
          else if (n.isInstanceOf[SSA.InvokeSpecial]) api.getMethodProps(key).fold(default)(_.pure)
          else api.resolveProps(key).fold(default)(_.pure)
        }

      case p: SSA.Phi => true
    }
  }

  def dummyProps(originalSig: MethodSig, optimistic: Boolean) = Properties(
    if (optimistic) IType.Bottom else originalSig.desc.ret,
    optimistic,
    if (optimistic) Set.empty
    else Range.inclusive(0, originalSig.desc.args.length + (if (originalSig.static) 0 else 1)).toSet
  )
}
