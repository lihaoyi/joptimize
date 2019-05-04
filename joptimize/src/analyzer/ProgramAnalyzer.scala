package joptimize.analyzer
import collection.JavaConverters._
import joptimize.frontend.{ClassManager, Frontend}
import joptimize.{Logger, Util}
import joptimize.model._
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

  def apply() = {

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

    val visitedMethods = mutable.LinkedHashMap.empty[InferredSig, ProgramAnalyzer.Result]
    for((k, props) <- methodProps) {
      visitedMethods(k) = ProgramAnalyzer.Result(
        frontend.cachedMethodBodies(k).get,
        analyses(k).evaluated,
        analyses(k).liveBlocks.toSet,
        props
      )
    }
    ProgramAnalyzer.GlobalResult(
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
    val currentCallSet = callStackSets(isig) ++ Set(isig)

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

    val newCurrent: Seq[InferredSig] = step match {
      case MethodAnalyzer.Step.Continue(nodes) =>
        val flat = nodes.flatMap{
          case n: SSA.GetField => handleFieldReference(isig, currentCallSet, n.owner, static = false)
          case n: SSA.PutField => handleFieldReference(isig, currentCallSet, n.owner, static = false)
          case n: SSA.GetStatic => handleFieldReference(isig, currentCallSet, n.cls, static = true)
          case n: SSA.PutStatic => handleFieldReference(isig, currentCallSet, n.cls, static = true)

          case n: SSA.New => handleNew(isig, n)
          case invoke: SSA.Invoke => handleInvoke(isig, currentCallSet, currentAnalysis, invoke)
          case indy: SSA.InvokeDynamic =>
            if (indy.bootstrap == Util.metafactory || indy.bootstrap == Util.altMetafactory){

              val target = indy.bootstrapArgs(1).asInstanceOf[SSA.InvokeDynamic.HandleArg]
              val calledDesc = indy.bootstrapArgs(0).asInstanceOf[SSA.InvokeDynamic.MethodArg].i
              val targetSig = MethodSig(
                target.cls,
                target.name,
                target.desc,
                target.tag == Opcodes.H_INVOKESTATIC
              )

              val retCls = indy.desc.ret.asInstanceOf[JType.Cls]
              val msig = MethodSig(
                indy.desc.ret.asInstanceOf[JType.Cls],
                indy.name,
                calledDesc,
                target.tag == Opcodes.H_INVOKESTATIC
              )
              classManager.seenLambdas(msig) =
                classManager.seenLambdas.getOrElse(msig, Set()) + (indy -> targetSig)
              currentAnalysis.evaluated(indy) = retCls
//              pprint.log(targetSig)
              val res = handleLambda(isig, retCls, targetSig)
//              pprint.log(res)
              res
            }else if(indy.bootstrap == Util.makeConcatWithConstants){
              currentAnalysis.evaluated(indy) = JType.Cls("java/lang/String")
              Seq(isig)
            } else{
              pprint.log(indy.bootstrap)
              pprint.log(indy.bootstrapArgs)
              pprint.log(indy.bootstrapArgs.map(_.getClass))
              pprint.log(indy.desc)
              pprint.log(indy.name)
              ???
            }
//            handleInvoke(isig, currentCallSet, currentAnalysis, invoke)
          case _ => Nil
        }.distinct

        flat ++ Seq(isig)

      case MethodAnalyzer.Step.Done() =>
        handleReturn(isig, currentCallSet, inferredLog, currentAnalysis)
    }


    newCurrent.foreach(current.add)
  }

  def handleNew(isig: InferredSig, n: SSA.New) = {
    classManager.loadClass(n.cls)
    val superClasses = classManager.resolveSuperTypes(n.cls)

    val newMethodOverrides = for {
      cls <- superClasses
      loadedClsNode <- classManager.loadClass(cls).toSeq
      mn <- loadedClsNode.methods.asScala
      if (mn.access & (Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC)) == 0
      msig = MethodSig(n.cls, mn.name, Desc.read(mn.desc), false)
      edge <- callGraph
      node <- edge.node
      if !node.isInstanceOf[SSA.InvokeSpecial]
      if edge.called.method.name == msig.name
      if edge.called.method.desc == msig.desc
      if classManager.mergeTypes(Seq(node.cls, msig.cls)) == node.cls
    } yield {
      analyses(edge.caller).invalidateWorkList.add(MethodAnalyzer.Invalidate.Invoke(node))
      callGraph.add(ProgramAnalyzer.CallEdge(edge.caller, Some(node), edge.called.copy(method = msig)))
      addToCallSet(edge.called.copy(method = msig), callStackSets(edge.caller))
      edge.called.copy(method = msig)
    }
    newMethodOverrides ++ Seq(isig)
  }

  def handleLambda(isig: InferredSig, retCls: JType.Cls, msig: MethodSig) = {
//    pprint.log(retCls)
    val loaded = classManager.loadClass(retCls)
//    pprint.log(loaded)
    val superClasses = classManager.resolveSuperTypes(retCls)

    val newMethodOverrides = for {
      cls <- superClasses
      loadedClsNode <- classManager.loadClass(cls).toSeq
      edge <- callGraph
      node <- edge.node
      if !node.isInstanceOf[SSA.InvokeSpecial]
      if edge.called.method.name == msig.name
      if edge.called.method.desc == msig.desc
      if classManager.mergeTypes(Seq(node.cls, msig.cls)) == node.cls
    } yield {
      analyses(edge.caller).invalidateWorkList.add(MethodAnalyzer.Invalidate.Invoke(node))
      callGraph.add(ProgramAnalyzer.CallEdge(edge.caller, Some(node), edge.called.copy(method = msig)))
      addToCallSet(edge.called.copy(method = msig), callStackSets(edge.caller))
      edge.called.copy(method = msig)
    }
    newMethodOverrides ++ Seq(isig)
  }

  def handleReturn(isig: InferredSig,
                   currentCallSet: Set[InferredSig],
                   inferredLog: Logger.InferredMethod,
                   currentAnalysis: MethodAnalyzer[IType]) = {
//    println("DONE")
    val optimisticResult = currentAnalysis.apply()
    val retTypes = currentAnalysis.apply()
      .inferredReturns
      .flatMap(_._2)
      .filter(_ != JType.Prim.V)
      .toSeq

    val inferredReturn =
      if (retTypes.isEmpty) JType.Prim.V
      else classManager.mergeTypes(retTypes)

    val props = ProgramAnalyzer.Properties(
      inferredReturn,
      computePurity(optimisticResult, currentCallSet) &&
        !(isig.method.static && classManager.loadMethod(MethodSig(isig.method.cls, "<clinit>", Desc(Nil, JType.Prim.V), true)).nonEmpty),
      optimisticResult.inferred.collect { case (a: SSA.Arg, _) => a.index }.toSet
    )

    val unchanged = methodProps.get(isig).contains(props)

    methodProps(isig) = props

    inferredLog.pprint(optimisticResult.inferred)
    inferredLog.pprint(optimisticResult.liveBlocks)

    val returnEdges = callGraph.iterator.filter(_.called == isig).toArray

    for (edge <- returnEdges) {
      for (node <- edge.node) {
        val mergedType = classManager.mergeTypes(
          analyses(edge.caller).evaluated.get(node).toSeq ++ Seq(props.inferredReturn)
        )

        if (!analyses(edge.caller).evaluated.get(node).contains(props.inferredReturn)) {
          analyses(edge.caller).invalidateWorkList.add(MethodAnalyzer.Invalidate.Invoke(node))
        }

        analyses(edge.caller).evaluated(node) = mergedType
      }
    }
    val filtered =
      if (!unchanged) returnEdges.map(_.caller)
      else returnEdges.map(_.caller).filter(!methodProps.contains(_))

//    pprint.log(filtered)
    filtered
  }

  def handleFieldReference(isig: InferredSig,
                           currentCallSet: Set[InferredSig],
                           cls: JType.Cls,
                           static: Boolean) = {
    if (static) staticFieldReferencedClasses.add(cls)
    val clinits = analyzeClinits(Seq(cls))

    if (clinits.isEmpty) Seq(isig)
    else {
      clinits.foreach(clinit => callGraph.add(ProgramAnalyzer.CallEdge(isig, None, clinit)))
      clinits.foreach(addToCallSet(_, currentCallSet))
      clinits
    }
  }

  def handleInvoke(isig: InferredSig,
                   currentCallSet: Set[InferredSig],
                   currentAnalysis: MethodAnalyzer[IType],
                   invoke: SSA.Invoke) = {
    val calledSig = invoke.inferredSig(currentAnalysis.evaluated(_))

//    pprint.log(isig)
//    pprint.log(invoke.sig)
    if (currentCallSet(calledSig)) {
      calledSignatures.add(calledSig)
      callGraph.add(ProgramAnalyzer.CallEdge(calledSig, Some(invoke), isig))
      addToCallSet(calledSig, currentCallSet)
      analyses(isig).evaluated(invoke) = IType.Bottom
      methodProps(calledSig) = ProgramAnalyzer.dummyProps(calledSig.method, true)

      Seq(isig)
    } else if (classManager.loadClass(calledSig.method.cls).isEmpty) {
      analyses(isig).evaluated(invoke) = calledSig.method.desc.ret
      Seq(isig)
    } else if (invoke.isInstanceOf[SSA.InvokeSpecial]) {
      calledSignatures.add(calledSig)
      callGraph.add(ProgramAnalyzer.CallEdge(isig, Some(invoke), calledSig))
      addToCallSet(calledSig, currentCallSet)
      Seq(calledSig)
    } else classManager.resolvePossibleSigs(calledSig.method) match {
      case None =>
        calledSignatures.add(calledSig)
        callGraph.add(ProgramAnalyzer.CallEdge(isig, Some(invoke), calledSig))
        addToCallSet(calledSig, currentCallSet)
        Seq(isig).filter(!methodProps.contains(_))
      case Some(subSigs0) =>
        calledSignatures.add(calledSig)
        val subSigs = subSigs0.filter { subSig =>
          classManager.loadMethod(subSig).exists(_.instructions.size() != 0)
        }

        assert(subSigs.nonEmpty)

        val (clinitss, subss) = subSigs
          .map { subSig =>
            val clinits = analyzeClinits(Seq(subSig.cls))
//            pprint.log(subSig)
//            pprint.log(calledSig.inferred)
            val prefix = subSig.desc.args.take(subSig.desc.args.length - calledSig.inferred.length)
            (clinits, Seq(InferredSig(subSig, prefix ++ calledSig.inferred)))
          }
          .unzip
        val clinits = clinitss.flatten
        val subs = subss.flatten

        val rets = (clinits ++ subs).filter(!methodProps.contains(_))

        analyses(isig).evaluated(invoke) = classManager.mergeTypes(
          analyses(isig).evaluated.get(invoke).toSeq ++
            subs.flatMap(methodProps.get).map(_.inferredReturn)
        )
        if (rets.isEmpty) Seq(isig)
        else {
          rets.foreach(ret => callGraph.add(ProgramAnalyzer.CallEdge(isig, Some(invoke), ret)))

          rets.foreach(addToCallSet(_, currentCallSet))
          rets
        }

    }
  }

  def resolveProps(isig: InferredSig) = {
    classManager.resolvePossibleSigs(isig.method).map{ resolved =>
      val copied = resolved.map{ m =>
        val prefix = m.desc.args.take(m.desc.args.length - isig.inferred.length)
        isig.copy(method = m, inferred = prefix ++ isig.inferred)
      }

      val resolvedProps = copied.flatMap(methodProps.get)
      ProgramAnalyzer.Properties(
        classManager.mergeTypes(resolvedProps.map(_.inferredReturn)),
        resolvedProps.forall(_.pure),
        resolvedProps.flatMap(_.liveArgs).toSet
      )
    }
  }

  def analyzeClinits(classes: Seq[JType.Cls]) = {

    for {
      cls <- classes
      val clinit = MethodSig(cls, "<clinit>", Desc(Nil, JType.Prim.V), true)
      if classManager.loadMethod(clinit).isDefined
      if !analyses.contains(InferredSig(clinit, Nil))
    } yield InferredSig(clinit, Nil)
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
      ITypeBrancher
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

      //    case n: SSA.InvokeDynamic => computeMethodSig(n, n.srcs.map(inferences))
      case p: SSA.Phi => true
    }
  }

  def checkSubclass(cls1: JType.Cls, cls2: JType.Cls) = classManager.mergeTypes(Seq(cls1, cls2)) == cls2
}

object ProgramAnalyzer {
  case class CallEdge(caller: InferredSig, node: Option[SSA.Invoke], called: InferredSig)
  case class GlobalResult(visitedMethods: collection.Map[InferredSig, Result],
                          visitedResolved: collection.Map[InferredSig, Properties],
                          staticFieldReferencedClasses: collection.Set[JType.Cls])
  case class Result(methodBody: MethodBody,
                    inferred: mutable.LinkedHashMap[SSA.Val, IType],
                    liveBlocks: Set[SSA.Block],
                    props: Properties)

  case class Properties(inferredReturn: IType,
                        pure: Boolean,
                        liveArgs: Set[Int])

  def dummyResult(originalSig: MethodSig, optimistic: Boolean) = ProgramAnalyzer.Result(
    new MethodBody(Nil, Nil),
    mutable.LinkedHashMap.empty,
    Set.empty,
    dummyProps(originalSig, optimistic)
  )

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
