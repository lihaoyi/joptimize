package joptimize.analyzer
import collection.JavaConverters._
import joptimize.algorithms.{Dominator, MultiBiMap, Scheduler}
import joptimize.frontend.{ClassManager, Frontend}
import joptimize.{Logger, Util}
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import joptimize.optimize._
import org.objectweb.asm.Opcodes

import scala.collection.mutable

class Analyzer(entrypoints: Seq[MethodSig],
               classManager: ClassManager,
               globalLog: Logger.Global,
               frontend: Frontend){
  val methodProps = mutable.LinkedHashMap.empty[InferredSig, Analyzer.Properties]
  val calledSignatures = mutable.LinkedHashSet.empty[InferredSig]
  val visitedClasses = mutable.LinkedHashSet.empty[JType.Cls]

  /**
    * Edges from the caller to the called
    *
    * This is kept up to date on a node-by-node basis during the traversal
    */
  var callerGraph = List.empty[Analyzer.CallEdge]

  val analyses = mutable.LinkedHashMap.empty[InferredSig, OptimisticAnalyze[IType]]

  val current = mutable.LinkedHashSet.empty[InferredSig]

  val callSets = mutable.Map.empty[InferredSig, Set[InferredSig]]
  def addToCallSet(k: InferredSig, v: Set[InferredSig]) = {
    if (callSets.contains(k)) callSets(k) ++= v
    else callSets(k) = v
  }

  def apply() = {

    for (ep <- entrypoints) {
      val isig = InferredSig(ep, ep.desc.args)
      current.add(isig)
      callSets(isig) = Set()
      calledSignatures.add(isig)
    }

    while(current.nonEmpty){
      step()
    }

    for(m <- methodProps.keysIterator){
      calledSignatures.add(m)
    }
    val visitedResolved = for{
      isig <- calledSignatures
      resolved <- resolveProps(isig)
    } yield (isig, resolved)

    val visitedMethods = mutable.LinkedHashMap.empty[InferredSig, Analyzer.Result]
    for((k, props) <- methodProps) {
      visitedMethods(k) = Analyzer.Result(
        frontend.cachedMethodBodies(k).get,
        analyses(k).evaluated,
        analyses(k).inferredBlocks.toSet,
        props
      )
    }
    Analyzer.GlobalResult(
      visitedMethods,
      visitedResolved.toMap,
      visitedClasses
    )
  }

  def step() = {
//    println()
//    println(pprint.apply(current.map(_.method.toString.stripPrefix("joptimize.examples.simple."))))
    val isig = current.maxBy(callSets(_).size)
    val currentCallSet = callSets(isig) ++ Set(isig)

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
      case OptimisticAnalyze.Step.Continue(nodeOpt) =>
        nodeOpt match {
          case Some(n: SSA.GetField) => handleFieldReference(isig, currentCallSet, n.owner)
          case Some(n: SSA.PutField) => handleFieldReference(isig, currentCallSet, n.owner)
          case Some(n: SSA.GetStatic) => handleFieldReference(isig, currentCallSet, n.cls)
          case Some(n: SSA.PutStatic) => handleFieldReference(isig, currentCallSet, n.cls)

          case Some(n: SSA.New) =>

            classManager.loadClass(n.cls)
            val superClasses = classManager.resolveSuperTypes(n.cls)
            val potentialSigs = for {
              cls <- superClasses
              loadedClsNode <- classManager.loadClass(cls).toSeq
              mn <- loadedClsNode.methods.asScala
              if (mn.access & (Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC)) == 0
              msig = MethodSig(n.cls, mn.name, Desc.read(mn.desc), false)
            }yield msig

            val newMethodOverrides = for{
              msig <- potentialSigs
              edge <- callerGraph
              node <- edge.node
              if !node.isInstanceOf[SSA.InvokeSpecial]
              if edge.called.method.name == msig.name
              if edge.called.method.desc == msig.desc
              if classManager.mergeTypes(Seq(node.cls, msig.cls)) == node.cls
            } yield {
              analyses(edge.caller).invalidations.add(OptimisticAnalyze.Invalidate.Force(node))
              callerGraph ::= Analyzer.CallEdge(edge.caller, Some(node), edge.called.copy(method = msig))
              addToCallSet(edge.called.copy(method = msig), callSets(edge.caller))
              edge.called.copy(method = msig)
            }

            newMethodOverrides ++ Seq(isig)
          case Some(invoke: SSA.Invoke) => handleInvoke(isig, currentCallSet, currentAnalysis, invoke)
          case _ => Seq(isig)
        }


      case OptimisticAnalyze.Step.Done() =>
        handleReturn(isig, currentCallSet, inferredLog, currentAnalysis)
    }

    current.remove(isig)
    for(c <- newCurrent){
      current.add(c)
    }
  }

  def handleReturn(isig: InferredSig,
                   currentCallSet: Set[InferredSig],
                   inferredLog: Logger.InferredMethod,
                   currentAnalysis: OptimisticAnalyze[IType]) = {
//    println("DONE")
    val optimisticResult = currentAnalysis.apply()
    val retTypes = currentAnalysis.apply().inferredReturns.filter(_ != JType.Prim.V)
    val inferredReturn =
      if (retTypes.isEmpty) JType.Prim.V
      else classManager.mergeTypes(retTypes)

    val props = Analyzer.Properties(
      inferredReturn,
      computePurity(optimisticResult, currentCallSet) &&
        !(isig.method.static && classManager.loadMethod(MethodSig(isig.method.cls, "<clinit>", Desc(Nil, JType.Prim.V), true)).nonEmpty),
      optimisticResult.inferred.collect { case (a: SSA.Arg, _) => a.index }.toSet
    )

    val unchanged = methodProps.get(isig).contains(props)

    methodProps(isig) = props

    inferredLog.pprint(optimisticResult.inferred)
    inferredLog.pprint(optimisticResult.liveBlocks)

    val returnEdges = callerGraph.filter(_.called == isig)

    for (edge <- returnEdges) {
      for (node <- edge.node) {
        if (analyses(edge.caller).evaluated.contains(node)) {
          analyses(edge.caller).invalidations.add(
            OptimisticAnalyze.Invalidate.Force(node)
          )
        }
        analyses(edge.caller).evaluated(node) = classManager.mergeTypes(
          analyses(edge.caller).evaluated.get(node).toSeq ++ Seq(props.inferredReturn)
        )

      }
    }
    val filtered =
      if (!unchanged) returnEdges.map(_.caller)
      else returnEdges.map(_.caller).filter(!methodProps.contains(_))

    filtered
  }

  def handleFieldReference(isig: InferredSig,
                           currentCallSet: Set[InferredSig],
                           cls: JType.Cls) = {
    val clinits = analyzeClinits(Seq(cls))

    if (clinits.isEmpty) Seq(isig)
    else {
      clinits.foreach(clinit => callerGraph ::= Analyzer.CallEdge(isig, None, clinit))
      clinits.foreach(addToCallSet(_, currentCallSet))
      clinits
    }
  }

  def handleInvoke(isig: InferredSig,
                   currentCallSet: Set[InferredSig],
                   currentAnalysis: OptimisticAnalyze[IType],
                   invoke: SSA.Invoke) = {
    val calledSig = invoke.inferredSig(currentAnalysis.evaluated(_))

    if (currentCallSet(calledSig)) {
      calledSignatures.add(calledSig)
      callerGraph ::= Analyzer.CallEdge(calledSig, Some(invoke), isig)
      addToCallSet(calledSig, currentCallSet)
      analyses(isig).evaluated(invoke) = IType.Bottom
      methodProps(calledSig) = Analyzer.dummyProps(calledSig.method, true)

      Seq(isig)
    } else if (classManager.loadClass(calledSig.method.cls).isEmpty) {
      analyses(isig).evaluated(invoke) = calledSig.method.desc.ret
      Seq(isig)
    } else if (invoke.isInstanceOf[SSA.InvokeSpecial]) {
      calledSignatures.add(calledSig)
      callerGraph ::= Analyzer.CallEdge(isig, Some(invoke), calledSig)
      addToCallSet(calledSig, currentCallSet)
      Seq(calledSig)
    } else classManager.resolvePossibleSigs(calledSig.method) match {
      case None =>
        calledSignatures.add(calledSig)
        callerGraph ::= Analyzer.CallEdge(isig, Some(invoke), calledSig)
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
            (clinits, Seq(InferredSig(subSig, calledSig.inferred)))
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
          rets.foreach(ret => callerGraph ::= Analyzer.CallEdge(isig, Some(invoke), ret))

          rets.foreach(addToCallSet(_, currentCallSet))
          rets
        }

    }
  }

  def resolveProps(isig: InferredSig) = {
    classManager.resolvePossibleSigs(isig.method).map{ resolved =>
      val copied = resolved.map(m => isig.copy(method = m))

      val resolvedProps = copied.flatMap(methodProps.get)
      Analyzer.Properties(
        classManager.mergeTypes(resolvedProps.map(_.inferredReturn)),
        resolvedProps.forall(_.pure),
        resolvedProps.flatMap(_.liveArgs).toSet
      )
    }
  }

  def analyzeClinits(classes: Seq[JType.Cls]) = {
    classes.foreach(visitedClasses.add)

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
    new OptimisticAnalyze[IType](
      methodBody,
      Map.empty,
      methodBody.getAllVertices().collect { case b: SSA.Block if b.upstream.isEmpty => b }.head,
      new ITypeLattice((x, y) => classManager.mergeTypes(Seq(x, y)), inferredArgs),
      log,
      evaluateUnaBranch = {
        case (CType.I(v), SSA.UnaBranch.IFNE) => Some(v != 0)
        case (CType.I(v), SSA.UnaBranch.IFEQ) => Some(v == 0)
        case (CType.I(v), SSA.UnaBranch.IFLE) => Some(v <= 0)
        case (CType.I(v), SSA.UnaBranch.IFLT) => Some(v < 0)
        case (CType.I(v), SSA.UnaBranch.IFGE) => Some(v >= 0)
        case (CType.I(v), SSA.UnaBranch.IFGT) => Some(v > 0)
        case (JType.Null, SSA.UnaBranch.IFNULL) => Some(true)
        case (JType.Null, SSA.UnaBranch.IFNONNULL) => Some(false)
        case _ => None
      },
      evaluateBinBranch = {
        case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPEQ) => Some(v1 == v2)
        case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPNE) => Some(v1 != v2)
        case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPLT) => Some(v1 < v2)
        case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPGE) => Some(v1 >= v2)
        case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPGT) => Some(v1 > v2)
        case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPLE) => Some(v1 <= v2)
        case _ => None
      },
      evaluateSwitch = {
        case CType.I(v) => Some(v)
        case _ => None
      }
    )
  }

  def computePurity(optResult: OptimisticAnalyze.Result[IType],
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

object Analyzer {
  case class CallEdge(caller: InferredSig, node: Option[SSA.Invoke], called: InferredSig)
  case class GlobalResult(visitedMethods: collection.Map[InferredSig, Analyzer.Result],
                          visitedResolved: collection.Map[InferredSig, Analyzer.Properties],
                          visitedClasses: collection.Set[JType.Cls])
  case class Result(methodBody: MethodBody,
                    inferred: mutable.LinkedHashMap[SSA.Val, IType],
                    liveBlocks: Set[SSA.Block],
                    props: Properties)

  case class Properties(inferredReturn: IType,
                        pure: Boolean,
                        liveArgs: Set[Int])

  def dummyResult(originalSig: MethodSig, optimistic: Boolean) = Analyzer.Result(
    new MethodBody(Nil, Nil),
    mutable.LinkedHashMap.empty,
    Set.empty,
    dummyProps(originalSig, optimistic)
  )

  def dummyProps(originalSig: MethodSig, optimistic: Boolean) = Analyzer.Properties(
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
