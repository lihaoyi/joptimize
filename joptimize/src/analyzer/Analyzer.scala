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
  val visitedMethods = mutable.LinkedHashMap.empty[InferredSig, Analyzer.Result]
  val visitedResolved = mutable.LinkedHashMap.empty[InferredSig, Analyzer.Properties]
  val visitedClasses = mutable.LinkedHashSet.empty[JType.Cls]
  val invalidated = mutable.LinkedHashMap.empty[JType.Cls, Seq[MethodSig]]

  /**
    * Edges from the caller to the called
    *
    * This is kept up to date on a node-by-node basis during the traversal
    */
  val callerGraph = new MultiBiMap.Mutable[
    (InferredSig, SSA.Invoke),
    InferredSig
  ]()

  val analyses = mutable.LinkedHashMap.empty[InferredSig, OptimisticAnalyze[IType]]

  val stacks = mutable.Buffer.empty[
    (List[(InferredSig, Analyzer.Properties => Unit)], Int)
  ]

  def apply() = {

    for (ep <- entrypoints) {
      stacks.append(List((InferredSig(ep, ep.desc.args), (_: Analyzer.Properties) => ())) -> 0)
    }

    while(stacks.nonEmpty){

//      pprint.log(stacks.map(_._1.map(_._1.toString)))
      val (currentStack, currentStackStartDepth) = stacks(0)

      val (isig, returnCallback) = currentStack.head
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

      val newCurrent: Seq[(List[(InferredSig, Analyzer.Properties => Unit)], Int)] = step match{
        case OptimisticAnalyze.Step.Continue(nodeOpt) =>
          val addedCls = nodeOpt match{
            case Some(n: SSA.GetField) => Some(n.owner)
            case Some(n: SSA.PutField) => Some(n.owner)
            case Some(n: SSA.GetStatic) => Some(n.cls)
            case Some(n: SSA.PutStatic) => Some(n.cls)
            case _ => None
          }
          val clinits = analyzeClinits(currentStack, addedCls.toSeq)

          clinits ++ Seq(currentStack -> currentStackStartDepth)

        case OptimisticAnalyze.Step.Done() =>
//          println("DONE")
//          pprint.log(originalSig.toString)
          val optimisticResult = currentAnalysis.apply()
          val retTypes = currentAnalysis.apply().inferredReturns.filter(_ != JType.Prim.V)
          val inferredReturn =
            if (retTypes.isEmpty) JType.Prim.V
            else classManager.mergeTypes(retTypes)

          val props = Analyzer.Properties(
            inferredReturn,
            computePurity(optimisticResult, currentStack.map(_._1)) &&
            !(isig.method.static && classManager.loadMethod(MethodSig(isig.method.cls, "<clinit>", Desc(Nil, JType.Prim.V), true)).nonEmpty),
            optimisticResult.inferred.collect { case (a: SSA.Arg, _) => a.index }.toSet
          )

          visitedMethods(isig) = Analyzer.Result(
            methodBody,
            optimisticResult.inferred,
            optimisticResult.liveBlocks,
            props
          )

          inferredLog.pprint(optimisticResult.inferred)
          inferredLog.pprint(optimisticResult.liveBlocks)
          returnCallback(props)
          if (currentStack.tail.length > currentStackStartDepth) Seq(currentStack.tail -> currentStackStartDepth)
          else Nil

        case OptimisticAnalyze.Step.ComputeSig(calledSig, invoke, callback) =>
          if (currentStack.exists(t => t._1 == calledSig)) {
            analyzeDummy(currentStack, currentStackStartDepth, calledSig.method, callback, optimistic = true)
          } else if(classManager.loadClass(calledSig.method.cls).isEmpty){
            analyzeDummy(currentStack, currentStackStartDepth, calledSig.method, callback, optimistic = false)
          } else if (invoke.isInstanceOf[SSA.InvokeSpecial]){
            val subLog = globalLog.inferredMethod(calledSig)
            subLog.println("================ INITIAL ================")

            subLog.graph(Renderer.dumpSvg(frontend.loadMethodBody(calledSig, globalLog.method(calledSig.method)).get))
            Seq(
              Tuple2(
                (calledSig, (props: Analyzer.Properties) => callback(props.inferredReturn)) :: currentStack,
                currentStackStartDepth
              )
            )
          }else{
            val resolved = classManager.resolvePossibleSigs(calledSig.method)
            resolved match {
              case None =>
                analyzeDummy(currentStack, currentStackStartDepth, calledSig.method, callback, optimistic = false)
              case Some(subSigs0) =>

                val subSigs = subSigs0.filter{ subSig =>
                  classManager.loadMethod(subSig).exists(_.instructions.size() != 0)
                }

                var agg = List.empty[(MethodSig, Analyzer.Properties)]

                val rets = for (subSig <- subSigs) yield {

                  val subCallback = (i: Analyzer.Properties) => {
                    agg ::= (subSig, i)

                    if (agg.length == subSigs.length){


                      visitedResolved(calledSig) =
                        Analyzer.Properties(
                          inferredReturn = classManager.mergeTypes(agg.map(_._2.inferredReturn)),
                          pure = agg.forall(_._2.pure),
                          liveArgs = agg.flatMap(_._2.liveArgs).toSet
                        )

                      callback(
                        visitedResolved(calledSig)
                          .inferredReturn
                      )
                    }

                    ()
                  }
                  val clinits = analyzeClinits(currentStack, Seq(subSig.cls))

                  val x = Seq(((InferredSig(subSig, calledSig.inferred), subCallback) :: currentStack, currentStack.length))
                  clinits ++ x
                }
                rets.flatten.toList ::: List(currentStack -> currentStackStartDepth)
            }
          }

      }

//      pprint.log(newCurrent.map(_._1.map(_._1.toString)))
      stacks.remove(0)
      stacks.insertAll(0, newCurrent)
    }

    for((isig, v) <- visitedMethods if !visitedResolved.contains(isig)){
      visitedResolved(isig) = v.props
    }

    Analyzer.GlobalResult(visitedMethods, visitedResolved, visitedClasses)

  }

  def analyzeDummy(currentStack: List[(InferredSig, Analyzer.Properties => Unit)],
                   currentStackStartDepth: Int,
                   calledSig: MethodSig,
                   callback: IType => Unit,
                   optimistic: Boolean) = {
    callback(Analyzer.dummyResult(calledSig, optimistic).props.inferredReturn)
    Seq(currentStack -> currentStackStartDepth)
  }

  def analyzeClinits(currentStack: List[(InferredSig, Analyzer.Properties => Unit)],
                     classes: Seq[JType.Cls]) = {
    classes.foreach(visitedClasses.add)

    val clinits = for {
      cls <- classes
      val clinit = MethodSig(cls, "<clinit>", Desc(Nil, JType.Prim.V), true)
      if classManager.loadMethod(clinit).isDefined
      if !analyses.contains(InferredSig(clinit, Nil))
    } yield {
      ((InferredSig(clinit, Nil), (_: Analyzer.Properties) => ()) :: currentStack, currentStack.length)
    }
    clinits
  }

  def onNew(cls: JType.Cls) = {
    for(upperClassNode <- classManager.loadClass(cls)){
      invalidated.put(
        cls,
        upperClassNode.methods.asScala.map(mn =>
          MethodSig(cls, mn.name, Desc.read(mn.desc), (mn.access & Opcodes.ACC_STATIC) != 0)
        )
      )
    }

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
      },
    )
  }

  def computePurity(optResult: OptimisticAnalyze.Result[IType],
                    innerStack: List[InferredSig]) = {
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
        val key = n.inferredSig(optResult.inferred)
        val default = innerStack.contains(key)
        if (n.isInstanceOf[SSA.InvokeSpecial]) visitedMethods.get(key).fold(default)(_.props.pure)
        else visitedResolved.get(key).fold(default)(_.pure)

      //    case n: SSA.InvokeDynamic => computeMethodSig(n, n.srcs.map(inferences))
      case p: SSA.Phi => true
    }
  }

  def checkSubclass(cls1: JType.Cls, cls2: JType.Cls) = classManager.mergeTypes(Seq(cls1, cls2)) == cls2
}

object Analyzer {
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
    Analyzer.Properties(
      if (optimistic) IType.Bottom else originalSig.desc.ret,
      optimistic,
      if (optimistic) Set.empty
      else Range.inclusive(0, originalSig.desc.args.length + (if (originalSig.static) 0 else 1)).toSet
    )
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
