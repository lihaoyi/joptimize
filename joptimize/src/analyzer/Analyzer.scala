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
  val visitedMethods = mutable.LinkedHashMap.empty[(MethodSig, Seq[IType]), Analyzer.Result]
  val visitedResolved = mutable.LinkedHashMap.empty[(MethodSig, Seq[IType]), Analyzer.Properties]
  val visitedClasses = mutable.LinkedHashSet.empty[JType.Cls]
  val invalidated = mutable.LinkedHashMap.empty[JType.Cls, Seq[MethodSig]]

  /**
    * Edges from the caller to the called
    *
    * This is kept up to date on a node-by-node basis during the traversal
    */
  val callerGraph = new MultiBiMap.Mutable[
    (MethodSig, Seq[IType], SSA.Invoke),
    (MethodSig, Seq[IType])
  ]()

  val analyses = mutable.LinkedHashMap.empty[(MethodSig, Seq[IType]), OptimisticAnalyze[IType]]

  val stacks = mutable.Buffer.empty[
    (List[(MethodSig, Seq[IType], Analyzer.Properties => Unit)], Int)
  ]

  def apply() = {

    for (ep <- entrypoints) {
      stacks.append(List((ep, ep.desc.args, (_: Analyzer.Properties) => ())) -> 0)
    }

    while(stacks.nonEmpty){

//      pprint.log(stacks.map(_._1.map(_._1.toString)))
      val (currentStack, currentStackStartDepth) = stacks(0)

      val (originalSig, inferred, returnCallback) = currentStack.head
      val methodLog = globalLog.method(originalSig)
      val inferredLog = globalLog.inferredMethod(originalSig, inferred)
      val methodBody = frontend.loadMethodBody(originalSig, inferred.drop(if (originalSig.static) 0 else 1), methodLog).get
      val currentAnalysis = analyses.getOrElseUpdate(
        (originalSig, inferred),
        optimisticAnalyze(inferred, inferredLog, methodBody, Nil)
      )

      val step = currentAnalysis.step()

      val newCurrent: Seq[(List[(MethodSig, Seq[IType], Analyzer.Properties => Unit)], Int)] = step match{
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
            else classManager.merge(retTypes)

          val props = Analyzer.Properties(
            inferredReturn,
            computePurity(optimisticResult, currentStack.map{case (k, v, cb) => (k, v)}) &&
            !(originalSig.static && classManager.loadMethod(MethodSig(originalSig.cls, "<clinit>", Desc(Nil, JType.Prim.V), true)).nonEmpty),
            optimisticResult.inferred.collect { case (a: SSA.Arg, _) => a.index }.toSet
          )

          visitedMethods((originalSig, inferred.drop(if (originalSig.static) 0 else 1))) = Analyzer.Result(
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

        case OptimisticAnalyze.Step.ComputeSig(calledSig, invoke, calledInferred, callback) =>
          if (currentStack.exists(t => t._1 == calledSig && t._2 == calledInferred)) {
            analyzeDummy(currentStack, currentStackStartDepth, calledSig, callback, optimistic = true)
          } else if(classManager.loadClass(calledSig.cls).isEmpty){
            analyzeDummy(currentStack, currentStackStartDepth, calledSig, callback, optimistic = false)
          } else if (invoke.isInstanceOf[SSA.InvokeSpecial]){
            val subLog = globalLog.inferredMethod(calledSig, calledInferred)
            subLog.println("================ INITIAL ================")

            subLog.graph(Renderer.dumpSvg(frontend.loadMethodBody(calledSig, calledInferred.drop(if (calledSig.static) 0 else 1), globalLog.method(calledSig)).get))
            Seq(
              Tuple2(
                (calledSig, calledInferred, (props: Analyzer.Properties) => callback(props.inferredReturn)) :: currentStack,
                currentStackStartDepth
              )
            )
          }else{
            val resolved = classManager.resolvePossibleSigs(calledSig)
            resolved match {
              case None =>
                analyzeDummy(currentStack, currentStackStartDepth, calledSig, callback, optimistic = false)
              case Some(subSigs0) =>

                val (subSigs, abstractSubSigs) = subSigs0
                  .partition{ subSig =>
                    classManager.loadMethod(subSig).exists(_.instructions.size() != 0)
                  }

                var agg = List.empty[(MethodSig, Analyzer.Properties)]

                val rets = for (subSig <- subSigs) yield {

                  val subCallback = (i: Analyzer.Properties) => {
                    agg ::= (subSig, i)

                    if (agg.length == subSigs.length){


                      visitedResolved((calledSig, calledInferred.drop(if (calledSig.static) 0 else 1))) =
                        Analyzer.Properties(
                          inferredReturn = classManager.merge(agg.map(_._2.inferredReturn)),
                          pure = agg.forall(_._2.pure),
                          liveArgs = agg.flatMap(_._2.liveArgs).toSet
                        )

                      callback(
                        visitedResolved((calledSig, calledInferred.drop(if (calledSig.static) 0 else 1)))
                          .inferredReturn
                      )
                    }

                    ()
                  }
                  val clinits = analyzeClinits(currentStack, Seq(subSig.cls))

                  val x = Seq(((subSig, calledInferred, subCallback) :: currentStack, currentStack.length))
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

    for(((sig, inferred), v) <- visitedMethods if !visitedResolved.contains((sig, inferred))){
      visitedResolved((sig, inferred)) = v.props
    }


    Analyzer.GlobalResult(visitedMethods, visitedResolved, visitedClasses)

  }

  def analyzeDummy(currentStack: List[(MethodSig, Seq[IType], Analyzer.Properties => Unit)],
                   currentStackStartDepth: Int,
                   calledSig: MethodSig,
                   callback: IType => Unit,
                   optimistic: Boolean) = {
    callback(Analyzer.dummyResult(calledSig, optimistic).props.inferredReturn)
    Seq(currentStack -> currentStackStartDepth)
  }

  def analyzeClinits(currentStack: List[(MethodSig, Seq[IType], Analyzer.Properties => Unit)], classes: Seq[JType.Cls]) = {
    classes.foreach(visitedClasses.add)

    val clinits = for {
      cls <- classes
      val clinit = MethodSig(cls, "<clinit>", Desc(Nil, JType.Prim.V), true)
      if classManager.loadMethod(clinit).isDefined
      if !analyses.contains((clinit, Nil))
    } yield {
      ((clinit, Nil, (_: Analyzer.Properties) => ()) :: currentStack, currentStack.length)
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

  def optimisticAnalyze(inferredArgs: Seq[IType], log: Logger.InferredMethod, methodBody: MethodBody, innerStack: List[(MethodSig, Seq[IType])]) = {
    new OptimisticAnalyze[IType](
      methodBody,
      Map.empty,
      methodBody.getAllVertices().collect { case b: SSA.Block if b.upstream.isEmpty => b }.head,
      new ITypeLattice(
        (x, y) => classManager.merge(Seq(x, y)),
        inferredArgs.flatMap { i => Seq.fill(i.getSize)(i) }
      ),
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
                    innerStack: List[(MethodSig, Seq[IType])]) = {
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
        val key = (n.sig, n.srcs.drop(if (n.sig.static) 0 else 1).map(optResult.inferred))
        val default = innerStack.contains(key)
        if (n.isInstanceOf[SSA.InvokeSpecial]) visitedMethods.get(key).fold(default)(_.props.pure)
        else visitedResolved.get(key).fold(default)(_.pure)

      //    case n: SSA.InvokeDynamic => computeMethodSig(n, n.srcs.map(inferences))
      case p: SSA.Phi => true
    }
  }

  def checkSubclass(cls1: JType.Cls, cls2: JType.Cls) = classManager.merge(Seq(cls1, cls2)) == cls2
}

object Analyzer {
  case class GlobalResult(visitedMethods: collection.Map[(MethodSig, Seq[IType]), Analyzer.Result],
                          visitedResolved: collection.Map[(MethodSig, Seq[IType]), Analyzer.Properties],
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
      originalSig.desc.ret,
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
