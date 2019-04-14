package joptimize.analyzer

import joptimize.algorithms.{Dominator, Scheduler}
import joptimize.frontend.Frontend
import joptimize.{Logger, Util}
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import joptimize.optimize._
import org.objectweb.asm.tree._

import scala.collection.mutable

class Analyzer(entrypoints: Seq[MethodSig],
               merge: Seq[IType] => IType,
               log: Logger.Global,
               frontend: Frontend){
  val visitedMethods = mutable.LinkedHashMap.empty[(MethodSig, Seq[IType]), Analyzer.Result]
  val visitedResolved = mutable.LinkedHashMap.empty[(MethodSig, Seq[IType]), Analyzer.Properties]
  val visitedClasses = mutable.LinkedHashSet.empty[JType.Cls]
  val callerGraph = mutable.LinkedHashMap[MethodSig, mutable.LinkedHashSet[MethodSig]]()
  def apply() = {
    for (ep <- entrypoints) computeMethodSig(ep, ep.desc.args, Nil)
    for(((sig, inferred), v) <- visitedMethods if !visitedResolved.contains((sig, inferred))){
      computeMethodSig(sig, (if (!sig.static) Seq(sig.cls) else Nil) ++ inferred, Nil)
    }
    (visitedMethods, visitedResolved, visitedClasses)
  }


  def computeMethodSig(sig: MethodSig,
                       inferredArgs: Seq[IType],
                       callStack: List[(MethodSig, Seq[IType])]) = {

    visitedClasses.add(sig.cls)
    visitedResolved.getOrElseUpdate(
      (sig, inferredArgs.drop(if (sig.static) 0 else 1)),
      if(frontend.loadClass(sig.cls).isEmpty) dummyResult(sig, optimistic = false).props
      else {
        frontend.resolvePossibleSigs(sig, inferredArgs) match {
          case None => dummyResult(sig, optimistic = false).props
          case Some(subSigs) =>
            val rets = for (subSig <- subSigs) yield {
              if (frontend.loadMethod(subSig).isEmpty) dummyResult(sig, optimistic = false)
              else walkMethod(
                subSig,
                inferredArgs,
                callStack,
                log.inferredMethod(sig, inferredArgs.drop(if (sig.static) 0 else 1))
              )
            }

            val (retTypes, retPurity, retLiveArgs) = rets
              .map(p => (p.props.inferredReturn, p.props.pure, p.props.liveArgs))
              .unzip3

            Analyzer.Properties(
              merge(retTypes),
              retPurity.forall(identity),
              retLiveArgs.iterator.flatten.toSet
            )
        }
      }
    )
  }

  def computeMethodSigFor(sig: MethodSig,
                          invokeSpecial: Boolean,
                          inferredArgs: Seq[IType],
                          callStack: List[(MethodSig, Seq[IType])]): Analyzer.Properties = {

    val key = (sig, inferredArgs.drop(if (sig.static) 0 else 1))
    if (invokeSpecial) {
      walkMethod(
        sig,
        inferredArgs,
        callStack,
        log.inferredMethod(sig, inferredArgs.drop(if (sig.static) 0 else 1))
      ).props
    }
    else {
      computeMethodSig(sig, inferredArgs, callStack)
      if (visitedMethods.contains(key))  visitedResolved((sig, inferredArgs.drop(if (sig.static) 0 else 1)))
      else dummyResult(sig, false).props
    }
  }

  def dummyResult(originalSig: MethodSig, optimistic: Boolean) = Analyzer.Result(
    new MethodBody(Nil, Nil),
    mutable.LinkedHashMap.empty,
    Set.empty,
    Analyzer.Properties(
      originalSig.desc.ret,
      optimistic,
      if (optimistic) Set.empty else originalSig.desc.args.indices.toSet
    )
  )

  def walkMethod(originalSig: MethodSig,
                 inferredArgs: Seq[IType],
                 callStack: List[(MethodSig, Seq[IType])],
                 log: Logger.InferredMethod): Analyzer.Result = {

    visitedMethods.getOrElseUpdate(
      (originalSig, inferredArgs.drop(if (originalSig.static) 0 else 1)),
      if (frontend.loadMethod(originalSig).isEmpty) dummyResult(originalSig, optimistic = false)
      else if (callStack.contains(originalSig -> inferredArgs)) dummyResult(originalSig, optimistic = true)
      else frontend.loadMethodBody(originalSig, log.method(originalSig)) match{
        case None => dummyResult(originalSig, optimistic = true)
        case Some(methodBody) =>
          log.global().println(
            "  " * callStack.length +
              "+" + Util.mangleName(originalSig, inferredArgs.drop(if(originalSig.static) 0 else 1))
          )
          log.pprint(callStack)
          log.pprint(inferredArgs)
          log.check(assert(
            Util.isValidationCompatible(inferredArgs.drop(if(originalSig.static) 0 else 1), originalSig, checkSubclass),
            s"Inferred param types [${inferredArgs.mkString(", ")}] is not compatible " +
              s"with declared param types [${originalSig.desc.args.mkString(", ")}]"
          ))


          log.graph(Renderer.dumpSvg(methodBody))
          log.println("================ INITIAL ================")

          val preScheduleNaming = Namer.apply(methodBody, Map.empty, methodBody.getAllVertices(), log = log)

          log(Renderer.renderSSA(methodBody, preScheduleNaming))

          log.check(methodBody.checkLinks())
          val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
            Analyzer.analyzeBlockStructure(methodBody)

          log.println("")
          log(Renderer.renderControlFlowGraph(controlFlowEdges, preScheduleNaming.savedLocals))

          val loopTree = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

          log.println("")
          log(Renderer.renderLoopTree(loopTree, preScheduleNaming.savedLocals))

          log.println("================ SCHEDULED ================")

          val dominators = Dominator.findDominators(blockEdges, allBlocks)

          // Just for debugging
          val nodesToBlocks2 = Scheduler.apply(
            loopTree, dominators, startBlock,
            methodBody.getAllVertices()
          )

          val postScheduleNaming = Namer.apply(methodBody, nodesToBlocks2, methodBody.getAllVertices(), log = log)

          log(Renderer.renderSSA(methodBody, postScheduleNaming, nodesToBlocks2))

          log.graph(Renderer.dumpSvg(methodBody, postScheduleNaming))
          log.println("================ OPTIMISTIC ================")

          val innerStack = (originalSig -> inferredArgs) :: callStack

          val optResult = optimisticAnalyze(inferredArgs, log, methodBody, postScheduleNaming, innerStack)

          val blockEnds = optResult.liveBlocks.map(_.next)
          val canThrow = blockEnds.exists(_.isInstanceOf[SSA.AThrow])

          //      pprint.log(optResult.inferredReturns)
          val retTypes0 = optResult.inferredReturns
          val retTypes = retTypes0.filter(_ != JType.Prim.V)
          //      pprint.log(retTypes)
          val inferredReturn =
            if (retTypes.isEmpty) JType.Prim.V
            else merge(retTypes)

          val inferredPurity = computePurity(optResult, innerStack)

          val inferredLiveArgs = optResult.inferred.collect{case (a: SSA.Arg, _) => a.index}

          log.pprint(optResult.inferred)
          log.pprint(optResult.liveBlocks)
          log.pprint(inferredReturn)
          log.pprint(inferredPurity)
          log.pprint(inferredLiveArgs)

          val allVertices2 = methodBody.getAllVertices()
          val classes = allVertices2.collect{
            case n: SSA.GetField => n.owner
            case n: SSA.PutField => n.owner
            case n: SSA.GetStatic => n.cls
            case n: SSA.PutStatic => n.cls
          }
          val calledMethodSigs = allVertices2.collect{
            case n: SSA.Invoke => n.sig
          }

          for(cls <- Seq(originalSig.cls) ++ classes){
            val clinit = MethodSig(cls, "<clinit>", Desc(Nil, JType.Prim.V), true)
            if (frontend.loadMethod(clinit).isDefined) computeMethodSig(
              clinit,
              Nil,
              (originalSig -> inferredArgs) :: callStack
            )
          }


          log.check(assert(
            Util.isValidationCompatible0(inferredReturn, originalSig.desc.ret, checkSubclass),
            s"Inferred return type [${inferredReturn}] is not compatible " +
              s"with declared return type [${originalSig.desc.ret}]"
          ))


          log.global().println(
            "  " * callStack.length +
              "-" + Util.mangleName(originalSig, inferredArgs.drop(if(originalSig.static) 0 else 1))
          )


          val result = Analyzer.Result(
            methodBody,
            optResult.inferred,
            optResult.liveBlocks,
            Analyzer.Properties(
              inferredReturn,
              !canThrow && inferredPurity,
              inferredLiveArgs.toSet
            )
          )

          classes.foreach(visitedClasses.add)
          for (m <- calledMethodSigs) {
            callerGraph.getOrElseUpdate(m, mutable.LinkedHashSet.empty).add(originalSig)
          }

          result
      }
    )
  }

  def optimisticAnalyze(inferredArgs: Seq[IType], log: Logger.InferredMethod, methodBody: MethodBody, postScheduleNaming: Namer.Result, innerStack: List[(MethodSig, Seq[IType])]) = {
    OptimisticAnalyze.apply[IType](
      methodBody,
      Map.empty,
      methodBody.getAllVertices().collect { case b: SSA.Block if b.upstream.isEmpty => b }.head,
      new ITypeLattice(
        (x, y) => merge(Seq(x, y)),
        computeMethodSigFor(_, _, _, innerStack).inferredReturn,
        inferredArgs.flatMap { i => Seq.fill(i.getSize)(i) }
      ),
      postScheduleNaming,
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

      case n: SSA.InvokeStatic => computeMethodSigFor(n.sig, false, n.srcs.map(optResult.inferred), innerStack).pure
      case n: SSA.InvokeSpecial => computeMethodSigFor(n.sig, true, n.srcs.map(optResult.inferred), innerStack).pure
      case n: SSA.InvokeVirtual => computeMethodSigFor(n.sig, false, n.srcs.map(optResult.inferred), innerStack).pure
      case n: SSA.InvokeInterface => computeMethodSigFor(n.sig, false, n.srcs.map(optResult.inferred), innerStack).pure
      //    case n: SSA.InvokeDynamic => computeMethodSig(n, n.srcs.map(inferences))
      case p: SSA.Phi => true
    }
  }

  def checkSubclass(cls1: JType.Cls, cls2: JType.Cls) = merge(Seq(cls1, cls2)) == cls2
}

object Analyzer {

  case class Result(methodBody: MethodBody,
                    inferred: mutable.LinkedHashMap[SSA.Val, IType],
                    liveBlocks: Set[SSA.Block],
                    props: Properties)

  case class Properties(inferredReturn: IType,
                        pure: Boolean,
                        liveArgs: Set[Int])

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
