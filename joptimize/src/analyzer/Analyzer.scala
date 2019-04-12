package joptimize.analyzer

import joptimize.algorithms.{Dominator, Scheduler}
import joptimize.frontend.Frontend
import joptimize.{Logger, Util}
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import joptimize.optimize._
import optimize.LivenessLattice
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
    for (ep <- entrypoints) computeMethodSig(ep, false, ep.desc.args, Nil)

    (visitedMethods, visitedResolved, visitedClasses)
  }


  def computeMethodSig(sig: MethodSig,
                       invokeSpecial: Boolean,
                       inferredArgs: Seq[IType],
                       callStack: List[(MethodSig, Seq[IType])]): Analyzer.Properties = {

    val visitedKey = (sig, inferredArgs.drop(if (sig.static) 0 else 1))
    if (visitedResolved.contains(visitedKey)) visitedResolved(visitedKey)
    else if(frontend.loadClass(sig.cls).isEmpty) Analyzer.Properties(sig.desc.ret, false, sig.desc.args.indices.toSet)
    else {
      lazy val walked = walkMethod(
        sig,
        computeMethodSig,
        inferredArgs,
        (inf, orig) => merge(Seq(inf, orig)) == orig,
        callStack,
        log.inferredMethod(sig, inferredArgs.drop(if (sig.static) 0 else 1)),
        merge,
        frontend
      )

      val subSigs = frontend.resolvePossibleSigs(sig, invokeSpecial, inferredArgs)

      val res = subSigs match {
        case None => Analyzer.Properties(sig.desc.ret, false, sig.desc.args.indices.toSet)
        case Some(subSigs) =>
          val rets = for (subSig <- subSigs) yield {
            if (subSig == sig) (walked.props.inferredReturn, walked.props.pure, walked.props.liveArgs)
            else if (frontend.loadMethod(subSig).isEmpty) (sig.desc.ret, false, sig.desc.args.indices.toSet)
            else {
              val res = computeMethodSig(subSig, invokeSpecial, inferredArgs, callStack)
              (res.inferredReturn, res.pure, res.liveArgs)
            }
          }

          val (retTypes, retPurity, retLiveArgs) = rets.unzip3
          Analyzer.Properties(merge(retTypes), retPurity.forall(identity), retLiveArgs.iterator.flatten.toSet)
      }

      visitedResolved(visitedKey) = res
      res
    }
  }


  def walkMethod(originalSig: MethodSig,
                 computeMethodSig: (MethodSig, Boolean, Seq[IType], List[(MethodSig, Seq[IType])]) => Analyzer.Properties,
                 inferredArgs: Seq[IType],
                 checkSubclass: (JType.Cls, JType.Cls) => Boolean,
                 callStack: List[(MethodSig, Seq[IType])],
                 log: Logger.InferredMethod,
                 merge: Seq[IType] => IType,
                 frontend: Frontend): Analyzer.Result = {

    def dummyResult = Analyzer.Result(
      new Program(Nil, Nil),
      mutable.LinkedHashMap.empty,
      Set.empty,
      Analyzer.Properties(
        originalSig.desc.ret,
        true,
        Set.empty
      )
    )

    val visitedKey = (originalSig, inferredArgs.drop(if (originalSig.static) 0 else 1))
    if (visitedMethods.contains(visitedKey)) visitedMethods(visitedKey)
    else if (callStack.contains(originalSig -> inferredArgs)) {
      visitedMethods(visitedKey) = dummyResult
      visitedMethods(visitedKey)
    } else frontend.apply(originalSig, log.method(originalSig)) match{
      case None =>
        visitedMethods(visitedKey) = dummyResult
        visitedMethods(visitedKey)
      case Some(program) =>
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


        log.graph(Renderer.dumpSvg(program))
        log.println("================ INITIAL ================")

        val preScheduleNaming = Namer.apply(program, Map.empty, program.getAllVertices(), log = log)

        log(Renderer.renderSSA(program, preScheduleNaming))

        log.check(program.checkLinks())
        val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
          Analyzer.analyzeBlockStructure(program)

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
          program.getAllVertices()
        )

        val postScheduleNaming = Namer.apply(program, nodesToBlocks2, program.getAllVertices(), log = log)

        log(Renderer.renderSSA(program, postScheduleNaming, nodesToBlocks2))

        log.graph(Renderer.dumpSvg(program, postScheduleNaming))
        log.println("================ OPTIMISTIC ================")

        val optResult = OptimisticAnalyze.apply[(IType, Boolean, Set[Int])](
          program,
          Map.empty,
          program.getAllVertices().collect{case b: SSA.Block if b.upstream.isEmpty => b}.head,
          new CombinedLattice(
            new ITypeLattice(
              (x, y) => merge(Seq(x, y)),
              computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack).inferredReturn,
              inferredArgs.flatMap{i => Seq.fill(i.getSize)(i)}
            ),
            new PurityLattice(
              computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack).pure
            ),
            new LivenessLattice(
              computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack).liveArgs
            )
          ),
          postScheduleNaming,
          log,
          evaluateUnaBranch = {
            case ((CType.I(v), _, _), SSA.UnaBranch.IFNE) => Some(v != 0)
            case ((CType.I(v), _, _), SSA.UnaBranch.IFEQ) => Some(v == 0)
            case ((CType.I(v), _, _), SSA.UnaBranch.IFLE) => Some(v <= 0)
            case ((CType.I(v), _, _), SSA.UnaBranch.IFLT) => Some(v < 0)
            case ((CType.I(v), _, _), SSA.UnaBranch.IFGE) => Some(v >= 0)
            case ((CType.I(v), _, _), SSA.UnaBranch.IFGT) => Some(v > 0)
            case ((JType.Null, _, _), SSA.UnaBranch.IFNULL) => Some(true)
            case ((JType.Null, _, _), SSA.UnaBranch.IFNONNULL) => Some(false)
            case _ => None
          },
          evaluateBinBranch = {
            case ((CType.I(v1), _, _), (CType.I(v2), _, _), SSA.BinBranch.IF_ICMPEQ) => Some(v1 == v2)
            case ((CType.I(v1), _, _), (CType.I(v2), _, _), SSA.BinBranch.IF_ICMPNE) => Some(v1 != v2)
            case ((CType.I(v1), _, _), (CType.I(v2), _, _), SSA.BinBranch.IF_ICMPLT) => Some(v1 < v2)
            case ((CType.I(v1), _, _), (CType.I(v2), _, _), SSA.BinBranch.IF_ICMPGE) => Some(v1 >= v2)
            case ((CType.I(v1), _, _), (CType.I(v2), _, _), SSA.BinBranch.IF_ICMPGT) => Some(v1 > v2)
            case ((CType.I(v1), _, _), (CType.I(v2), _, _), SSA.BinBranch.IF_ICMPLE) => Some(v1 <= v2)
            case _ => None
          },
          evaluateSwitch = {
            case (CType.I(v), _, _) => Some(v)
            case _ => None
          }
        )
        val blockEnds = optResult.liveBlocks.map(_.next)
        val canThrow = blockEnds.exists(_.isInstanceOf[SSA.AThrow])

        //      pprint.log(optResult.inferredReturns)
        val (retTypes0, retPurity, retLiveArgss) = optResult.inferredReturns.unzip3
        val retTypes = retTypes0.filter(_ != JType.Prim.V)
        //      pprint.log(retTypes)
        val inferredReturn =
          if (retTypes.isEmpty) JType.Prim.V
          else merge(retTypes)

        val inferredPurity = optResult.inferred.valuesIterator.forall(_._2)

        val inferredLiveArgs = optResult.inferred.valuesIterator.flatMap(_._3).toSet

        log.pprint(optResult.inferred)
        log.pprint(optResult.liveBlocks)
        log.pprint(inferredReturn)
        log.pprint(inferredPurity)
        log.pprint(inferredLiveArgs)

        val allVertices2 = program.getAllVertices()
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
            false,
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
          program,
          optResult.inferred,
          optResult.liveBlocks,
          Analyzer.Properties(
            inferredReturn,
            !canThrow && inferredPurity,
            inferredLiveArgs
          )
        )

        classes.foreach(visitedClasses.add)
        for (m <- calledMethodSigs) {
          callerGraph.getOrElseUpdate(m, mutable.LinkedHashSet.empty).add(originalSig)
        }

        visitedMethods(visitedKey) = result

        result
    }
  }
}
object Analyzer {

  case class Result(program: Program,
                    inferred: mutable.LinkedHashMap[SSA.Val, (IType, Boolean, Set[Int])],
                    liveBlocks: Set[SSA.Block],
                    props: Properties)

  case class Properties(inferredReturn: IType,
                        pure: Boolean,
                        liveArgs: Set[Int])

  def analyzeBlockStructure(program: Program) = {
    val controlFlowEdges = Renderer.findControlFlowGraph(program)
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
