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

object Analyzer{
  def apply(resolver: Resolver,
            subtypeMap: mutable.LinkedHashMap[JType.Cls, scala.List[JType.Cls]],
            entrypoints: Seq[MethodSig],
            classNodeMap: Map[JType.Cls, ClassNode],
            originalMethods: Map[MethodSig, MethodNode],
            leastUpperBound: Seq[JType.Cls] => Seq[JType.Cls],
            merge: Seq[IType] => IType,
            log: Logger.Global,
            frontend: Frontend) = {
    val visitedMethods = mutable.LinkedHashMap.empty[(MethodSig, Seq[IType]), Analyzer.Result]
    val visitedClasses = mutable.LinkedHashSet.empty[JType.Cls]
    val callerGraph = mutable.LinkedHashMap[MethodSig, mutable.LinkedHashSet[MethodSig]]()

    def computeMethodSig(sig: MethodSig,
                         invokeSpecial: Boolean,
                         inferredArgs: Seq[IType],
                         callStack: List[(MethodSig, Seq[IType])]): (IType, Boolean, Set[Int]) = {

      resolver.resolvePossibleSigs(
        sig,
        invokeSpecial,
        inferredArgs,
        compute = (subSig, original, inferredArgs) => Some(visitedMethods.getOrElseUpdate(
          (subSig, inferredArgs.drop(if (subSig.static) 0 else 1)),
          {
            val (res, newVisitedClasses, calledMethods) = walkMethod(
              subSig,
              original,
              computeMethodSig,
              inferredArgs,
              (inf, orig) => leastUpperBound(Seq(inf, orig)) == Seq(orig),
              callStack,
              log.inferredMethod(subSig, inferredArgs.drop(if (subSig.static) 0 else 1)),
              classNodeMap.contains,
              merge,
              frontend
            )
            newVisitedClasses.foreach(visitedClasses.add)
            for (m <- calledMethods) {
              callerGraph.getOrElseUpdate(m, mutable.LinkedHashSet.empty).add(subSig)
            }
            res
          }
        ))
      )
    }

    for (ep <- entrypoints) {
      val (res, seenClasses, calledMethods) = walkMethod(
        ep,
        originalMethods(ep),
        computeMethodSig,
        ep.desc.args,
        (inf, orig) => leastUpperBound(Seq(inf, orig)) == Seq(orig),
        Nil,
        log.inferredMethod(ep, ep.desc.args.drop(if (ep.static) 0 else 1)),
        classNodeMap.contains,
        merge,
        frontend
      )
      for (m <- calledMethods) {
        callerGraph.getOrElseUpdate(m, mutable.LinkedHashSet.empty).add(ep)
      }

      visitedMethods((ep, ep.desc.args)) = res
      seenClasses.foreach(visitedClasses.add)
    }
    (visitedMethods, visitedClasses)
  }

  class Resolver(val classNodeMap: Map[JType.Cls, ClassNode],
                 val originalMethods: Map[MethodSig, MethodNode],
                 val subtypeMap: mutable.LinkedHashMap[JType.Cls, List[JType.Cls]],
                 val leastUpperBound: Seq[JType.Cls] => Seq[JType.Cls],
                 val merge: Seq[IType] => IType) {
    def resolvePossibleSigs(sig: MethodSig,
                            invokeSpecial: Boolean,
                            inferredArgs: Seq[IType],
                            compute: (MethodSig, MethodNode, Seq[IType]) => Option[Analyzer.Result]) = {

      val subSigs = (sig.static, invokeSpecial) match {
        case (true, false) =>
          def rec(currentCls: JType.Cls): Option[MethodSig] = {
            val currentSig = sig.copy(cls = currentCls)
            if (originalMethods.contains(currentSig)) Some(currentSig)
            else if (!classNodeMap.contains(currentCls)) None
            else rec(JType.Cls(classNodeMap(currentCls).superName))
          }

          if (sig.name == "<clinit>") Some(Seq(sig))
          else rec(sig.cls).map(Seq(_))
        case (false, true) => Some(Seq(sig))
        case (false, false) =>
          val subTypes = subtypeMap
            .getOrElse(sig.cls, Nil)
            .map(c => sig.copy(cls = c))

          Some(sig :: subTypes)
      }

      subSigs match {
        case None => (sig.desc.ret, false, sig.desc.args.indices.toSet)
        case Some(subSigs) =>
          val rets = for (subSig <- subSigs) yield originalMethods.get(subSig) match {
            case Some(original) =>
              val resOpt = compute(subSig, original, inferredArgs)
              resOpt.map(res => (res.inferredReturn, res.pure, res.liveArgs))
            case None =>
              Some((sig.desc.ret, false, sig.desc.args.indices.toSet))
          }

          val (retTypes, retPurity, retLiveArgs) = rets.flatten.unzip3
          (merge(retTypes), retPurity.forall(identity), retLiveArgs.iterator.flatten.toSet)
      }
    }
  }

  def walkMethod(originalSig: MethodSig,
                 mn: MethodNode,
                 computeMethodSig: (MethodSig, Boolean, Seq[IType], List[(MethodSig, Seq[IType])]) => (IType, Boolean, Set[Int]),
                 inferredArgs: Seq[IType],
                 checkSubclass: (JType.Cls, JType.Cls) => Boolean,
                 callStack: List[(MethodSig, Seq[IType])],
                 log: Logger.InferredMethod,
                 classExists: JType.Cls => Boolean,
                 merge: Seq[IType] => IType,
                 frontend: Frontend): (Analyzer.Result, Set[JType.Cls], Set[MethodSig]) = {

    if (callStack.contains(originalSig -> inferredArgs) || mn.instructions.size() == 0){
      Tuple3(
        Analyzer.Result(
          originalSig.desc.ret,
          new Program(Nil, Nil),
          mutable.LinkedHashMap.empty,
          Set.empty,
          true,
          Set.empty
        ),
        Set.empty,
        Set.empty
      )
    }else{
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

      val program = frontend.apply(originalSig, mn, log.method(originalSig))
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
            computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack)._1,
            inferredArgs.flatMap{i => Seq.fill(i.getSize)(i)}
          ),
          new PurityLattice(
            computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack)._2
          ),
          new LivenessLattice(
            computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack)._3
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
        computeMethodSig(
          MethodSig(cls, "<clinit>", Desc(Nil, JType.Prim.V), true),
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

      val result = Analyzer.Result(
        inferredReturn,
        program,
        optResult.inferred,
        optResult.liveBlocks,
        !canThrow && inferredPurity,
        inferredLiveArgs
      )

      log.global().println(
        "  " * callStack.length +
        "-" + Util.mangleName(originalSig, inferredArgs.drop(if(originalSig.static) 0 else 1))
      )
      (result, classes, calledMethodSigs.toSet)
    }
  }


  /**
    * @param inferredReturn The return type of the method, narrowed to potentially
    *                       a more specific value given what we learned from
    *                       analyzing the method body.
    */
  case class Result(inferredReturn: IType,
                    program: Program,
                    inferred: mutable.LinkedHashMap[SSA.Val, (IType, Boolean, Set[Int])],
                    liveBlocks: Set[SSA.Block],
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
