package joptimize.analyzer

import frontend.ConstructSSA
import joptimize.algorithms.{Dominator, Scheduler}
import joptimize.frontend.Frontend
import joptimize.{FileLogger, Logger, Util}
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import joptimize.optimize.{ITypeLattice, OptimisticAnalyze, OptimisticSimplify}
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}
import os.{Path, RelPath}

import scala.collection.mutable

object Analyzer{
  def apply(subtypeMap: mutable.LinkedHashMap[JType.Cls, scala.List[JType.Cls]],
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
                         callStack: List[(MethodSig, Seq[IType])]): IType = {

      val subSigs = {
        (sig.static, invokeSpecial) match {
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
              .filter(c => leastUpperBound(Seq(c, inferredArgs(0).asInstanceOf[JType.Cls])) == Seq(inferredArgs(0)))
              .map(c => sig.copy(cls = c))

            Some(sig :: subTypes)
        }
      }

      subSigs match {
        case None => sig.desc.ret
        case Some(subSigs) =>
          val rets = for (subSig <- subSigs) yield originalMethods.get(subSig) match {
            case Some(original) =>
              visitedMethods.getOrElseUpdate(
                (subSig, inferredArgs.drop(if (sig.static) 0 else 1)),
                {
                  val (res, newVisitedClasses, calledMethods) = walkMethod(
                    subSig,
                    original,
                    computeMethodSig,
                    inferredArgs,
                    (inf, orig) => leastUpperBound(Seq(inf, orig)) == Seq(orig),
                    callStack,
                    log.inferredMethod(sig, inferredArgs.drop(if (sig.static) 0 else 1)),
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
              ).inferredReturn
            case None =>
              sig.desc.ret
          }

          merge(rets)
      }
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

  def walkMethod(originalSig: MethodSig,
                 mn: MethodNode,
                 computeMethodSig: (MethodSig, Boolean, Seq[IType], List[(MethodSig, Seq[IType])]) => IType,
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
          inferredReturn = originalSig.desc.ret,
          program = new Program(Nil, Nil),
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
      assert(
        Util.isValidationCompatible(inferredArgs.drop(if(originalSig.static) 0 else 1), originalSig, checkSubclass),
        s"Inferred param types [${inferredArgs.mkString(", ")}] is not compatible " +
          s"with declared param types [${originalSig.desc.args.mkString(", ")}]"
      )

      val program = frontend.apply(originalSig, mn, log.method(originalSig))
      log.graph(Renderer.dumpSvg(program))
      log.println("================ INITIAL ================")

      val preScheduleNaming = Namer.apply(program, Map.empty, program.getAllVertices())

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

      val postScheduleNaming = Namer.apply(program, nodesToBlocks2, program.getAllVertices())

      log(Renderer.renderSSA(program, postScheduleNaming, nodesToBlocks2))

      log.graph(Renderer.dumpSvg(program, postScheduleNaming))
      log.println("================ OPTIMISTIC ================")

      val (inferred, liveBlocks) = OptimisticAnalyze.apply(
        program,
        Map.empty,
        program.getAllVertices().collect{case b: SSA.Block if b.upstream.isEmpty => b}.head,
        new ITypeLattice(
          (x, y) => merge(Seq(x, y)),
          computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack),
          inferredArgs.flatMap{i => Seq.fill(i.getSize)(i)}
        ),
        postScheduleNaming,
        log
      )

      log.pprint(inferred)
      log.pprint(liveBlocks)

      val calledMethodSigs = OptimisticSimplify.apply(
        program,
        inferred,
        liveBlocks,
        log,
        classExists
      )

      log.check(program.checkLinks(checkDead = false))
      program.removeDeadNodes()
      log.graph(Renderer.dumpSvg(program))
      log.check(program.checkLinks())

      val allVertices2 = program.getAllVertices()

      val classes = allVertices2.collect{
        case n: SSA.GetField => n.owner
        case n: SSA.PutField => n.owner
        case n: SSA.GetStatic => n.cls
        case n: SSA.PutStatic => n.cls
      }

      for(cls <- Seq(originalSig.cls) ++ classes){
        computeMethodSig(
          MethodSig(cls, "<clinit>", Desc(Nil, JType.Prim.V), true),
          false,
          Nil,
          (originalSig -> inferredArgs) :: callStack
        )
      }


      val allInferredReturns = allVertices2
        .collect{case r: SSA.ReturnVal => r.src}
        .flatMap{
          case n: SSA.Copy => inferred.get(n.src)
          case n => inferred.get(n)
        }

      val inferredReturn =
        if (allInferredReturns.isEmpty) JType.Prim.V
        else merge(allInferredReturns.toSeq)

      assert(
        Util.isValidationCompatible0(inferredReturn, originalSig.desc.ret, checkSubclass),
        s"Inferred return type [$inferredReturn] is not compatible " +
          s"with declared return type [${originalSig.desc.ret}]"
      )

      val result = Analyzer.Result(
        inferredReturn,
        program
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
  case class Result(inferredReturn: IType, program: Program)

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
