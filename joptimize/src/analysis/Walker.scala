package joptimize.analysis

import frontend.ConstructSSA
import joptimize.algorithms.{Dominator, Scheduler}
import joptimize.backend.{CodeGenMethod, RegisterAllocator}
import joptimize.frontend.{BytecodeToSSAInterpreter, ControlFlowExtraction}
import joptimize.{Logger, Util}
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import joptimize.optimize.{ITypeLattice, OptimisticAnalyze, OptimisticSimplify}

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.mutable

class Walker(merge: (IType, IType) => IType) {

  def walkMethod(originalSig: MethodSig,
                 mn: MethodNode,
                 computeMethodSig: (MethodSig, Boolean, Seq[IType], List[(MethodSig, Seq[IType])]) => IType,
                 inferredArgs: Seq[IType],
                 checkSubclass: (JType.Cls, JType.Cls) => Boolean,
                 callStack: List[(MethodSig, Seq[IType])],
                 log: Logger,
                 classExists: JType.Cls => Boolean): (Walker.MethodResult, Set[JType.Cls], Set[MethodSig]) = {

    if (callStack.contains(originalSig -> inferredArgs) || mn.instructions.size() == 0){
      Tuple3(
        Walker.MethodResult(
          inferredReturn = originalSig.desc.ret,
          program = new Program(Nil, Nil),
          seenTryCatchBlocks = Nil,
          Nil, Nil, null, Set.empty
        ),
        Set.empty,
        Set.empty
      )
    }else{
      println(
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
      val printer = new Textifier
      val methodPrinter = new TraceMethodVisitor(printer)
      log.println("================ BYTECODE ================")
      log(Renderer.renderInsns(mn.instructions, printer, methodPrinter))

      val program = ConstructSSA.apply(originalSig.cls.name, mn, log)

      log.graph(Renderer.dumpSvg(program))
      removeDeadNodes(program)
      program.checkLinks()

      simplifyPhiMerges(program)
      program.checkLinks()
      log.graph(Renderer.dumpSvg(program))
      log.println("================ INITIAL ================")

      val preScheduleNaming = Namer.apply(program, Map.empty, program.getAllVertices())

      log(Renderer.renderSSA(program, preScheduleNaming))

      program.checkLinks()
      val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
        Walker.analyzeBlockStructure(program)

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
          merge,
          computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack),
          inferredArgs.flatMap{i => Seq.fill(i.getSize)(i)}
        ),
        postScheduleNaming,
        log
      )

      log.pprint(inferred)

      val calledMethodSigs = OptimisticSimplify.apply(
        program,
        inferred,
        liveBlocks,
        log,
        classExists
      )

      program.checkLinks(checkDead = false)
      removeDeadNodes(program)
      program.checkLinks()

      val allVertices2 = Util.breadthFirstSeen[SSA.Node](program.allTerminals.toSet)(_.upstream)

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

      val inferredReturn = allInferredReturns
        .reduceLeftOption(merge)
        .getOrElse(JType.Prim.V)

      assert(
        Util.isValidationCompatible0(inferredReturn, originalSig.desc.ret, checkSubclass),
        s"Inferred return type [$inferredReturn] is not compatible " +
          s"with declared return type [${originalSig.desc.ret}]"
      )

      val result = Walker.MethodResult(
        inferredReturn,
        program,
        Nil,
        blockEdges,
        allBlocks,
        startBlock,
        allVertices2
      )
      println(
        "  " * callStack.length +
        "-" + Util.mangleName(originalSig, inferredArgs.drop(if(originalSig.static) 0 else 1))
      )
      (result, classes, calledMethodSigs.toSet)
    }
  }



  def removeDeadNodes(program: Program) = {
    // Remove dead phi nodes that may have been inserted during SSA construction
    val allVertices = program.getAllVertices()
    for(v <- allVertices){
      for(down <- v.downstreamList){
        if (!allVertices.contains(down)) {
          v.downstreamRemoveAll(down)
        }
      }
    }
  }

  def simplifyPhiMerges(program: Program) = program.transform{
    case phi: SSA.Phi =>
      val filteredValues = phi.incoming.filter(_._2 != phi)

      if (filteredValues.map(_._2).size == 1) Util.replace(phi, filteredValues.head._2)
      else Nil

    case reg: SSA.Merge =>
      if (reg.incoming.size == 1) Util.replace(reg, reg.incoming.head)
      else Nil
  }
}

object Walker{

  /**
    * @param inferredReturn The return type of the method, narrowed to potentially
    *                       a more specific value given what we learned from
    *                       analyzing the method body.
    */
  case class MethodResult(inferredReturn: IType,
                          program: Program,
                          seenTryCatchBlocks: Seq[TryCatchBlockNode],
                          blockEdges: Seq[(SSA.Block, SSA.Block)],
                          allBlocks: Seq[SSA.Block],
                          startBlock: SSA.Block,
                          allVertices2: Set[SSA.Node])

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