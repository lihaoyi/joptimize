package joptimize.analysis

import joptimize.Util
import joptimize.bytecode.Frame
import joptimize.graph.HavlakLoopTree
import joptimize.model._
import org.objectweb.asm.Opcodes

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.{immutable, mutable}

class Walker(merge: (IType, IType) => IType) {

  def walkMethod(sig: MethodSig, mn: MethodNode): (Walker.MethodResult, Set[MethodSig], Set[JType.Cls]) = {
    println("+" * 20 + sig + "+" * 20)
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)
    println("================ BYTECODE ================")
    println(Renderer.renderInsns(mn.instructions, printer, methodPrinter))

    val program = constructSSAProgram(sig, mn)

    removeDeadNodes(program)
//    program.checkLinks()
//
//    simplifyPhiMerges(program)
//    program.checkLinks()

    println("================ INITIAL ================")

    val preScheduleNaming = Namer.apply(program, Map.empty, program.getAllVertices())

    println(Renderer.renderSSA(program, preScheduleNaming))
    pprint.log(preScheduleNaming.savedLocals.mapValues(_._2))
    program.checkLinks()
    val (controlFlowEdges, startBlock, allBlocks, blockEdges) =
      analyzeBlockStructure(program)

    println()
    println(Renderer.renderControlFlowGraph(controlFlowEdges, preScheduleNaming.savedLocals))

    val loopTree = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

    println()
    println(Renderer.renderLoopTree(loopTree, preScheduleNaming.savedLocals))

    println("================ SCHEDULED ================")

    val dominators = Dominator.findDominators(blockEdges, allBlocks)

   // Just for debugging
    val nodesToBlocks2 = Scheduler.apply(
      loopTree, dominators, startBlock,
      preScheduleNaming.savedLocals.mapValues(_._2), program.getAllVertices()
    )

    val postScheduleNaming = Namer.apply(program, nodesToBlocks2, program.getAllVertices())

    println(Renderer.renderSSA(program, postScheduleNaming, nodesToBlocks2))

    println("================ PESSIMISTIC ================")

//    PartialEvaluator.apply(program)
//
//    removeDeadNodes(program)
//
//    program.checkLinks()

    val postPessimisticNaming = Namer.apply(program, Map.empty, program.getAllVertices())

    println(Renderer.renderSSA(program, postPessimisticNaming))

    val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

    println()
    println(Renderer.renderLoopTree(loopTree2, preScheduleNaming.savedLocals))

    println("================ OPTIMISTIC ================")

    OptimisticAnalyze.apply(
      program,
      Map.empty,
      program.getAllVertices().collect{case b: SSA.Block if b.upstream.isEmpty => b}.head,
      ITypeLattice(merge),
      postPessimisticNaming
    )

    val dominators2 = Dominator.findDominators(blockEdges, allBlocks)

    pprint.log(startBlock)

    { // Just for debugging
      val nodesToBlocks = Scheduler.apply(
        loopTree2, dominators2, startBlock,
        preScheduleNaming.savedLocals.mapValues(_._2), program.getAllVertices()
      )

      val postScheduleNaming = Namer.apply(program, nodesToBlocks, program.getAllVertices())

//        pprint.log(preScheduleNaming.savedLocals.collect{case (k: SSA.Block, (v1, v2)) => (k, v2)}, height=9999)
//        pprint.log(preScheduleNaming.saveable)
//        pprint.log(nodesToBlocks, height=9999)
//
      println()
      println(Renderer.renderSSA(program, postScheduleNaming, nodesToBlocks))

//      ???
    }

    RegisterAllocator.apply(program, dominators2.immediateDominators)

    val allVertices2 = Util.breadthFirstAggregation[SSA.Node](program.allTerminals.toSet)(_.upstream)._1

    val nodesToBlocks = Scheduler.apply(
      loopTree2, dominators2, startBlock,
      preScheduleNaming.savedLocals.mapValues(_._2), allVertices2
    )

    val postRegisterAllocNaming = Namer.apply(program, nodesToBlocks, allVertices2)

    println()
    println(Renderer.renderSSA(program, postRegisterAllocNaming, nodesToBlocks))

    val (blockCode, finalInsns) = CodeGen(
      program,
      allVertices2,
      nodesToBlocks,
      analyzeBlockStructure(program)._1,
      postRegisterAllocNaming
    )

    println(Renderer.renderBlockCode(blockCode, finalInsns))

    val called = allVertices2.collect{
      case SSA.InvokeStatic(state, srcs, cls, name, desc) => MethodSig(cls, name, desc, true)
      case SSA.InvokeVirtual(state, srcs, cls, name, desc) => MethodSig(cls, name, desc, false)
      case SSA.InvokeSpecial(state, srcs, cls, name, desc) => MethodSig(cls, name, desc, false)
      case SSA.InvokeInterface(state, srcs, cls, name, desc) => MethodSig(cls, name, desc, false)
      case SSA.InvokeDynamic(name, desc, bootstrap, bsArgs, srcs)
        if bootstrap == Util.metafactory || bootstrap == Util.altMetafactory =>
        val target = bsArgs(1).asInstanceOf[SSA.InvokeDynamic.HandleArg]
        MethodSig(
          target.cls,
          target.name,
          target.desc,
          target.tag == Opcodes.H_INVOKESTATIC
        )
    }

    val classes = allVertices2.collect{
      case n: SSA.GetField => n.owner
      case n: SSA.PutField => n.owner
      case n: SSA.GetStatic => n.cls
      case n: SSA.PutStatic => n.cls
    }

    val result = Walker.MethodResult(Nil, sig.desc.ret, finalInsns, false, Nil)

    (result, called, classes)
  }

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

  def constructSSAProgram(sig: MethodSig, mn: MethodNode) = {
    val phiMerges0 = mutable.LinkedHashSet.empty[SSA.Phi]

    val insns = mn.instructions.iterator().asScala.toVector
    val insnIndices = insns.zipWithIndex.toMap

    val regionStarts = findRegionStarts(insns)
    val decoration = insns.zip(regionStarts).toMap
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)
    println("================ DECORATED ================")
    println(Renderer.renderInsns(mn.instructions, printer, methodPrinter, decorate = i => " " + pprint.apply(decoration(i))))
    val startRegionLookup = findStartRegionLookup(insns, regionStarts)
    val program = extractControlFlow(
      insns,
      i => regionStarts(insnIndices(i)),
      joptimize.bytecode.Analyzer.analyze(
        sig.cls.name, mn,
        new BytecodeToSSA(phiMerges0, startRegionLookup, regionStarts),
        new SSA.ChangedState(null)
      ),
      startRegionLookup
    )
    program
  }

  def findStartRegionLookup(insns: IndexedSeq[AbstractInsnNode],
                            regionStarts: IndexedSeq[Option[SSA.Block]]) = {
    val startRegionLookup = new Array[SSA.Block](insns.size)
    var currentRegion: SSA.Block = null

    for(i <- insns.indices){
      for(x <- regionStarts(i)) currentRegion = x
      startRegionLookup(i) = currentRegion
    }
    startRegionLookup
  }

  def findRegionStarts(insns: Vector[AbstractInsnNode]) = {
    val jumpTargets = insns
      .collect {
        case n: TableSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
        case n: LookupSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
        case n: JumpInsnNode => Seq(n.label) ++ Option(n.getNext)
        case n if n == insns.head => Seq(n)
      }
      .flatten

    val blockStarts = (insns.take(1) ++ jumpTargets).toSet

    val regionStarts =
      for (i <- insns.indices)
        yield {
          if (!blockStarts.contains(insns(i))) None
          else Some(new SSA.Merge(i, Set()): SSA.Block)
        }
    regionStarts
  }

  def extractControlFlow(insns: Vector[AbstractInsnNode],
                         regionStarts: AbstractInsnNode => Option[SSA.Block],
                         frames: Array[Frame[SSA.Val, SSA.State]],
                         findStartRegion: Int => SSA.Block) = {
    def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)

    def mergeBlocks(lhs0: AbstractInsnNode, rhs: SSA.Control): Unit = {
      val lhs = regionStarts(lhs0).get.asInstanceOf[SSA.Merge]
      pprint.log((lhs, rhs))
      lhs.incoming += rhs
      rhs.downstreamAdd(lhs)
    }

    val terminals = insns.zipWithIndex.flatMap{case (insn, i) =>
      val newNodes = (insn.getOpcode, insn) match{
        case (RETURN, insn) => (insn, new SSA.Return(frames(i).state, findStartRegion(i)), i) :: Nil

        case (IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn) =>
          (insn, new SSA.ReturnVal(frames(i).state, findStartRegion(i), frameTop(i, 0)), i) :: Nil

        case (ATHROW, insn) => (insn, new SSA.AThrow(frames(i).state, findStartRegion(i), frameTop(i, 0)), i) :: Nil

        case (GOTO, insn: JumpInsnNode) =>
          mergeBlocks(insn.label, findStartRegion(i))
          Nil

        case (LOOKUPSWITCH, insn: LookupSwitchInsnNode) =>
          val startRegion = findStartRegion(i)
          val keys = insn.keys.asScala.map(_.toInt)
          val labels = insn.labels.asScala
          val n = new SSA.LookupSwitch(startRegion, frameTop(i, 0), keys)
          for((k, l) <- keys.zip(labels)){
            mergeJumpTarget(new SSA.Case(n, k), l, regionStarts, mergeBlocks, startRegion)
          }
          mergeBlocks(insn.dflt, new SSA.Default(n))
          Nil

        case (TABLESWITCH, insn: TableSwitchInsnNode) =>
          val startRegion = findStartRegion(i)
          val keys = Range.inclusive(insn.min, insn.max)
          val labels = insn.labels.asScala
          val n = new SSA.TableSwitch(startRegion, frameTop(i, 0), insn.min, insn.max)
          for((k, l) <- keys.zip(labels)){
            mergeJumpTarget(new SSA.Case(n, k), l, regionStarts, mergeBlocks, startRegion)
          }
          mergeBlocks(insn.dflt, new SSA.Default(n))
          Nil

        case (IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE, insn: JumpInsnNode) =>
          val startRegion = findStartRegion(i)
          val n = new SSA.UnaBranch(startRegion, frameTop(i, 0), SSA.UnaBranch.lookup(insn.getOpcode))
          mergeJumpTarget(new SSA.True(n), insn.label, regionStarts, mergeBlocks, startRegion)
          mergeJumpTarget(new SSA.False(n), insn.getNext, regionStarts, mergeBlocks, startRegion)
          Nil

        case (IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE, insn: JumpInsnNode) =>
          val startRegion = findStartRegion(i)
          val n = new SSA.BinBranch(startRegion, frameTop(i, 1), frameTop(i, 0), SSA.BinBranch.lookup(insn.getOpcode))
          mergeJumpTarget(new SSA.True(n), insn.label, regionStarts, mergeBlocks, startRegion)
          mergeJumpTarget(new SSA.False(n), insn.getNext, regionStarts, mergeBlocks, startRegion)
          Nil

        case _ =>
          if (Option(insn.getNext).exists(regionStarts(_).isDefined)){
            mergeBlocks(insn.getNext, findStartRegion(i))
          }
          Nil
      }
      newNodes

    }

    val locals = Range(0, frames(0).getLocals)
      .map(frames(0).getLocal)
      .filter(_.jtype != JType.Prim.V)

    Program(locals.map(_.asInstanceOf[SSA.Arg]), terminals.map(_._2))
  }

  /**
    * Similar to `mergeBlocks`, but does some work to re-wire the phi nodes
    * associated with the regions following conditional jump target blocks
    * refer to the conditional jump target blocks in their incoming list,
    * rather than the shared upstream block containing the jump
    */
  def mergeJumpTarget(destBlock: SSA.Block,
                      destInsn: AbstractInsnNode,
                      regionStarts: AbstractInsnNode => Option[SSA.Block],
                      mergeBlocks: (AbstractInsnNode, SSA.Control) => Unit,
                      startReg: SSA.Block) = {

    mergeBlocks(destInsn, destBlock)
    val mergeNode = regionStarts(destInsn).get
    mergeNode.downstreamList.collect { case phi: SSA.Phi =>
      phi.incoming = phi.incoming.map {
        case (k, v) =>
          if (k == startReg) {
            destBlock.downstreamAdd(phi)
            startReg.downstreamRemove(phi)
            (destBlock, v)
          }
          else (k, v)
      }

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
    case phi: SSA.Phi if phi.getSize != 0 =>
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
    *
    * @param liveArgs Which of the method's original arguments end up being live:
    *                 possibly contributing to the execution of the method. Other
    *                 arguments are candidate for removal since they don't do
    *                 anything
    *
    * @param inferredReturn The return type of the method, narrowed to potentially
    *                       a more specific value given what we learned from
    *                       analyzing the method body.
    *
    * @param methodBody The optimized instruction list of the optimized method
    *
    * @param pure Whether the method's only contribution to the computation is
    *             its return value: without side effects, IO, or exceptions.
    *             Such methods are candidates for re-ordering or outright
    *             elimination if their return value does not end up being used.
    */
  case class MethodResult(liveArgs: Seq[Boolean],
                          inferredReturn: IType,
                          methodBody: InsnList,
                          pure: Boolean,
                          seenTryCatchBlocks: Seq[TryCatchBlockNode])
}