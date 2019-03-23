package joptimize.analysis

import joptimize.Util
import joptimize.bytecode.Frame
import joptimize.graph.HavlakLoopTree
import joptimize.model._

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
                 checkSideEffects: (MethodSig, Seq[IType]) => SideEffects,
                 checkSubclass: (JType.Cls, JType.Cls) => Boolean,
                 callStack: List[(MethodSig, Seq[IType])])
      : (Walker.MethodResult, Set[JType.Cls], Set[MethodSig]) = {

    if (callStack.contains(originalSig -> inferredArgs) || mn.instructions.size() == 0){
      Tuple3(
        Walker.MethodResult(
          liveArgs = Nil,
          inferredReturn = originalSig.desc.ret,
          methodBody = new InsnList(),
          sideEffects = SideEffects.Pure,
          seenTryCatchBlocks = Nil
        ),
        Set.empty,
        Set.empty
      )
    }else{
      println("+" * 20 + originalSig + "+" * 20)
      pprint.log(callStack)
      pprint.log(inferredArgs)
      assert(
        Util.isValidationCompatible(inferredArgs, originalSig, checkSubclass),
        s"Inferred param types [${inferredArgs.mkString(", ")}] is not compatible " +
          s"with declared param types [${originalSig.desc.args.mkString(", ")}]"
      )
      val printer = new Textifier
      val methodPrinter = new TraceMethodVisitor(printer)
      println("================ BYTECODE ================")
      println(Renderer.renderInsns(mn.instructions, printer, methodPrinter))

      val program = constructSSAProgram(originalSig.cls.name, mn)

      Renderer.dumpSvg(program, "initial.svg")
      removeDeadNodes(program)
      program.checkLinks()

      simplifyPhiMerges(program)
      program.checkLinks()
      Renderer.dumpSvg(program, "simplified.svg")
      println("================ INITIAL ================")

      val preScheduleNaming = Namer.apply(program, Map.empty, program.getAllVertices())

      println(Renderer.renderSSA(program, preScheduleNaming))

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

      //    println("================ PESSIMISTIC ================")
      //
      //    PartialEvaluator.apply(program)
      //
      //    removeDeadNodes(program)
      //
      //    program.checkLinks()
      //
      //    val postPessimisticNaming = Namer.apply(program, Map.empty, program.getAllVertices())
      //
      //    println(Renderer.renderSSA(program, postPessimisticNaming))
      //
      //    val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)
      //
      //    println()
      //    println(Renderer.renderLoopTree(loopTree2, preScheduleNaming.savedLocals))

      Renderer.dumpSvg(program, "pre-optimistic.svg", postScheduleNaming)
      println("================ OPTIMISTIC ================")

      val (inferred, liveBlocks) = OptimisticAnalyze.apply(
        program,
        Map.empty,
        program.getAllVertices().collect{case b: SSA.Block if b.upstream.isEmpty => b}.head,
        new ITypeLattice(
          merge,
          computeMethodSig(_, _, _, (originalSig -> inferredArgs) :: callStack),
          inferredArgs.flatMap{i => Seq.fill(i.getSize)(i)}
        ),
        postScheduleNaming
      )

      pprint.log(inferred)

      program.checkLinks()

      var aggregateSideEffects: SideEffects = SideEffects.Pure

      val calledMethodSigs = mutable.Set.empty[MethodSig]

      program.getAllVertices().foreach{

        case p: SSA.ChangedState => // do nothing
        case n: SSA.Invoke =>
          val (mangledName, mangledDesc) =
            if (n.name == "<init>") (n.name, n.desc)
            else Util.mangle(n.sig, n.srcs.map(inferred), inferred.getOrElseUpdate(n, n.desc.ret))
          val sideEffects = checkSideEffects(n.sig, n.srcs.map(inferred))
          calledMethodSigs.add(n.sig)
          aggregateSideEffects = (aggregateSideEffects, sideEffects) match{
            case (SideEffects.Pure, SideEffects.Pure) => SideEffects.Pure
            case _ => SideEffects.Global
          }
          if (sideEffects == SideEffects.Pure){
            val replacement = inferred.get(n) match{
              case Some(CType.I(v)) => Some(SSA.ConstI(v))
              case Some(CType.J(v)) => Some(SSA.ConstJ(v))
              case Some(CType.F(v)) => Some(SSA.ConstF(v))
              case Some(CType.D(v)) => Some(SSA.ConstD(v))
              case _ => None
            }
            replacement match{
              case Some(r) =>
                inferred(r) = inferred(n)
                n.upstream.foreach(_.downstreamRemoveAll(n))
                for(d <- n.downstreamList) {
                  r.downstreamAdd(d)
                  d.replaceUpstream(n, r)
                }
              case None =>
                n.name = mangledName
                n.desc = mangledDesc
            }
          }else{
            n.name = mangledName
            n.desc = mangledDesc
          }
        case p: SSA.Phi =>
          p.incoming = p.incoming.filter{t =>
            val live = liveBlocks(t._1)
            if (!live) {
              t._1.downstreamRemove(p)
              t._2.downstreamRemove(p)
            }
            live
          }
        case m: SSA.Merge =>
          m.incoming = m.incoming.filter{ t =>
            val live = liveBlocks(t)
            if (!live) t.downstreamRemove(m)
            live
          }

        case n: SSA.Val =>
          val replacement = inferred.get(n) match{
            case Some(CType.I(v)) => Some(SSA.ConstI(v))
            case Some(CType.J(v)) => Some(SSA.ConstJ(v))
            case Some(CType.F(v)) => Some(SSA.ConstF(v))
            case Some(CType.D(v)) => Some(SSA.ConstD(v))
            case _ => None
          }
          replacement.foreach{r =>
            inferred(r) = inferred(n)
            n.upstream.foreach(_.downstreamRemoveAll(n))
            for(d <- n.downstreamList) {
              r.downstreamAdd(d)
              d.replaceUpstream(n, r)
            }
          }

        case j: SSA.Jump =>
          val allTargets = j.downstreamList.collect{case b: SSA.Block => b}
          val liveTargets = allTargets.filter(liveBlocks)
          if (liveTargets.size == 1){
            println("ELIMINATING JUMP " + j)
            PartialEvaluator.replaceJump(j, liveTargets.head)
          }else if (liveTargets.size <= allTargets.size){
            for(t <- allTargets if !liveTargets.contains(t)){
              j.downstreamRemove(t)
            }
          }
        case _ => // do nothing
      }

      Renderer.dumpSvg(program, "post-optimistic.svg")
      program.checkLinks(checkDead = false)
      removeDeadNodes(program)
      Renderer.dumpSvg(program, "post-optimistic-cleanup.svg")
      program.checkLinks()

      val loopTree2 = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

      val dominators2 = Dominator.findDominators(blockEdges, allBlocks)


      { // Just for debugging
        val nodesToBlocks = Scheduler.apply(
          loopTree2, dominators2, startBlock,
          preScheduleNaming.savedLocals.mapValues(_._2), program.getAllVertices()
        )

        val postOptimisticNaming = Namer.apply(program, nodesToBlocks, program.getAllVertices())

        println()
        println(Renderer.renderSSA(program, postOptimisticNaming, nodesToBlocks))
      }

      println("================ REGISTERS ALLOCATED ================")
      RegisterAllocator.apply(program, dominators2.immediateDominators)

      val allVertices2 = Util.breadthFirstSeen[SSA.Node](program.allTerminals.toSet)(_.upstream)

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

      println("================ OUTPUT BYTECODE ================")
      println(Renderer.renderBlockCode(blockCode, finalInsns))

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
        Nil,
        inferredReturn,
        finalInsns,
        aggregateSideEffects,
        Nil
      )

      (result, classes, calledMethodSigs.toSet)
    }
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

  def constructSSAProgram(clsName: String, mn: MethodNode) = {
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
        clsName, mn,
        new BytecodeToSSA(phiMerges0, startRegionLookup, regionStarts),
        new SSA.ChangedState(regionStarts(0).get),
        i => regionStarts(i).map{ b =>
          new SSA.ChangedState(b)
        }
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

    def mergeBlocks(lhs0: AbstractInsnNode, rhs: SSA.Block): Unit = {
      val lhs = regionStarts(lhs0).get.asInstanceOf[SSA.Merge]
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
                      mergeBlocks: (AbstractInsnNode, SSA.Block) => Unit,
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
                          sideEffects: SideEffects,
                          seenTryCatchBlocks: Seq[TryCatchBlockNode])
}