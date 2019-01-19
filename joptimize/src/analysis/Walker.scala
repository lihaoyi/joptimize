package joptimize.analysis

import joptimize.Util
import joptimize.bytecode.Frame
import joptimize.graph.HavlakLoopTree
import joptimize.model._

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.collection.{immutable, mutable}

class Walker(isInterface: JType.Cls => Boolean,
             lookupMethod: MethodSig => Option[MethodNode],
             visitedMethods: mutable.LinkedHashMap[(MethodSig, Seq[IType]), Walker.MethodResult],
             visitedClasses: mutable.LinkedHashSet[JType.Cls],
             findSubtypes: JType.Cls => List[JType.Cls],
             findSupertypes: JType.Cls => Seq[JType.Cls],
             isConcrete: MethodSig => Boolean,
             exists: MethodSig => Boolean,
             merge: Seq[IType] => IType,
//             typer: Typer,
             ignore: String => Boolean) {

  def walkMethod(sig: MethodSig,
                 mn: MethodNode,
                 args: Seq[IType],
                 seenMethods0: Set[(MethodSig, Seq[IType])]): Walker.MethodResult = {

    visitedMethods.getOrElseUpdate((sig, args.drop(if (sig.static) 0 else 1)), {

      println("+" * 20 + sig + "+" * 20)
      println(Renderer.renderInsns(mn.instructions))

      val phiMerges0 = mutable.LinkedHashSet.empty[SSA.Phi]

      val insns = mn.instructions.iterator().asScala.toVector
      val insnIndices = insns.zipWithIndex.toMap

      val regionStarts = findRegionStarts(insns)

      val startRegionLookup = findStartRegionLookup(insns, regionStarts)

      val frames = joptimize.bytecode.Analyzer.analyze(
        sig.cls.name, mn,
        new StepEvaluator(phiMerges0, startRegionLookup, regionStarts)
      )

      val terminals = extractControlFlow(
        insns,
        i => regionStarts(insnIndices(i)),
        frames,
        startRegionLookup
      )

      removeDeadPhis(phiMerges0, terminals.map(_._2: SSA.Node).toSet)

      simplifyPhiMerges(phiMerges0, regionStarts.flatten)

      val program = Program(terminals.map(_._2))

      val allVertices = Util.breadthFirstAggregation[SSA.Node](program.allTerminals.toSet)(_.upstream)._1

      val preScheduleIndex = Indexer.findSaveable(program, Map.empty, allVertices)

      println()
      println(Renderer.renderSSA(program, preScheduleIndex))

      val controlFlowEdges = Util.findControlFlowGraph(program)
      val allBlocks = controlFlowEdges
        .flatMap{case (k, v) => Seq(k, v)}
        .collect{case b: SSA.Block => b}

      val blockEdges = controlFlowEdges.flatMap{
        case (k: SSA.Block, v: SSA.Jump) => Nil
        case (k: SSA.Jump, v: SSA.Block) => Seq(k.block -> v)
        case (k: SSA.Block, v: SSA.Block) => Seq(k -> v)
      }

      println()
      println(Renderer.renderControlFlowGraph(controlFlowEdges, preScheduleIndex.savedLocals))

      val loopTree = HavlakLoopTree.analyzeLoops(blockEdges, allBlocks)

      println()
      println(Renderer.renderLoopTree(loopTree, preScheduleIndex.savedLocals))

      val dominators = Dominator.findDominators(blockEdges, allBlocks)

      { // Just for debugging
        val nodesToBlocks = Scheduler.schedule(
          program, loopTree, dominators,
          controlFlowEdges, preScheduleIndex.savedLocals.mapValues(_._2),
          allVertices
        )

        val postScheduleIndex = Indexer.findSaveable(program, nodesToBlocks, allVertices)

        println()
        println(Renderer.renderSSA(program, postScheduleIndex, nodesToBlocks))

      }

      RegisterAllocator.apply(program, dominators.immediateDominators)

      val (allVertices2, _, _) = Util.breadthFirstAggregation[SSA.Node](program.allTerminals.toSet)(_.upstream)

      val nodesToBlocks = Scheduler.schedule(
        program, loopTree, dominators,
        controlFlowEdges, preScheduleIndex.savedLocals.mapValues(_._2),
        allVertices2
      )

      val postRegisterAllocIndex = Indexer.findSaveable(program, nodesToBlocks, allVertices2)

      println()
      println(Renderer.renderSSA(program, postRegisterAllocIndex, nodesToBlocks))

      val blockCode = CodeGen(
        program,
        allVertices2,
        nodesToBlocks,
        controlFlowEdges,
        postRegisterAllocIndex.savedLocals,
        postRegisterAllocIndex.finalOrderingMap
      )

      val output = new InsnList()
      for((insns, footer) <- blockCode){
        insns.foreach(output.add)
        footer.foreach(output.add)
      }

      println(Renderer.renderBlockCode(blockCode, output))

      Walker.MethodResult(Nil, sig.desc.ret, output, false, Nil)
    })
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
                         frames: Array[Frame[SSA.Val]],
                         findStartRegion: Int => SSA.Block) = {
    def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)

    def mergeBlocks(lhs0: AbstractInsnNode, rhs: SSA.Control, rhsInsn: Option[AbstractInsnNode] = None): Unit = {
      val lhs = regionStarts(lhs0).get
      (lhs, rhs) match {
        case (l: SSA.Merge, r) =>
          l.incoming += r
          r.downstreamAdd(l)
      }
    }


    val terminals = insns.map(i => (i.getOpcode, i)).zipWithIndex.collect {
      case ((RETURN, insn), i) => (insn, new SSA.Return(findStartRegion(i)), i) :: Nil

      case ((IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn), i) =>
        (insn, new SSA.ReturnVal(findStartRegion(i), frameTop(i, 0)), i) :: Nil

      case ((ATHROW, insn), i) => (insn, new SSA.AThrow(findStartRegion(i), frameTop(i, 0)), i) :: Nil

      case ((GOTO, insn: JumpInsnNode), i) =>
        mergeBlocks(insn.label, findStartRegion(i), Some(insn))
        Nil

      case ((IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE, insn: JumpInsnNode), i) =>
        val n = new SSA.UnaBranch(findStartRegion(i), frameTop(i, 0), SSA.UnaBranch.lookup(insn.getOpcode))
        mergeBlocks(insn.label, new SSA.True(n))
        mergeBlocks(insn.getNext, new SSA.False(n))

        Nil

      case ((IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE, insn: JumpInsnNode), i) =>
        val startReg = findStartRegion(i)
        val n = new SSA.BinBranch(startReg, frameTop(i, 1), frameTop(i, 0), SSA.BinBranch.lookup(insn.getOpcode))
        mergeBlocks(insn.label, new SSA.True(n))
        mergeBlocks(insn.getNext, new SSA.False(n))
        Nil

      case ((_, insn), i) if Option(insn.getNext).exists(regionStarts(_).isDefined) =>
        mergeBlocks(insn.getNext, findStartRegion(i), Some(insn))
        Nil
    }.flatten
    terminals
  }

  def removeDeadPhis(phiMerges0: mutable.LinkedHashSet[SSA.Phi], terminals: Set[SSA.Node]) = {

    val (allVertices, _, _) =
      Util.breadthFirstAggregation[SSA.Node](terminals)(_.upstream)
    // Remove dead phi nodes that may have been inserted during SSA construction
    for (phi <- phiMerges0) {
      if (!allVertices.contains(phi)) {
        for (up <- phi.upstream) {
          up.downstreamRemove(phi)
        }
      }
    }
  }

  def simplifyPhiMerges(phiMerges0: mutable.LinkedHashSet[SSA.Phi], regions: Seq[SSA.Block]) = {
    val queue = phiMerges0 ++ regions
//    queue.foreach(_.checkLinks())

    while (queue.nonEmpty) {
      val current = queue.head
      queue.remove(current)
      val replacementOpt = current match {
        case phi: SSA.Phi =>
          val filteredValues = phi.incoming.filter(_._2 != phi)

          if (filteredValues.map(_._2).size == 1) Some(filteredValues.head._2)
          else None

        case reg: SSA.Merge =>
          if (reg.incoming.size == 1) Some(reg.incoming.head)
          else None

        case _ => None
      }
      for (replacement <- replacementOpt) {
        for (v <- current.upstream) v.downstreamRemove(current)
        replacement.downstreamRemove(current)
        val deltaDownstream = current.downstreamList.filter(_ != current)
        deltaDownstream.foreach(replacement.downstreamAdd)

        for (down <- deltaDownstream) SSA.update(down, current, replacement)
        queue.add(replacement)
      }
    }
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