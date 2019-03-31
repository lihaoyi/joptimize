package joptimize.analysis

import collection.JavaConverters._
import joptimize.bytecode.Frame
import joptimize.model.{JType, Program, SSA}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.{AbstractInsnNode, JumpInsnNode, LookupSwitchInsnNode, TableSwitchInsnNode}

object ControlFlowExtraction {

  def findStartRegionLookup(insns: IndexedSeq[AbstractInsnNode],
                            regionStarts: IndexedSeq[Option[SSA.Merge]]) = {
    val startRegionLookup = new Array[SSA.Merge](insns.size)
    var currentRegion: SSA.Merge = null

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
          else Some(new SSA.Merge(i, Set()))
        }
    regionStarts
  }

  def extractControlFlow(insns: Vector[AbstractInsnNode],
                         regionStarts: AbstractInsnNode => Option[SSA.Merge],
                         frames: Array[Frame[SSA.Val, SSA.State]],
                         findStartRegion: Int => SSA.Merge) = {
    def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)

    def mergeBlocks(lhs: SSA.Merge, rhs: SSA.Block): Unit = {
      lhs.asInstanceOf[SSA.Merge].incoming += rhs
      rhs.downstreamAdd(lhs)
    }

    /**
      * Similar to `mergeBlocks`, but does some work to re-wire the phi nodes
      * associated with the regions following conditional jump target blocks
      * refer to the conditional jump target blocks in their incoming list,
      * rather than the shared upstream block containing the jump
      */
    def mergeJumpTarget(destBlock: SSA.Block,
                        destInsn: AbstractInsnNode,
                        startReg: SSA.Merge) = {
      val startRegion = regionStarts(destInsn).get
      mergeBlocks(startRegion, destBlock)
      startRegion
        .downstreamList
        .collect { case phi: SSA.Phi if phi.block == startRegion =>
          phi.incoming = phi.incoming.map {
            case (k, v) if k == startReg =>
              destBlock.downstreamAdd(phi)
              startReg.downstreamRemove(phi)
              (destBlock, v)

            case (k, v) => (k, v)
          }
        }
    }


    val terminals = insns.zipWithIndex.flatMap{case (insn, i) =>
      val newNodes = (insn.getOpcode, insn) match{
        case (RETURN, insn) => (insn, new SSA.Return(frames(i).state, findStartRegion(i)), i) :: Nil

        case (IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn) =>
          (insn, new SSA.ReturnVal(frames(i).state, findStartRegion(i), frameTop(i, 0)), i) :: Nil

        case (ATHROW, insn) => (insn, new SSA.AThrow(frames(i).state, findStartRegion(i), frameTop(i, 0)), i) :: Nil

        case (GOTO, insn: JumpInsnNode) =>
          mergeBlocks(regionStarts(insn.label).get, findStartRegion(i))
          Nil

        case (LOOKUPSWITCH, insn: LookupSwitchInsnNode) =>
          val startRegion = findStartRegion(i)
          val keys = insn.keys.asScala.map(_.toInt)
          val labels = insn.labels.asScala
          val n = new SSA.LookupSwitch(frames(i).state, startRegion, frameTop(i, 0), keys)
          for((k, l) <- keys.zip(labels)){
            mergeJumpTarget(new SSA.Case(n, k), l, startRegion)
          }
          mergeJumpTarget(new SSA.Default(n), insn.dflt, startRegion)
          Nil

        case (TABLESWITCH, insn: TableSwitchInsnNode) =>
          val startRegion = findStartRegion(i)
          val keys = Range.inclusive(insn.min, insn.max)
          val labels = insn.labels.asScala
          val n = new SSA.TableSwitch(frames(i).state, startRegion, frameTop(i, 0), insn.min, insn.max)
          for((k, l) <- keys.zip(labels)){
            mergeJumpTarget(new SSA.Case(n, k), l, startRegion)
          }
          mergeJumpTarget(new SSA.Default(n), insn.dflt, startRegion)
          Nil

        case (IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE, insn: JumpInsnNode) =>
          val startRegion = findStartRegion(i)
          val n = new SSA.UnaBranch(frames(i).state, startRegion, frameTop(i, 0), SSA.UnaBranch.lookup(insn.getOpcode))
          mergeJumpTarget(new SSA.True(n), insn.label, startRegion)
          mergeJumpTarget(new SSA.False(n), insn.getNext, startRegion)
          Nil

        case (IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE, insn: JumpInsnNode) =>
          val startRegion = findStartRegion(i)
          val n = new SSA.BinBranch(frames(i).state, startRegion, frameTop(i, 1), frameTop(i, 0), SSA.BinBranch.lookup(insn.getOpcode))
          mergeJumpTarget(new SSA.True(n), insn.label, startRegion)
          mergeJumpTarget(new SSA.False(n), insn.getNext, startRegion)
          Nil

        case _ =>
          if (Option(insn.getNext).exists(regionStarts(_).isDefined)){
            mergeBlocks(regionStarts(insn.getNext).get, findStartRegion(i))
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

}
