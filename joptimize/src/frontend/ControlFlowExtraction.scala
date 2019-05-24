package joptimize.frontend

import joptimize.Logger
import joptimize.analyzer.Renderer
import joptimize.model.{JType, MethodBody, SSA}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.{
  AbstractInsnNode,
  JumpInsnNode,
  LookupSwitchInsnNode,
  TableSwitchInsnNode,
  TryCatchBlockNode
}

import scala.collection.JavaConverters._
import scala.collection.mutable

object ControlFlowExtraction {

  def findStartRegionLookup(
    insns: IndexedSeq[AbstractInsnNode],
    regionStarts: IndexedSeq[Option[SSA.Merge]]
  ) = {
    val startRegionLookup = new Array[SSA.Merge](insns.size)
    var currentRegion: SSA.Merge = null

    for (i <- insns.indices) {
      for (x <- regionStarts(i)) currentRegion = x
      startRegionLookup(i) = currentRegion
    }
    startRegionLookup
  }

  def findRegionStarts(
    insns: Vector[AbstractInsnNode],
    tryCacheBlockNodes: Seq[TryCatchBlockNode]
  ) = {
    val jumpTargets = insns
      .collect {
        case n: TableSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
        case n: LookupSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
        case n: JumpInsnNode => Seq(n.label) ++ Option(n.getNext)
      }
      .flatten
      .++(Seq(insns.head))
      .++(tryCacheBlockNodes.map(_.handler))

    val blockStarts = (insns.take(1) ++ jumpTargets).toSet

    var first = true
    val regionStarts =
      for (i <- insns.indices)
        yield {
          if (!blockStarts.contains(insns(i))) None
          else if (first) {
            first = false
            Some(new SSA.Start(null))
          } else Some(new SSA.Merge(mutable.LinkedHashMap(), null, Nil))
        }
    regionStarts
  }

  def extractControlFlow(
    insns: Vector[AbstractInsnNode],
    regionStarts: AbstractInsnNode => Option[SSA.Merge],
    frames: Array[Frame[SSA.Val, SSA.State]],
    findStartRegion: Int => SSA.Merge,
    log: Logger.Method
  ) = {
    def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)

    def mergeBlocks(lhs: SSA.Merge, rhs: SSA.Block, rhsState: SSA.State): Unit = {
      lhs.asInstanceOf[SSA.Merge].incoming += ((rhs, rhsState))
      rhsState.next = lhs
      rhs.next = lhs
    }

    /**
      * Similar to `mergeBlocks`, but does some work to re-wire the phi nodes
      * associated with the regions following conditional jump target blocks
      * refer to the conditional jump target blocks in their incoming list,
      * rather than the shared upstream block containing the jump
      */
    def mergeJumpTarget(
      destBlock: SSA.Block,
      destInsn: AbstractInsnNode,
      startReg: SSA.Merge,
      state: SSA.State
    ) = {
      val startRegion = regionStarts(destInsn).get
      mergeBlocks(startRegion, destBlock, state)
      startRegion
        .downstreamList
        .collect {
          case phi: SSA.Phi if phi.block == startRegion =>
            val arr = phi.incoming.toArray
            phi.incoming.clear()
            for ((k, v) <- arr) {
              if (k == startReg) {
                destBlock.nextPhis ++= Seq(phi)
                startReg.nextPhis = startReg.nextPhis.filter(_ != phi)
                phi.incoming(destBlock) = v
              } else {
                phi.incoming(k) = v
              }
            }
        }
    }

    val terminals = insns.zipWithIndex.flatMap {
      case (_, i) if frames(i) == null => None
      case (insn0, i) =>
        val newNodes = (insn0.getOpcode, insn0) match {
          case (RETURN, insn) =>
            val n = new SSA.Return(frames(i - 1).state, findStartRegion(i))
            findStartRegion(i).next = n
            (insn, n, i) :: Nil

          case (IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn) =>
            val n = new SSA.ReturnVal(frames(i - 1).state, findStartRegion(i), frameTop(i - 1, 0))
            findStartRegion(i).next = n
            (insn, n, i) :: Nil

          case (ATHROW, insn) =>
            val n =
              new SSA.AThrow(frames(i - 1).state, findStartRegion(i), frameTop(i - 1, 0), None)
            findStartRegion(i).next = n
            (insn, n, i) :: Nil

          case (GOTO, insn: JumpInsnNode) =>
            mergeBlocks(regionStarts(insn.label).get, findStartRegion(i), frames(i).state)
            Nil

          case (LOOKUPSWITCH, insn: LookupSwitchInsnNode) =>
            val startRegion = findStartRegion(i)
            val keys = insn.keys.asScala.map(_.toInt)
            val labels = insn.labels.asScala
            val n = new SSA.LookupSwitch(
              frames(i - 1).state,
              startRegion,
              frameTop(i - 1, 0),
              keys,
              null,
              null
            )
            val cases = for ((k, l) <- keys.zip(labels)) yield {
              val caseNode = new SSA.Case(n, k, null)
              mergeJumpTarget(caseNode, l, startRegion, new SSA.State(caseNode))
              k -> caseNode
            }
            val default = new SSA.Default(n, null)
            mergeJumpTarget(default, insn.dflt, startRegion, new SSA.State(default))
            n.cases = mutable.LinkedHashMap.empty[Int, SSA.Case] ++ cases
            n.default = default
            Nil

          case (TABLESWITCH, insn: TableSwitchInsnNode) =>
            val startRegion = findStartRegion(i)
            val keys = Range.inclusive(insn.min, insn.max)
            val labels = insn.labels.asScala
            val n = new SSA.TableSwitch(
              frames(i - 1).state,
              startRegion,
              frameTop(i - 1, 0),
              insn.min,
              insn.max,
              null,
              null
            )
            val cases = for ((k, l) <- keys.zip(labels)) yield {
              val caseNode = new SSA.Case(n, k, null)
              mergeJumpTarget(caseNode, l, startRegion, new SSA.State(caseNode))
              k -> caseNode
            }
            val default = new SSA.Default(n, null)
            mergeJumpTarget(default, insn.dflt, startRegion, new SSA.State(default))
            n.cases = mutable.LinkedHashMap.empty[Int, SSA.Case] ++ cases
            n.default = default
            Nil

          case (IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IFNULL | IFNONNULL, insn: JumpInsnNode) =>
            val startRegion = findStartRegion(i)
            val n = new SSA.UnaBranch(
              frames(i - 1).state,
              startRegion,
              frameTop(i - 1, 0),
              SSA.UnaBranch.lookup(insn.getOpcode),
              null,
              null
            )

            log.pprint(n)
            log.pprint(n.state)
            log.pprint(frames(i - 1).state)
            log.pprint(frames(i - 1).state.next)
            val trueBranch = new SSA.True(n, null)
            val falseBranch = new SSA.False(n, null)
            mergeJumpTarget(trueBranch, insn.label, startRegion, new SSA.State(trueBranch))
            mergeJumpTarget(falseBranch, insn.getNext, startRegion, new SSA.State(falseBranch))
            startRegion.next = n
            Nil

          case (
              IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ |
              IF_ACMPNE,
              insn: JumpInsnNode
              ) =>
            val startRegion = findStartRegion(i)
            val n = new SSA.BinBranch(
              frames(i - 1).state,
              startRegion,
              frameTop(i - 1, 1),
              frameTop(i - 1, 0),
              SSA.BinBranch.lookup(insn.getOpcode),
              null,
              null
            )

            val trueBranch = new SSA.True(n, null)
            val falseBranch = new SSA.False(n, null)
            mergeJumpTarget(trueBranch, insn.label, startRegion, new SSA.State(trueBranch))
            mergeJumpTarget(falseBranch, insn.getNext, startRegion, new SSA.State(falseBranch))
            startRegion.next = n
            Nil

          case (_, insn) =>
            if (Option(insn.getNext).exists(regionStarts(_).isDefined)) {
              log.pprint(regionStarts(insn.getNext).get)
              log.pprint(findStartRegion(i))
              log.pprint(frames(i).state)
              mergeBlocks(regionStarts(insn.getNext).get, findStartRegion(i), frames(i).state)
            }
            Nil
        }
        newNodes

    }

    val locals = Range(0, frames(0).getLocals)
      .map(frames(0).getLocal)
      .filter(_.jtype != JType.Prim.V)

    val program = MethodBody(locals.map(_.asInstanceOf[SSA.Arg]), terminals.map(_._2))

    program
  }

}
