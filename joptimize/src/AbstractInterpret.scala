package joptimize

import java.util
import collection.JavaConverters._
import org.objectweb.asm.Label
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.collection.mutable

object AbstractInterpret {
  class Frame(val locals: Array[Inferred],
              val stack: Array[Inferred]){
    override def hashCode() = {
      locals.iterator.map(_.##).sum + stack.iterator.map(_.##).sum
    }

    override def equals(obj: scala.Any) = obj match{
      case other: Frame =>
        locals.sameElements(other.locals) && stack.sameElements(other.stack)
      case _ => false
    }
  }

  def interpretMethod(insns: InsnList,
                      args: List[Inferred],
                      maxStack: Int,
                      maxLocals: Int) = {
    // - One-pass walk through the instruction list of a method, starting from
    //   narrowed argument types
    //
    // - Recursively visit any called methods, in order to pick up their
    //   narrowed return types
    //
    // - Simulate stack for all operations, in order to propagate narrowed types
    //
    // - Instructions visited multiple times non-recursively (e.g. jumped to
    //   from different source instructions) are duplicated once
    //   per narrowed-incoming-abstract-state
    //
    // - Instructions visited multiple times recursively (e.g. loops):
    //
    //   - If the recursive visit state is the same as or narrower than the
    //     initial state, simply visit the already-visited instructions
    //
    //   - If the recursive visit state is wider than the initial state,
    //     visit the block with the original, un-narrowed incoming state
    //
    //   - Only one recursive duplicate visit ever happens; the un-narrowed
    //     incoming state is by definition wider than all possible narrowed
    //     states

    val initialFrame = new Frame(new Array(maxLocals), new Array(maxStack))
    var i = 0
    for(arg <- args){
      for(_ <- 0 until arg.getSize){
        initialFrame.locals(i) = arg
        i += 1
      }
    }

    // the frame state and the *original* instruction, mapped to the *final* instruction
    val seenJumpTargets = mutable.Map.empty[(Frame, AbstractInsnNode), AbstractInsnNode]

    def rec(currentInsn: AbstractInsnNode,
            currentState: Frame,
            finalInsnList: InsnList): InsnList = {

      currentInsn match{
        case current: FieldInsnNode =>
          val n = new FieldInsnNode(current.getOpcode, current.owner, current.name, current.desc)
          finalInsnList.add(n)
          rec(current.getNext, currentState, finalInsnList)

        case current: FrameNode =>
          val n = new FrameNode(
            current.`type`,
            current.local.size,
            current.local.toArray,
            current.stack.size,
            current.stack.toArray
          )
          finalInsnList.add(n)
          rec(current.getNext, currentState, finalInsnList)

        case current: IincInsnNode =>
          val n = new IincInsnNode(current.`var`, current.incr)
          finalInsnList.add(n)
          rec(current.getNext, currentState, finalInsnList)

        case current: InsnNode =>
          val n = new InsnNode(current.getOpcode)
          finalInsnList.add(n)
          rec(current.getNext, currentState, finalInsnList)

        case current: IntInsnNode =>
          val n = new IntInsnNode(current.getOpcode, current.operand)
          finalInsnList.add(n)
          rec(current.getNext, currentState, finalInsnList)

        case current: InvokeDynamicInsnNode => ???

        case current: JumpInsnNode =>
          val n = new JumpInsnNode(current.getOpcode, null)
          finalInsnList.add(n)
          // only GOTOs are unconditional; all other jumps may or may not jump
          // to the target label
          if (current.getOpcode != GOTO) rec(current.getNext, currentState, finalInsnList)
          val jumped = rec(current.label, currentState, new InsnList)
          n.label = jumped.asInstanceOf[LabelNode]
          finalInsnList

        case current: LabelNode =>
          finalInsnList.add(new LabelNode())
          rec(current.getNext, currentState, finalInsnList)
          finalInsnList

        case current: LdcInsnNode =>
          finalInsnList.add(new LdcInsnNode(current.cst))
          rec(current.getNext, currentState, finalInsnList)
          finalInsnList

        case current: LineNumberNode =>
          finalInsnList.add(new LineNumberNode(current.line, current.start))
          rec(current.getNext, currentState, finalInsnList)
          finalInsnList

        case current: LookupSwitchInsnNode =>
          val n = new LookupSwitchInsnNode(null, Array(), Array())
          finalInsnList.add(n)
          n.dflt = rec(current.dflt, currentState, finalInsnList).asInstanceOf[LabelNode]
          n.keys = current.keys
          n.labels = current.labels.asScala.map(rec(_, currentState, finalInsnList).asInstanceOf[LabelNode]).asJava
          finalInsnList

        case current: MethodInsnNode =>
          val n = new MethodInsnNode(
            current.getOpcode, current.owner, current.name, current.desc, current.itf
          )
          finalInsnList.add(n)
          finalInsnList

        case current: MultiANewArrayInsnNode =>
          val n = new MultiANewArrayInsnNode(current.desc, current.dims)
          finalInsnList.add(n)
          rec(current.getNext, currentState, finalInsnList)

        case current: TableSwitchInsnNode =>
          val n = new TableSwitchInsnNode(current.min, current.max, null)
          finalInsnList.add(n)
          n.dflt = rec(current.dflt, currentState, finalInsnList).asInstanceOf[LabelNode]
          n.labels = current.labels.asScala.map(rec(_, currentState, finalInsnList).asInstanceOf[LabelNode]).asJava
          finalInsnList

        case current: TypeInsnNode =>
          val n = new TypeInsnNode(current.getOpcode, current.desc)
          finalInsnList.add(n)
          rec(current.getNext, currentState, finalInsnList)

        case current: VarInsnNode =>
          val n = new VarInsnNode(current.getOpcode, current.`var`)
          finalInsnList.add(n)
          rec(current.getNext, currentState, finalInsnList)
      }
    }
    rec(insns.getFirst, initialFrame, new InsnList())
  }

  def findLoops(insns: InsnList) = {

    val queue = mutable.Queue(insns.getFirst)
    val insnToIndex = mutable.Map.empty[AbstractInsnNode, Int]
    val edges = mutable.Buffer.empty[AbstractInsnNode]
    while(queue.nonEmpty){
      val current = queue.dequeue()
      insnToIndex.getOrElseUpdate(current, {
        val newIndex = edges.length
        current.getType
        newIndex
      })
    }
  }

}