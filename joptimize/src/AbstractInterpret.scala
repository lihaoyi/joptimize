package joptimize


import collection.JavaConverters._
import org.objectweb.asm.{Label, Type}
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.annotation.tailrec
import scala.collection.mutable

class AbstractInterpret(isInterface: String => Boolean,
                        lookupMethod: MethodSig => MethodNode,
                        visitedMethods: mutable.Map[(MethodSig, Seq[Type]), Type]) {

  def interpretMethod(sig: MethodSig,
                      insns: InsnList,
                      args: List[Inferred],
                      maxLocals: Int,
                      maxStack: Int): Type = visitedMethods.getOrElseUpdate((sig, args.map(_.value)), {
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

    val visitedBlocks = mutable.Map.empty[(AbstractInsnNode, Frame), InsnList]
    val methodReturns = mutable.Buffer.empty[Inferred]
    def walkBlock(currentInsn: AbstractInsnNode,
                  currentState: Frame): InsnList = {
      val finalInsnList = new InsnList

      visitedBlocks((currentInsn, currentState)) = finalInsnList

      /**
        * Walks a single basic block, returning:
        *
        * - Instruction list of that basic block
        */
      @tailrec def walkBlockInsn(currentInsn: AbstractInsnNode, currentState: Frame): InsnList = {
        val nextState = currentState.execute(currentInsn, Dataflow)
        currentInsn match{
          case current: FieldInsnNode =>
            val n = new FieldInsnNode(current.getOpcode, current.owner, current.name, current.desc)
            finalInsnList.add(n)
            walkBlockInsn(current.getNext, nextState)

          case current: FrameNode =>
            val n = new FrameNode(
              current.`type`,
              current.local.size,
              current.local.toArray,
              current.stack.size,
              current.stack.toArray
            )
            finalInsnList.add(n)
            walkBlockInsn(current.getNext, nextState)

          case current: IincInsnNode =>
            val n = new IincInsnNode(current.`var`, current.incr)
            finalInsnList.add(n)
            walkBlockInsn(current.getNext, nextState)

          case current: InsnNode =>
            val n = new InsnNode(current.getOpcode)
            finalInsnList.add(n)
            n.getOpcode match{
              case ARETURN =>
                methodReturns.append(currentState.stack.last)
                finalInsnList
              case RETURN | DRETURN | FRETURN | IRETURN | LRETURN =>
                finalInsnList
              case _ => walkBlockInsn(current.getNext, nextState)
            }


          case current: IntInsnNode =>
            val n = new IntInsnNode(current.getOpcode, current.operand)
            finalInsnList.add(n)
            walkBlockInsn(current.getNext, nextState)

          case current: InvokeDynamicInsnNode => ???

          case current: JumpInsnNode =>
            val n = new JumpInsnNode(current.getOpcode, null)
            finalInsnList.add(n)
            // only GOTOs are unconditional; all other jumps may or may not jump
            // to the target label
            if (current.getOpcode != GOTO) walkBlock(current.getNext, nextState)
            val jumped = walkBlock(current.label, nextState)
            n.label = jumped.getFirst.asInstanceOf[LabelNode]
            finalInsnList

          case current: LabelNode =>
            finalInsnList.add(new LabelNode())
            walkBlockInsn(current.getNext, nextState)

          case current: LdcInsnNode =>
            finalInsnList.add(new LdcInsnNode(current.cst))
            walkBlockInsn(current.getNext, nextState)

          case current: LineNumberNode =>
            finalInsnList.add(new LineNumberNode(current.line, current.start))
            walkBlockInsn(current.getNext, nextState)

          case current: LookupSwitchInsnNode =>
            val n = new LookupSwitchInsnNode(null, Array(), Array())
            finalInsnList.add(n)
            n.dflt = walkBlock(current.dflt, nextState).getFirst.asInstanceOf[LabelNode]
            n.keys = current.keys
            n.labels = current.labels.asScala.map(walkBlock(_, nextState).getFirst.asInstanceOf[LabelNode]).asJava
            finalInsnList

          case current: MethodInsnNode =>
            val copy = new MethodInsnNode(
              current.getOpcode, current.owner, current.name, current.desc, current.itf
            )
            val mangled = copy.getOpcode match{
              case INVOKEVIRTUAL | INVOKEINTERFACE | INVOKESPECIAL =>
                mangleInstruction(
                  currentState, copy,
                  static = false,
                  recurse = (staticSig, types) => interpretMethod(
                    staticSig.copy(clsName = types(0).getInternalName),
                    lookupMethod(staticSig.copy(clsName = types(0).getInternalName)).instructions,
                    types.toList.map(Inferred(_)),
                    lookupMethod(staticSig.copy(clsName = types(0).getInternalName)).maxLocals,
                    lookupMethod(staticSig.copy(clsName = types(0).getInternalName)).maxStack
                  )
                )
              case INVOKESTATIC =>
                mangleInstruction(
                  currentState, copy,
                  static = true,
                  recurse = (staticSig, types) => interpretMethod(
                    staticSig,
                    lookupMethod(staticSig).instructions,
                    types.toList.map(Inferred(_)),
                    lookupMethod(staticSig).maxLocals,
                    lookupMethod(staticSig).maxStack
                  )
                )
              case _ => ??? // do nothing
            }
            finalInsnList.add(mangled)
            finalInsnList

          case current: MultiANewArrayInsnNode =>
            val n = new MultiANewArrayInsnNode(current.desc, current.dims)
            finalInsnList.add(n)
            walkBlockInsn(current.getNext, nextState)

          case current: TableSwitchInsnNode =>
            val n = new TableSwitchInsnNode(current.min, current.max, null)
            finalInsnList.add(n)
            n.dflt = walkBlock(current.dflt, nextState).getFirst.asInstanceOf[LabelNode]
            n.labels = current.labels.asScala.map(walkBlock(_, nextState).getFirst.asInstanceOf[LabelNode]).asJava
            finalInsnList

          case current: TypeInsnNode =>
            val n = new TypeInsnNode(current.getOpcode, current.desc)
            finalInsnList.add(n)
            walkBlockInsn(current.getNext, nextState)

          case current: VarInsnNode =>
            val n = new VarInsnNode(current.getOpcode, current.`var`)
            finalInsnList.add(n)
            walkBlockInsn(current.getNext, nextState)
        }
      }
      walkBlockInsn(currentInsn, currentState)
    }

    walkBlock(insns.getFirst, Frame.initial(maxLocals, maxStack, args))

    if (methodReturns.isEmpty) Type.getMethodType(sig.desc).getReturnType
    else methodReturns.reduce(Dataflow.merge).value
  })


  def mangleInstruction(frame: Frame,
                        insn: AbstractInsnNode,
                        static: Boolean,
                        recurse: (MethodSig, Seq[Type]) => Type): AbstractInsnNode = {
    val called = insn.asInstanceOf[MethodInsnNode]

    val calledDesc = Type.getType(called.desc)
    val calledSelf = if (static) Nil else Seq(Type.getObjectType(called.owner))
    val originalTypes = calledSelf ++ calledDesc.getArgumentTypes.toSeq

    val inferredTypes =
      (frame.stack.length - originalTypes.map(_.getSize).sum)
        .until(frame.stack.length)
        .map(frame.stack(_).value)

    val sig = MethodSig(called.owner, called.name, called.desc, static)

    if (inferredTypes == originalTypes) {
      // No narrowing
      recurse(sig, originalTypes)
      insn
    } else {
      val newNode =
        if (static || Type.getObjectType(called.owner) == frame.stack(frame.stack.length - originalTypes.length).value) called
        else {
          // Owner type changed! We may need to narrow from an invokeinterface to an invokevirtual
          val newOwner = frame.stack(frame.stack.length - originalTypes.map(_.getSize).sum).value.getInternalName
          (isInterface(called.owner), isInterface(newOwner)) match{
            case (true, false) =>
              new MethodInsnNode(INVOKEVIRTUAL, newOwner, called.name, called.desc)
            case (false, true) => ???
            case _ =>
              called.owner = newOwner
              called
          }
        }

      val narrowReturnType = recurse(sig, inferredTypes)
      val descChanged =
        (static && inferredTypes == originalTypes) ||
        (!static && inferredTypes.drop(1) == originalTypes.drop(1)) // ignore self type
      val (mangledName, mangledDesc) =
        if (descChanged) (newNode.name, newNode.desc)
        else mangle(newNode.name, inferredTypes, narrowReturnType)

      newNode.name = mangledName
      called.desc = mangledDesc
      newNode
    }
  }

  def mangle(name: String, stackTypes: Seq[Type], narrowReturnType: Type) = {
    val mangledName = name + "__" + stackTypes.mkString("__").replace('/', '_').replace(';', '_')
    val mangledDesc = Type.getMethodDescriptor(narrowReturnType, stackTypes:_*)
    (mangledName, mangledDesc)
  }
}