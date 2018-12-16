package joptimize


import collection.JavaConverters._
import org.objectweb.asm.{Label, Type}
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.annotation.tailrec
import scala.collection.mutable

class AbstractInterpreter(isInterface: String => Boolean,
                          lookupMethod: MethodSig => Option[MethodNode],
                          visitedMethods: mutable.Map[(MethodSig, Seq[Type]), (Type, InsnList)],
                          findSubtypes: String => List[String],
                          isConcrete: MethodSig => Boolean) {

  def interpretMethod(sig: MethodSig,
                      insns: InsnList,
                      args: List[Inferred],
                      maxLocals: Int,
                      maxStack: Int,
                      seen0: Set[(MethodSig, Seq[Inferred])]): (Type, InsnList) = {

    visitedMethods.getOrElseUpdate((sig, args.map(_.value).drop(if (sig.static) 0 else 1)), {
      val seen = seen0 ++ Seq((sig, args))
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

      val visitedBlocks = mutable.LinkedHashMap.empty[(AbstractInsnNode, Frame), InsnList]
      val methodReturns = mutable.Buffer.empty[Inferred]
      def walkBlock(currentInsn: AbstractInsnNode,
                    currentState: Frame): InsnList = {
        val finalInsnList = new InsnList

        /**
          * Walks a single basic block, returning:
          *
          * - Instruction list of that basic block
          */
        @tailrec def walkInsn(currentInsn: AbstractInsnNode, currentState: Frame): Unit = {
//          println(Util.prettyprint(currentInsn))
//          println(currentState)
          val nextState = currentState.execute(currentInsn, Dataflow)
          currentInsn match{
            case current: FieldInsnNode =>
              val n = new FieldInsnNode(current.getOpcode, current.owner, current.name, current.desc)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: FrameNode =>
              val n = new FrameNode(
                current.`type`,
                current.local.size,
                current.local.toArray,
                current.stack.size,
                current.stack.toArray
              )
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: IincInsnNode =>
              val n = new IincInsnNode(current.`var`, current.incr)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: InsnNode =>
              val n = new InsnNode(current.getOpcode)
              finalInsnList.add(n)
              n.getOpcode match{
                case ARETURN =>
                  methodReturns.append(currentState.stack.last)
                case RETURN | DRETURN | FRETURN | IRETURN | LRETURN =>
                case _ =>
                  if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
                  else walkInsn(current.getNext, nextState)
              }


            case current: IntInsnNode =>
              val n = new IntInsnNode(current.getOpcode, current.operand)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: InvokeDynamicInsnNode => ???

            case current: JumpInsnNode =>
              val n = new JumpInsnNode(current.getOpcode, null)
              finalInsnList.add(n)
              // only GOTOs are unconditional; all other jumps may or may not jump
              // to the target label
              if (current.getOpcode != GOTO) walkBlock(current.getNext, nextState)
              val jumped = walkBlock(current.label, nextState)
              n.label = jumped.getFirst.asInstanceOf[LabelNode]

            case current: LabelNode =>
              finalInsnList.add(new LabelNode())
              walkInsn(current.getNext, nextState)

            case current: LdcInsnNode =>
              finalInsnList.add(new LdcInsnNode(current.cst))
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: LineNumberNode =>
              finalInsnList.add(new LineNumberNode(current.line, current.start))
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: LookupSwitchInsnNode =>
              val n = new LookupSwitchInsnNode(null, Array(), Array())
              finalInsnList.add(n)
              n.dflt = walkBlock(current.dflt, nextState).getFirst.asInstanceOf[LabelNode]
              n.keys = current.keys
              n.labels = current.labels.asScala.map(walkBlock(_, nextState).getFirst.asInstanceOf[LabelNode]).asJava

            case current: MethodInsnNode =>
              val copy = new MethodInsnNode(
                current.getOpcode, current.owner, current.name, current.desc, current.itf
              )

              val mangled =
                if (current.owner.startsWith("java/")) copy
                else mangleInstruction(
                  currentState, copy,
                  static = copy.getOpcode == INVOKESTATIC,
                  special = copy.getOpcode == INVOKESPECIAL,
                  recurse = (staticSig, types) => {

                    if (seen((staticSig, types.map(Inferred(_))))) Type.getMethodType(staticSig.desc).getReturnType
                    else {
                      val clinitSig = MethodSig(staticSig.clsName, "<clinit>", "()V", true)
                      if (!seen.contains((clinitSig, Nil))) {
                        for(clinit <- lookupMethod(clinitSig)){
                          interpretMethod(
                            clinitSig,
                            clinit.instructions,
                            Nil,
                            clinit.maxLocals,
                            clinit.maxStack,
                            seen ++ Seq((clinitSig, Nil))
                          )
                        }
                      }

                      interpretMethod(
                        staticSig,
                        lookupMethod(staticSig).get.instructions,
                        types.toList.map(Inferred(_)),
                        lookupMethod(staticSig).get.maxLocals,
                        lookupMethod(staticSig).get.maxStack,
                        seen
                      )._1
                    }
                  }
                )

              finalInsnList.add(mangled)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, currentState.execute(mangled, Dataflow))
              else walkInsn(current.getNext, currentState.execute(mangled, Dataflow))

            case current: MultiANewArrayInsnNode =>
              val n = new MultiANewArrayInsnNode(current.desc, current.dims)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: TableSwitchInsnNode =>
              val n = new TableSwitchInsnNode(current.min, current.max, null)
              finalInsnList.add(n)
              n.dflt = walkBlock(current.dflt, nextState).getFirst.asInstanceOf[LabelNode]
              n.labels = current.labels.asScala.map(walkBlock(_, nextState).getFirst.asInstanceOf[LabelNode]).asJava

            case current: TypeInsnNode =>
              val n = new TypeInsnNode(current.getOpcode, current.desc)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: VarInsnNode =>
              val n = new VarInsnNode(current.getOpcode, current.`var`)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)
          }
        }
        if (!visitedBlocks.contains((currentInsn, currentState))){
//          println("NEW BLOCK " + (currentInsn, currentState))
          visitedBlocks((currentInsn, currentState)) = finalInsnList
          walkInsn(currentInsn, currentState)
//          println("END BLOCK")
          finalInsnList
        }else{
//          println("OLD BLOCK")
          val res = new InsnList()
          val label = new LabelNode()
          val jump = new JumpInsnNode(
            GOTO,
            visitedBlocks((currentInsn, currentState)).getFirst.asInstanceOf[LabelNode]
          )
          res.add(label)
          res.add(jump)
          visitedBlocks((label, currentState)) = res
          res
        }
      }
//      pprint.log(sig -> insns.size)

      walkBlock(insns.getFirst, Frame.initial(maxLocals, maxStack, args))


      val resultType =
        if (methodReturns.isEmpty) Type.getMethodType(sig.desc).getReturnType
        else methodReturns.reduce(Dataflow.merge).value
      val outputInsns = new InsnList
      visitedBlocks.valuesIterator.foreach(outputInsns.add)
      (resultType, outputInsns)
    })
  }


  def mangleInstruction(frame: Frame,
                        insn: AbstractInsnNode,
                        static: Boolean,
                        special: Boolean,
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

    val (concreteSigs, abstractSigs) =
      if (special) (Seq(sig), Nil)
      else{
        val subtypes = findSubtypes(sig.clsName)
        val possibleSigs = subtypes.map(st => sig.copy(clsName = st)) ++ Seq(sig)
        possibleSigs.partition(isConcrete)
      }

    for(interfaceSig <- abstractSigs){
      visitedMethods((interfaceSig, originalTypes.drop(1))) = (
        Type.getMethodType(interfaceSig.desc).getReturnType,
        new InsnList
      )
    }

    val narrowReturnType = concreteSigs
      .map(recurse(_, inferredTypes))
      .reduce((l, r) => Dataflow.merge(Inferred(l), Inferred(r)).value)

    if (inferredTypes == originalTypes) insn // No narrowing
    else {
      val descChanged =
        (static && inferredTypes != originalTypes) ||
        (!static && inferredTypes.drop(1) != originalTypes.drop(1)) // ignore self type

      val (mangledName, mangledDesc) =
        if (!descChanged) (called.name, called.desc)
        else Util.mangle(called.name, if (static) inferredTypes else inferredTypes.drop(1), narrowReturnType)

      // Owner type changed! We may need to narrow from an invokeinterface to an invokevirtual
      val newOwner = frame.stack(frame.stack.length - originalTypes.map(_.getSize).sum).value.getInternalName
      (isInterface(called.owner), isInterface(newOwner)) match{
        case (false, true) => ??? // cannot widen interface into class!
        case (true, false) => new MethodInsnNode(INVOKEVIRTUAL, newOwner, mangledName, mangledDesc)
        case _ =>
          called.owner = newOwner
          called.name = mangledName
          called.desc = mangledDesc
          called
      }
    }
  }
}