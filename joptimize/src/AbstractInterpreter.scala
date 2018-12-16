package joptimize


import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.annotation.tailrec
import scala.collection.mutable

class AbstractInterpreter(isInterface: JType.Cls => Boolean,
                          lookupMethod: MethodSig => Option[MethodNode],
                          visitedMethods: mutable.Map[(MethodSig, Seq[IType]), (IType, InsnList)],
                          findSubtypes: JType.Cls => List[JType.Cls],
                          isConcrete: MethodSig => Boolean,
                          merge: Seq[IType] => IType) {

  def walkMethod(sig: MethodSig,
                 insns: InsnList,
                 args: Seq[IType],
                 maxLocals: Int,
                 maxStack: Int,
                 seen0: Set[(MethodSig, Seq[IType])]): (IType, InsnList) = {

    visitedMethods.getOrElseUpdate((sig, args.drop(if (sig.static) 0 else 1)), {
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
      val methodReturns = mutable.Buffer.empty[IType]
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
              val clinitSig = MethodSig(current.owner, "<clinit>", Desc.read("()V"), true)
              if (!seen.contains((clinitSig, Nil))) {
                for(clinit <- lookupMethod(clinitSig)){
                  walkMethod(
                    clinitSig,
                    clinit.instructions,
                    Nil,
                    clinit.maxLocals,
                    clinit.maxStack,
                    seen ++ Seq((clinitSig, Nil))
                  )
                }
              }
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

                    if (seen((staticSig, types))) staticSig.desc.ret
                    else {
                      val clinitSig = MethodSig(staticSig.cls, "<clinit>", Desc.read("()V"), true)
                      if (!seen.contains((clinitSig, Nil))) {
                        for(clinit <- lookupMethod(clinitSig)){
                          walkMethod(
                            clinitSig,
                            clinit.instructions,
                            Nil,
                            clinit.maxLocals,
                            clinit.maxStack,
                            seen ++ Seq((clinitSig, Nil))
                          )
                        }
                      }

                      walkMethod(
                        staticSig,
                        lookupMethod(staticSig).get.instructions,
                        types.toList,
                        lookupMethod(staticSig).get.maxLocals,
                        lookupMethod(staticSig).get.maxStack,
                        seen
                      )._1
                    }
                  }
                )

              finalInsnList.add(mangled)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) {
                walkBlock(currentInsn.getNext, currentState.execute(mangled, Dataflow))
              } else {
                walkInsn(current.getNext, currentState.execute(mangled, Dataflow))
              }

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
        if (methodReturns.isEmpty) sig.desc.ret
        else merge(methodReturns) match{
          case IType.Intersect(classes) =>
            classes.distinct match{
              case Seq(singleton) => singleton
              case _ => sig.desc.ret
            }
          case j: JType => j
        }
      val outputInsns = new InsnList
      visitedBlocks.valuesIterator.foreach(outputInsns.add)

      (resultType, outputInsns)
    })
  }


  def mangleInstruction(frame: Frame,
                        insn: AbstractInsnNode,
                        static: Boolean,
                        special: Boolean,
                        recurse: (MethodSig, Seq[IType]) => IType): AbstractInsnNode = {
    val called = insn.asInstanceOf[MethodInsnNode]


    val calledDesc = Desc.read(called.desc)
    val calledSelf = if (static) Nil else Seq(JType.Cls(called.owner))
    val originalTypes = calledSelf ++ calledDesc.args.toSeq

    val inferredTypes =
      (frame.stack.length - originalTypes.map(_.getSize).sum)
        .until(frame.stack.length)
        .map(frame.stack(_))

    val sig = MethodSig(called.owner, called.name, Desc.read(called.desc), static)

    val (concreteSigs, abstractSigs) =
      if (special) (Seq(sig), Nil)
      else{
        val subtypes = findSubtypes(sig.cls)
        val possibleSigs = subtypes.map(st => sig.copy(cls = st)) ++ Seq(sig)
        possibleSigs.partition(isConcrete)
      }

    for(interfaceSig <- abstractSigs){
      visitedMethods((interfaceSig, originalTypes.drop(1))) = (
        interfaceSig.desc.ret,
        new InsnList
      )
    }

    val narrowReturnType = merge(
      concreteSigs.map(recurse(_, inferredTypes))
    )

    if (inferredTypes == originalTypes) insn // No narrowing
    else {
      val descChanged =
        (static && inferredTypes != originalTypes) ||
        (!static && inferredTypes.drop(1) != originalTypes.drop(1)) // ignore self type

      val (mangledName, mangledDesc) =
        if (!descChanged) (called.name, Desc.read(called.desc))
        else {
          val zipped = inferredTypes.zip(originalTypes)
          Util.mangle(
            called.name,
            (if (static) zipped else zipped.drop(1)).map(t => JType.fromIType(t._1, t._2)),
            JType.fromIType(narrowReturnType, sig.desc.ret)
          )
        }

      // Owner type changed! We may need to narrow from an invokeinterface to an invokevirtual
      val newOwner = JType.fromIType(
        frame.stack(frame.stack.length - originalTypes.map(_.getSize).sum), JType.Cls(called.owner)
      ).name
      (isInterface(called.owner), isInterface(newOwner)) match{
        case (false, true) => ??? // cannot widen interface into class!
        case (true, false) => new MethodInsnNode(INVOKEVIRTUAL, newOwner, mangledName, mangledDesc.unparse)
        case _ =>
          called.owner = newOwner
          called.name = mangledName
          called.desc = mangledDesc.unparse
          called
      }
    }
  }
}