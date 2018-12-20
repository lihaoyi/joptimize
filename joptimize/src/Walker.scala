package joptimize


import joptimize.Bytecode.Fixed

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.annotation.tailrec
import scala.collection.mutable
object Walker{
  case class MethodResult(liveArgs: Seq[Boolean], inferredReturn: IType, methodBody: InsnList)
}
class Walker(isInterface: JType.Cls => Boolean,
             lookupMethod: MethodSig => Option[MethodNode],
             visitedMethods: mutable.Map[(MethodSig, Seq[IType]), Walker.MethodResult],
             visitedClasses: mutable.Set[JType.Cls],
             findSubtypes: JType.Cls => List[JType.Cls],
             isConcrete: MethodSig => Boolean,
             merge: Seq[IType] => IType,
             dataflow: Dataflow) {

  def walkMethod(sig: MethodSig,
                 insns: InsnList,
                 args: Seq[IType],
                 maxLocals: Int,
                 maxStack: Int,
                 seenMethods: Set[(MethodSig, Seq[IType])]): Walker.MethodResult = {

    visitedMethods.getOrElseUpdate((sig, args.drop(if (sig.static) 0 else 1)), {
      val seen = seenMethods ++ Seq((sig, args))
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

      val visitedBlocks = mutable.LinkedHashMap.empty[(AbstractInsnNode, Frame[IType]), (InsnList, Frame[LValue])]
      var lastBlock: Option[(AbstractInsnNode, Frame[LValue])] = None
      val methodReturns = mutable.Buffer.empty[LValue]
      val labelMapping = mutable.Map.empty[LabelNode, List[LabelNode]]
      def walkBlock(blockStart: AbstractInsnNode,
                    blockState0: Frame[LValue],
                    seenBlocks0: Set[AbstractInsnNode]): (Boolean, InsnList) = {
        val blockState =
          if (!seenBlocks0.contains(blockStart)) blockState0
          else blockState0.widen
        val seenBlocks = seenBlocks0 + blockStart
        val finalInsnList = new InsnList

        /**
          * Walk another block. Automatically inserts a GOTO if the next block isn't
          * going to be immediately after the current block, and elides the GOTO otherwise
          *
          * Shouldn't be used if there isn't any possibility of the next block
          * seamlessly following the current, e.g. in the case of switches where
          * every branch needs a jump.
          */
        def walkNextBlock(destBlockStart: AbstractInsnNode, destBlockState: Frame[LValue]) = {

          val preWalkLastBlock = lastBlock
          val (fresh, jumped) = walkBlock(destBlockStart, destBlockState, seenBlocks)
          if (!fresh || !preWalkLastBlock.exists(_ != (destBlockStart, blockState))){
            val l = jumped.getFirst match{
              case l: LabelNode => l
              case _ =>
                val l = new LabelNode()
                jumped.insert(l)
                l
            }

            finalInsnList.add(new JumpInsnNode(GOTO, l))
          }
          jumped
        }

        /**
          * Walks a single basic block, returning:
          *
          * - Instruction list of that basic block
          */
        @tailrec def walkInsn(currentInsn: AbstractInsnNode, currentState: Frame[LValue]): Unit = {
//          println(Util.prettyprint(currentInsn))
//          println("    " + currentState)
          val nextState = currentState.execute(currentInsn, dataflow)

          /**
            * Walk the next instruction as a new block, if it is a label. If not
            * then return `false` so we can tail-recursively walk it as a simple
            * instruction
            */
          def walkNextLabel(nextState1: Frame[LValue] = nextState) = {
            if (currentInsn.getNext.isInstanceOf[LabelNode]) {
              walkNextBlock(currentInsn.getNext, nextState1)
              true
            } else false
          }

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
              if (!walkNextLabel()) walkInsn(current.getNext, nextState)

            case current: FrameNode =>
              val n = new FrameNode(
                current.`type`,
                current.local.size,
                current.local.toArray,
                current.stack.size,
                current.stack.toArray
              )
              finalInsnList.add(n)
              if (!walkNextLabel()) walkInsn(current.getNext, nextState)

            case current: IincInsnNode =>
              finalInsnList.add(new IincInsnNode(current.`var`, current.incr))
              if (!walkNextLabel()) walkInsn(current.getNext, nextState)

            case current: InsnNode =>
              current.getOpcode match{
                case ARETURN =>
                  finalInsnList.add(new InsnNode(current.getOpcode))
                  methodReturns.append(currentState.stack.last)
                case RETURN | DRETURN | FRETURN | IRETURN | LRETURN =>
                  finalInsnList.add(new InsnNode(current.getOpcode))
                case _ =>
                  constantFold(currentInsn, new InsnNode(current.getOpcode), nextState, finalInsnList)
                  if (!walkNextLabel()) walkInsn(current.getNext, nextState)
              }

            case current: IntInsnNode =>
              finalInsnList.add(new IntInsnNode(current.getOpcode, current.operand))
              if (!walkNextLabel()) walkInsn(current.getNext, nextState)

            case current: InvokeDynamicInsnNode => ???

            case current: JumpInsnNode =>
              walkJump(
                walkBlock(_, _, seenBlocks), finalInsnList,
                walkNextBlock, currentState, nextState, current
              )
            case current: LabelNode =>
              val newLabel = new LabelNode()
              labelMapping(current) = newLabel :: labelMapping.getOrElse(current, Nil)
              finalInsnList.add(newLabel)
              walkNextBlock(current.getNext, nextState)

            case current: LdcInsnNode =>
              finalInsnList.add(new LdcInsnNode(current.cst))
              if (!walkNextLabel()) walkInsn(current.getNext, nextState)

            case current: LineNumberNode =>
              for(newLabel <- labelMapping.getOrElse(current.start, Nil)){
                finalInsnList.add(new LineNumberNode(current.line, newLabel))
              }
              if (!walkNextLabel()) walkInsn(current.getNext, nextState)

            case current: LookupSwitchInsnNode =>
              val n = new LookupSwitchInsnNode(null, Array(), Array())
              finalInsnList.add(n)
              n.dflt = walkBlock(current.dflt, nextState, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]
              n.keys = current.keys
              n.labels = current.labels.asScala.map(walkBlock(_, nextState, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]).asJava

            case current: MethodInsnNode =>
              val copy = new MethodInsnNode(
                current.getOpcode, current.owner, current.name, current.desc, current.itf
              )

              val mangled =
                if (current.owner.startsWith("java/")) copy
                else mangleMethodCallInsn(
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
                      ).inferredReturn
                    }
                  }
                )

              finalInsnList.add(mangled)

              val nextState1 = currentState.execute(mangled, dataflow)
              if (!walkNextLabel(nextState1)) walkInsn(current.getNext, nextState1)

            case current: MultiANewArrayInsnNode =>
              finalInsnList.add(new MultiANewArrayInsnNode(current.desc, current.dims))
              if (!walkNextLabel()) walkInsn(current.getNext, nextState)

            case current: TableSwitchInsnNode =>
              val n = new TableSwitchInsnNode(current.min, current.max, null)
              finalInsnList.add(n)
              n.dflt = walkBlock(current.dflt, nextState, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]
              n.labels = current.labels.asScala.map(walkBlock(_, nextState, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]).asJava

            case current: TypeInsnNode =>
              visitedClasses.add(JType.Cls(current.desc))
              constantFold(currentInsn, new TypeInsnNode(current.getOpcode, current.desc), nextState, finalInsnList)

              if (!walkNextLabel()) walkInsn(current.getNext, nextState)

            case current: VarInsnNode =>
              constantFold(currentInsn, new VarInsnNode(current.getOpcode, current.`var`), nextState, finalInsnList)

              if (!walkNextLabel()) walkInsn(current.getNext, nextState)
          }
        }
        val typeState = blockState.map(_.tpe)
        visitedBlocks.get((blockStart, typeState)) match{
          case Some((insns, frame)) =>
          //          println("OLD BLOCK")
            visitedBlocks((blockStart, typeState)) = (
              insns,
              frame.zipWith(blockState){(x1, x2) => x1.merge(x2)}
            )
            (false, insns)
          case None =>
//          println("NEW BLOCK " + (currentInsn, currentState))
            visitedBlocks((blockStart, typeState)) = (finalInsnList, blockState)
            lastBlock = Some((blockStart, blockState))
            walkInsn(blockStart, blockState)
  //          println("END BLOCK")
            (true, finalInsnList)
        }
      }
//      pprint.log(sig -> insns.size)

      walkBlock(
        insns.getFirst,
        Frame.initial(
          maxLocals, maxStack,
          args.map(new LValue(_, None, Nil)),
          new LValue(JType.Null, None, Nil)
        ),
        Set()
      )

//      pprint.log(sig -> "END")

      val resultType =
        if (methodReturns.isEmpty) sig.desc.ret
        else merge(methodReturns.map(_.tpe))

      val outputInsns = new InsnList
      visitedBlocks.valuesIterator.foreach(x => outputInsns.add(x._1))

      Walker.MethodResult(sig.desc.args.map(_ => true), resultType, outputInsns)
    })
  }

  /**
    * Removes a jump if we already statically know the destination, replacing
    * it with POPs and an optional GOTO directly to the end destination.
    *
    * If we do not know the destination, we simply emit the jump in the final
    * bytecode, and walk the two jump targets to make sure we have somewhere
    * to jump to
    */
  def walkJump(walkBlock: (AbstractInsnNode, Frame[LValue]) => (Boolean, InsnList),
               finalInsnList: InsnList,
               walkNextBlock: (AbstractInsnNode, Frame[LValue]) => InsnList,
               currentState: Frame[LValue],
               nextState: Frame[LValue],
               current: JumpInsnNode) = {
    def jumpBlock(pred: Boolean): Unit = {
      popN(finalInsnList, Bytecode.stackEffect(current.getOpcode).asInstanceOf[Fixed].pop)
      walkNextBlock(if (pred) current.label else current.getNext, nextState)
    }

    (
      current.getOpcode,
      currentState.stack.lift(currentState.stack.length - 1).map(_.tpe),
      currentState.stack.lift(currentState.stack.length - 2).map(_.tpe)
    ) match {
      case (IFEQ, Some(IType.I(i)), _) => jumpBlock(i == 0)
      case (IFNE, Some(IType.I(i)), _) => jumpBlock(i != 0)
      case (IFLT, Some(IType.I(i)), _) => jumpBlock(i < 0)
      case (IFGE, Some(IType.I(i)), _) => jumpBlock(i >= 0)
      case (IFGT, Some(IType.I(i)), _) => jumpBlock(i > 0)
      case (IFLE, Some(IType.I(i)), _) => jumpBlock(i <= 0)
      case (IF_ICMPEQ, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 == i2)
      case (IF_ICMPNE, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 != i2)
      case (IF_ICMPLT, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 > i2)
      case (IF_ICMPGE, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 <= i2)
      case (IF_ICMPGT, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 <= i2)
      case (IF_ICMPLE, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 >= i2)
      case (GOTO, _, _) => jumpBlock(true)

      case _ => // JSR, IFNULL, IFNONNULL, IF_ACMPEQ, IF_ACMPNE, anything else
        // We don't know how to handle these, so walk both cases
        val n = new JumpInsnNode(current.getOpcode, null)
        finalInsnList.add(n)
        walkNextBlock(current.getNext, nextState)

        val (fresh, jumped) = walkBlock(current.label, nextState)
        n.label = jumped.getFirst.asInstanceOf[LabelNode]
    }
  }
  def popN(finalInsnList: InsnList, n: Int) = {
    for(_ <- 0 until n) finalInsnList.add(new InsnNode(POP))
  }
  /**
    * JOptimize's constant folding simply replaces all instructions with
    * known outputs by a constant (ICONST, LCONST, LDC, ...) preceded by
    * the correct number of pops. We are not able to remove the
    * instruction because we cannot tell during the initial
    * dataflow-ordered pass which instruction's output ends up getting
    * used in future.
    *
    * A second pass in reverse-dataflow order can then trivially
    * collapse all the unused instructions later (TBD)
    */
  def constantFold(currentInsn: AbstractInsnNode,
                   newInsn: AbstractInsnNode,
                   nextState: Frame[LValue],
                   finalInsnList: InsnList) = {
    val stackEffect = Bytecode.stackEffect(currentInsn.getOpcode).asInstanceOf[Fixed]
    if (stackEffect.push == 1){
      nextState.stack.last.tpe match{
        case IType.I(v) =>
          popN(finalInsnList, stackEffect.pop)
          finalInsnList.add(
            v match{
              case -1 => new InsnNode(ICONST_M1)
              case 0 => new InsnNode(ICONST_0)
              case 1 => new InsnNode(ICONST_1)
              case 2 => new InsnNode(ICONST_2)
              case 3 => new InsnNode(ICONST_3)
              case 4 => new InsnNode(ICONST_4)
              case 5 => new InsnNode(ICONST_5)
              case _ => new LdcInsnNode(java.lang.Integer.valueOf(v))
            }
          )
        case IType.J(v) =>
          popN(finalInsnList, stackEffect.pop)
          finalInsnList.add(
            v match{
              case 0 => new InsnNode(LCONST_0)
              case 1 => new InsnNode(LCONST_1)
              case _ => new LdcInsnNode(java.lang.Long.valueOf(v))
            }
          )
        case IType.F(v) =>
          popN(finalInsnList, stackEffect.pop)
          finalInsnList.add(
            v match{
              case 0 => new InsnNode(FCONST_0)
              case 1 => new InsnNode(FCONST_1)
              case 2 => new InsnNode(FCONST_2)
              case _ => new LdcInsnNode(java.lang.Float.valueOf(v))
            }
          )
        case IType.D(v) =>
          popN(finalInsnList, stackEffect.pop)
          finalInsnList.add(
            v match{
              case 0 => new InsnNode(DCONST_0)
              case 1 => new InsnNode(DCONST_1)
              case _ => new LdcInsnNode(java.lang.Double.valueOf(v))
            }
          )

        case _ => finalInsnList.add(newInsn)
      }
    }else finalInsnList.add(newInsn)
  }

  def mangleMethodCallInsn(frame: Frame[LValue],
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
        .map(frame.stack(_).tpe)

    val sig = MethodSig(called.owner, called.name, Desc.read(called.desc), static)

    val (concreteSigs, abstractSigs) =
      if (special) (Seq(sig), Nil)
      else{
        val subtypes = findSubtypes(sig.cls)
        val possibleSigs = subtypes.map(st => sig.copy(cls = st)) ++ Seq(sig)
        possibleSigs.partition(isConcrete)
      }

    for(interfaceSig <- abstractSigs){
      visitedMethods((interfaceSig, originalTypes.drop(1))) = Walker.MethodResult(
        Array.fill(originalTypes.length - 1)(true),
        interfaceSig.desc.ret,
        new InsnList
      )
    }

    val narrowReturnType = merge(
      concreteSigs.map(recurse(_, inferredTypes))
    )

    if (Util.isCompatible(inferredTypes, originalTypes)) insn // No narrowing
    else {
      val descChanged =
        (static && inferredTypes != originalTypes) ||
        (!static && inferredTypes.drop(1) != originalTypes.drop(1)) // ignore self type

      val (mangledName, mangledDesc) =
        if (!descChanged) (called.name, Desc.read(called.desc))
        else {
          Util.mangle(
            called.name,
            if (static) inferredTypes else inferredTypes.drop(1),
            if (static) originalTypes else originalTypes.drop(1),
            narrowReturnType,
            calledDesc.ret
          )
        }

      // Owner type changed! We may need to narrow from an invokeinterface to an invokevirtual
      val newOwner = JType.fromIType(
        frame.stack(frame.stack.length - originalTypes.map(_.getSize).sum).tpe,
        JType.Cls(called.owner)
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