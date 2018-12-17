package joptimize


import joptimize.Bytecode.Fixed

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.annotation.tailrec
import scala.collection.mutable

class Walker(isInterface: JType.Cls => Boolean,
             lookupMethod: MethodSig => Option[MethodNode],
             visitedMethods: mutable.Map[(MethodSig, Seq[IType]), (IType, InsnList)],
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
                 seenMethods: Set[(MethodSig, Seq[IType])]): (IType, InsnList) = {

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

      val visitedBlocks = mutable.LinkedHashMap.empty[(AbstractInsnNode, Frame), InsnList]
      var lastBlock: Option[(AbstractInsnNode, Frame)] = None
      val methodReturns = mutable.Buffer.empty[IType]
      def walkBlock(blockStart: AbstractInsnNode,
                    blockState0: Frame,
                    seenBlocks0: Set[AbstractInsnNode]): (Boolean, InsnList) = {
        val blockState =
          if (!seenBlocks0.contains(blockStart)) blockState0
          else blockState0.widen
        val seenBlocks = seenBlocks0 + blockStart
        val finalInsnList = new InsnList

        def walkNextBlock(destBlockStart: AbstractInsnNode, destBlockState: Frame) = {
          /**
            * Walk another block. Automatically inserts a GOTO if the next block isn't
            * going to be immediately after the current block, and elides the GOTO otherwise
            *
            * Shouldn't be used if there isn't any possibility of the next block
            * seamlessly following the current, e.g. in the case of switches where
            * every branch needs a jump.
            */
          val preWalkLastBlock = lastBlock
          val (fresh, jumped) = walkBlock(destBlockStart, destBlockState, seenBlocks)
          if (!fresh || !preWalkLastBlock.exists(_._1 != (destBlockStart, blockState))){
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
        @tailrec def walkInsn(currentInsn: AbstractInsnNode, currentState: Frame): Unit = {
//          println(Util.prettyprint(currentInsn))
//          println("    " + currentState)
          val nextState = currentState.execute(currentInsn, dataflow)

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
          def constantFold(newInsn: AbstractInsnNode) = {
            val stackEffect = Bytecode.stackEffect(currentInsn.getOpcode).asInstanceOf[Fixed]
            if (stackEffect.push == 1){
              nextState.stack.last match{
                case IType.I(v) =>
                  for(i <- 0 until stackEffect.pop) finalInsnList.add(new InsnNode(POP))
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
                  for(i <- 0 until stackEffect.pop) finalInsnList.add(new InsnNode(POP))
                  finalInsnList.add(
                    v match{
                      case 0 => new InsnNode(LCONST_0)
                      case 1 => new InsnNode(LCONST_1)
                      case _ => new LdcInsnNode(java.lang.Long.valueOf(v))
                    }
                  )
                case IType.F(v) =>
                  for(i <- 0 until stackEffect.pop) finalInsnList.add(new InsnNode(POP))
                  finalInsnList.add(
                    v match{
                      case 0 => new InsnNode(FCONST_0)
                      case 1 => new InsnNode(FCONST_1)
                      case 2 => new InsnNode(FCONST_2)
                      case _ => new LdcInsnNode(java.lang.Float.valueOf(v))
                    }
                  )
                case IType.D(v) =>
                  for(i <- 0 until stackEffect.pop) finalInsnList.add(new InsnNode(POP))
                  finalInsnList.add(
                    v match{
                      case 0 => new InsnNode(DCONST_0)
                      case 1 => new InsnNode(DCONST_1)
                      case _ => new LdcInsnNode(java.lang.Double.valueOf(v))
                    }
                  )

                case _ =>
                  finalInsnList.add(newInsn)
              }
            }else{
              finalInsnList.add(newInsn)
            }
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
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
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
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: IincInsnNode =>
              val n = new IincInsnNode(current.`var`, current.incr)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: InsnNode =>

              current.getOpcode match{
                case ARETURN =>
                  finalInsnList.add(new InsnNode(current.getOpcode))
                  methodReturns.append(currentState.stack.last)
                case RETURN | DRETURN | FRETURN | IRETURN | LRETURN =>
                  finalInsnList.add(new InsnNode(current.getOpcode))
                case _ =>
                  constantFold(new InsnNode(current.getOpcode))
                  if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
                  else walkInsn(current.getNext, nextState)
              }


            case current: IntInsnNode =>
              val n = new IntInsnNode(current.getOpcode, current.operand)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: InvokeDynamicInsnNode => ???

            case current: JumpInsnNode =>
              def jumpBlock(pred: Boolean): Boolean = {
                for(_ <- 0 until Bytecode.stackEffect(current.getOpcode).asInstanceOf[Fixed].pop) {
                  finalInsnList.add(new InsnNode(POP))
                }
                if (pred) walkNextBlock(current.label, nextState)
                pred
              }
              (
                current.getOpcode,
                currentState.stack.lift(currentState.stack.length - 1),
                currentState.stack.lift(currentState.stack.length - 2)
              ) match{
                case (IFEQ, Some(IType.I(i)), _) =>
                  if (!jumpBlock(i == 0)) walkNextBlock(current.getNext, nextState)

                case (IFNE, Some(IType.I(i)), _) =>
                  if (!jumpBlock(i != 0)) walkNextBlock(current.getNext, nextState)

                case (IFLT, Some(IType.I(i)), _) =>
                  if (!jumpBlock(i < 0)) walkNextBlock(current.getNext, nextState)

                case (IFGE, Some(IType.I(i)), _) =>
                  if (!jumpBlock(i >= 0)) walkNextBlock(current.getNext, nextState)

                case (IFGT, Some(IType.I(i)), _) =>
                  if (!jumpBlock(i > 0)) walkNextBlock(current.getNext, nextState)

                case (IFLE, Some(IType.I(i)), _) =>
                  if (!jumpBlock(i <= 0)) walkNextBlock(current.getNext, nextState)

                case (IF_ICMPEQ, Some(IType.I(i1)), Some(IType.I(i2))) =>
                  if (!jumpBlock(i1 == i2)) walkNextBlock(current.getNext, nextState)

                case (IF_ICMPNE, Some(IType.I(i1)), Some(IType.I(i2))) =>
                  if (!jumpBlock(i1 != i2)) walkNextBlock(current.getNext, nextState)

                case (IF_ICMPLT, Some(IType.I(i1)), Some(IType.I(i2))) =>
                  if (!jumpBlock(i1 > i2)) walkNextBlock(current.getNext, nextState)

                case (IF_ICMPGE, Some(IType.I(i1)), Some(IType.I(i2))) =>
                  if (!jumpBlock(i1 <= i2)) walkNextBlock(current.getNext, nextState)

                case (IF_ICMPGT, Some(IType.I(i1)), Some(IType.I(i2))) =>
                  if (!jumpBlock(i1 <= i2)) walkNextBlock(current.getNext, nextState)

                case (IF_ICMPLE, Some(IType.I(i1)), Some(IType.I(i2))) =>
                  if (!jumpBlock(i1 >= i2)) walkNextBlock(current.getNext, nextState)

                case (GOTO, _, _) => jumpBlock(true)

                case _ => // JSR, IFNULL, IFNONNULL, IF_ACMPEQ, IF_ACMPNE, anything else
                  // We don't know how to handle these, so walk both cases
                  val n = new JumpInsnNode(current.getOpcode, null)
                  finalInsnList.add(n)
                  walkNextBlock(current.getNext, nextState)
                  val (fresh, jumped) = walkBlock(current.label, nextState, seenBlocks)
                  n.label = jumped.getFirst.asInstanceOf[LabelNode]
              }
            case current: LabelNode =>
              finalInsnList.add(new LabelNode())
              walkNextBlock(current.getNext, nextState)

            case current: LdcInsnNode =>
              finalInsnList.add(new LdcInsnNode(current.cst))
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: LineNumberNode =>
              finalInsnList.add(new LineNumberNode(current.line, current.start))
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

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
                      )._1
                    }
                  }
                )

              finalInsnList.add(mangled)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) {
                walkNextBlock(currentInsn.getNext, currentState.execute(mangled, dataflow))
              } else {
                walkInsn(current.getNext, currentState.execute(mangled, dataflow))
              }

            case current: MultiANewArrayInsnNode =>
              val n = new MultiANewArrayInsnNode(current.desc, current.dims)
              finalInsnList.add(n)
              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: TableSwitchInsnNode =>
              val n = new TableSwitchInsnNode(current.min, current.max, null)
              finalInsnList.add(n)
              n.dflt = walkBlock(current.dflt, nextState, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]
              n.labels = current.labels.asScala.map(walkBlock(_, nextState, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]).asJava

            case current: TypeInsnNode =>
              visitedClasses.add(JType.Cls(current.desc))
              constantFold(new TypeInsnNode(current.getOpcode, current.desc))

              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)

            case current: VarInsnNode =>
              constantFold(new VarInsnNode(current.getOpcode, current.`var`))

              if (currentInsn.getNext.isInstanceOf[LabelNode]) walkNextBlock(currentInsn.getNext, nextState)
              else walkInsn(current.getNext, nextState)
          }
        }
        if (!visitedBlocks.contains((blockStart, blockState))){
//          println("NEW BLOCK " + (currentInsn, currentState))
          visitedBlocks((blockStart, blockState)) = finalInsnList
          lastBlock = Some((blockStart, blockState))
          walkInsn(blockStart, blockState)
//          println("END BLOCK")
          (true, finalInsnList)
        }else{
//          println("OLD BLOCK")
          val res = new InsnList()
          val label = new LabelNode()
          val jump = new JumpInsnNode(
            GOTO,
            visitedBlocks((blockStart, blockState)).getFirst match{
              case l: LabelNode => l
              case _ =>
                val newLabel = new LabelNode()
                visitedBlocks((blockStart, blockState)).insert(newLabel)
                newLabel
            }
          )
          res.add(label)
          res.add(jump)
          visitedBlocks((label, blockState)) = res
          (false, res)
        }
      }
//      pprint.log(sig -> insns.size)

      walkBlock(insns.getFirst, Frame.initial(maxLocals, maxStack, args), Set())

//      pprint.log(sig -> "END")

      val resultType =
        if (methodReturns.isEmpty) sig.desc.ret
        else merge(methodReturns)

      val outputInsns = new InsnList
      visitedBlocks.valuesIterator.foreach(outputInsns.add)

      (resultType, outputInsns)
    })
  }


  def mangleMethodCallInsn(frame: Frame,
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