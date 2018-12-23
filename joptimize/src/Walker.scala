package joptimize


import joptimize.Bytecode.Fixed

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.annotation.tailrec
import scala.collection.mutable

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

      // Equivalent to visitedBlocks.lastOption, but faster because
      // LinkedHashMap#lastOption isn't optimized and so is O(n) instead of O(1)
      var lastBlock: Option[(AbstractInsnNode, Frame[LValue])] = None
      val methodReturns = mutable.Buffer.empty[LValue]
      val terminalInsns = mutable.Buffer.empty[Terminal]
      val labelMapping = mutable.Map.empty[LabelNode, List[LabelNode]]
      var pure: Boolean = sig.name != "<init>" && sig.name != "<clinit>"
      val subCallArgLiveness = mutable.Map.empty[AbstractInsnNode, Seq[Boolean]]
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
        @tailrec def walkInsn(currentInsn: AbstractInsnNode, currentFrame: Frame[LValue]): Unit = {
//          println("    " + currentFrame)
//          val nextFrame = currentFrame.execute(currentInsn, dataflow)

          /**
            * Walk the next instruction as a new block, if it is a label. If not
            * then return `false` so we can tail-recursively walk it as a simple
            * instruction
            */
          def walkNextLabel(nextFrame1: Frame[LValue]) = {
            if (currentInsn.getNext.isInstanceOf[LabelNode]) {
              walkNextBlock(currentInsn.getNext, nextFrame1)
              true
            } else false
          }

          currentInsn match{
            case current: FieldInsnNode =>
              val clinitSig = MethodSig(current.owner, "<clinit>", Desc.read("()V"), true)

              val n = new FieldInsnNode(current.getOpcode, current.owner, current.name, current.desc)
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
              current.getOpcode match{
                case PUTFIELD | PUTSTATIC => terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last), None))
                case GETFIELD | GETSTATIC => terminalInsns.append(Terminal(n, Nil, None))
              }
//
              finalInsnList.add(n)
              val nextFrame = currentFrame.execute(n, dataflow)
              if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame)

            case current: FrameNode =>
              // We discard frame nodes; we're going to be doing a bunch of mangling
              // so they'll all be wrong, so easier just let ASM recompute them
              if (!walkNextLabel(currentFrame)) walkInsn(current.getNext, currentFrame)

            case _: IincInsnNode | _: IntInsnNode | _: LdcInsnNode | _: MultiANewArrayInsnNode =>
              val n = Util.clone(currentInsn, mutable.Map.empty)
              finalInsnList.add(n)
              val nextFrame = currentFrame.execute(n, dataflow)
              if (!walkNextLabel(nextFrame)) walkInsn(currentInsn.getNext, nextFrame)

            case current: InsnNode =>
              current.getOpcode match{
                case IASTORE | LASTORE | FASTORE | DASTORE | AASTORE | BASTORE | CASTORE | SASTORE |
                     IALOAD | LALOAD | FALOAD | DALOAD | AALOAD | BALOAD | CALOAD | SALOAD =>
                  val (newInsns, nextFrame) = constantFold(currentInsn, currentFrame)

                  newInsns.foreach(finalInsnList.add)
                  terminalInsns.append(Terminal(
                    newInsns.last,
                    currentFrame.stack.takeRight(
                      Bytecode.stackEffect(currentInsn.getOpcode).pop(currentInsn)
                    ),
                    None
                  ))

                  if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame)
                case ARETURN | DRETURN | FRETURN | IRETURN | LRETURN =>
                  val n = new InsnNode(current.getOpcode)
                  finalInsnList.add(n)
                  methodReturns.append(currentFrame.stack.last)
                  terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last), None))
                case RETURN =>
                  val n = new InsnNode(current.getOpcode)
                  finalInsnList.add(n)
                  terminalInsns.append(Terminal(n, Seq(), None))
                case _ =>
                  val (nextInsns, nextFrame) = constantFold(currentInsn, currentFrame)
                  nextInsns.foreach(finalInsnList.add)
                  if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame)
              }

            case current: InvokeDynamicInsnNode => ???

            case current: JumpInsnNode =>
              walkJump(
                walkBlock(_, _, seenBlocks), finalInsnList, walkNextBlock,
                currentFrame, terminalInsns, current
              )
            case current: LabelNode =>
              val newLabel = new LabelNode()
              val nextFrame = currentFrame.execute(newLabel, dataflow)
              labelMapping(current) = newLabel :: labelMapping.getOrElse(current, Nil)
              finalInsnList.add(newLabel)
              if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame)

            case current: LineNumberNode =>
              if (!walkNextLabel(currentFrame)) walkInsn(current.getNext, currentFrame)
              // TODO: handle line numbers properly

            case current: LookupSwitchInsnNode =>
              val n = new LookupSwitchInsnNode(null, Array(), Array())
              terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last), None))
              finalInsnList.add(n)
              val nextFrame = currentFrame.execute(n, dataflow)
              n.dflt = walkBlock(current.dflt, nextFrame, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]
              n.keys = current.keys
              n.labels = current.labels.asScala.map(walkBlock(_, nextFrame, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]).asJava

            case current: MethodInsnNode =>
              val copy = new MethodInsnNode(
                current.getOpcode, current.owner, current.name, current.desc, current.itf
              )

              val argOutCount = Desc.read(current.desc).args.length + (if (current.getOpcode == INVOKESTATIC) 0 else 1)
              val nextFrame1 =
                if (current.owner.startsWith("java/")) {
                  pure = false
                  terminalInsns.append(Terminal(
                    copy,
                    currentFrame.stack.takeRight(argOutCount),
                    Some(Desc.read(current.desc).ret)
                  ))
                  finalInsnList.add(copy)
                  currentFrame.execute(copy, dataflow)
                } else {
                  var methodPure = true
                  val argLivenesses = mutable.Buffer.empty[Seq[Boolean]]
                  val (narrowRet, mangled) = mangleMethodCallInsn(
                    currentFrame, copy,
                    static = copy.getOpcode == INVOKESTATIC,
                    special = copy.getOpcode == INVOKESPECIAL,
                    recurse = (staticSig, types) => {

                      if (seen((staticSig, types))) {
                        // When we hit recursive methods, simply assume that
                        // they are impure and that all their arguments are live.
                        methodPure = false
                        argLivenesses.append(Seq.fill(argOutCount)(true))
                        staticSig.desc.ret
                      }
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

                        val walked = walkMethod(
                          staticSig,
                          lookupMethod(staticSig).get.instructions,
                          types.toList,
                          lookupMethod(staticSig).get.maxLocals,
                          lookupMethod(staticSig).get.maxStack,
                          seen
                        )

                        if (copy.getOpcode == INVOKESTATIC) {
                          argLivenesses.append(walked.liveArgs)
                        }
                        else {
                          argLivenesses.append(
                            if (walked.liveArgs.nonEmpty) walked.liveArgs.updated(0, true)
                            else Seq(true)
                          )
                        }
                        val walkedPure = walked.pure
                        pure &= walkedPure
                        methodPure &= walkedPure

                        walked.inferredReturn
                      }
                    }
                  )


                  if (!methodPure){
                    terminalInsns.append(Terminal(
                      mangled,
                      currentFrame.stack.takeRight(argOutCount),
                      Some(narrowRet)
                    ))
                  }

                  if (methodPure && narrowRet.isConstant){
                    val insns = popN(argOutCount) ++ Seq(Util.constantToInstruction(narrowRet.asInstanceOf[IType.Constant]))
                    insns.foreach(finalInsnList.add)
                    insns.foldLeft(currentFrame)(_.execute(_, dataflow))
                  }else{
                    val finalArgLiveness = argLivenesses.transpose.map(_.reduce(_ || _))
                    subCallArgLiveness(mangled.asInstanceOf[MethodInsnNode]) = finalArgLiveness
                    finalInsnList.add(mangled)

                    currentFrame.execute(mangled, dataflow)
                  }
                }


              if (!walkNextLabel(nextFrame1)) walkInsn(current.getNext, nextFrame1)

            case current: TableSwitchInsnNode =>
              val n = new TableSwitchInsnNode(current.min, current.max, null)
              finalInsnList.add(n)
              val nextFrame = currentFrame.execute(n, dataflow)
              terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last), None))
              n.dflt = walkBlock(current.dflt, nextFrame, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]
              n.labels = current.labels.asScala.map(walkBlock(_, nextFrame, seenBlocks)._2.getFirst.asInstanceOf[LabelNode]).asJava

            case current: TypeInsnNode =>
              visitedClasses.add(JType.Cls(current.desc))
              val (newInsns, nextFrame) = constantFold(currentInsn, currentFrame)
              newInsns.foreach(finalInsnList.add)
              if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame)

            case current: VarInsnNode =>
              val (newInsns, nextFrame) = constantFold(currentInsn, currentFrame)
              newInsns.foreach(finalInsnList.add)
              if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame)
          }
        }
        val typeState = blockState.map(_.tpe)
//        pprint.log("VISITING BLOCK" + Util.prettyprint(blockStart))
        visitedBlocks.get((blockStart, typeState)) match{
          case Some((insns, frame)) =>

//            println("OLD BLOCK " + sig)
            // When we jump back to a previously visited block with the same
            // typestate, aggregate the upstream LValues of the new jump with
            // those already saved earlier
            visitedBlocks((blockStart, typeState)) = (
              insns,
              frame.zipWith(blockState){(x1, x2) => x1.mergeIntoThis(x2)}
            )

            (false, insns)
          case None =>
//          println("NEW BLOCK " + (currentInsn, currentFrame))
            visitedBlocks((blockStart, typeState)) = (finalInsnList, blockState)
            lastBlock = Some((blockStart, blockState))
            walkInsn(blockStart, blockState)
  //          println("END BLOCK")
            (true, finalInsnList)
        }
      }
//      pprint.log(sig -> insns.size)

      val initialArgumentLValues = args
        .zipWithIndex
        .map{case (a, i) => new LValue(a, Left(i), Nil, mutable.Buffer())}

      walkBlock(
        insns.getFirst,
        Frame.initial(
          maxLocals, maxStack,
          initialArgumentLValues,
          new LValue(JType.Null, Left(-1), Nil, mutable.Buffer())
        ),
        Set()
      )

//      pprint.log(sig -> "END")

//      pprint.log((sig, pure, methodReturns))
      val resultType =
        if (methodReturns.isEmpty) sig.desc.ret // abstract methods have no return insn
        else merge(methodReturns.map(_.tpe))

      val (outputInsns, liveArguments) =
        if(false){
          val outputInsns = new InsnList
          visitedBlocks.valuesIterator.foreach(t => outputInsns.add(t._1))
          (outputInsns, (_: Any) => true)
        }else{
          val outputInsns = new InsnList
          visitedBlocks.valuesIterator.foreach(t => outputInsns.add(t._1))
          Liveness(outputInsns, terminalInsns, subCallArgLiveness.toMap)
        }

      val liveArgs =
        if (sig.static) sig.desc.args.indices.map(liveArguments)
        else Seq(true) ++ sig.desc.args.indices.map(_+1).map(liveArguments)

      Walker.MethodResult(
        liveArgs,
        resultType,
        outputInsns,
        pure
      )
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
               currentFrame: Frame[LValue],
               terminalInsns: mutable.Buffer[Terminal],
               current: JumpInsnNode) = {
    def jumpBlock(pred: Boolean): Unit = {
      val popInsns = popN(Bytecode.stackEffect(current.getOpcode).pop(current))

      popInsns.foreach(finalInsnList.add)

      val nextFrame = popInsns.foldLeft(currentFrame)(_.execute(_, dataflow))
      walkNextBlock(if (pred) current.label else current.getNext, nextFrame)
    }

    (
      current.getOpcode,
      currentFrame.stack.lift(currentFrame.stack.length - 1).map(_.tpe),
      currentFrame.stack.lift(currentFrame.stack.length - 2).map(_.tpe)
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
        terminalInsns.append(Terminal(
          n,
          currentFrame.stack.takeRight(Bytecode.stackEffect(current.getOpcode).pop(current)),
          None
        ))
        val nextFrame = currentFrame.execute(n, dataflow)
        walkNextBlock(current.getNext, nextFrame)

        val (fresh, jumped) = walkBlock(current.label, nextFrame)

        n.label = jumped.getFirst.asInstanceOf[LabelNode]
    }
  }
  def popN(n: Int) = {
    for(_ <- 0 until n) yield new InsnNode(POP)
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
                   currentFrame: Frame[LValue]): (Seq[AbstractInsnNode], Frame[LValue]) = {
    val stackEffect = Bytecode.stackEffect(currentInsn.getOpcode)
    val tentativeNextFrame = currentFrame.execute(currentInsn, dataflow)
    val newInsns =
      if (stackEffect.push(currentInsn) == 1){
        tentativeNextFrame.stack.last.tpe match{
          case const: IType.Constant => popN(stackEffect.pop(currentInsn)) ++ Seq(Util.constantToInstruction(const))
          case _ => Seq(Util.clone(currentInsn, mutable.Map.empty))
        }
      }else Seq(Util.clone(currentInsn, mutable.Map.empty))
    (newInsns, newInsns.foldLeft(currentFrame)(_.execute(_, dataflow)))
  }

  def mangleMethodCallInsn(frame: Frame[LValue],
                           insn: AbstractInsnNode,
                           static: Boolean,
                           special: Boolean,
                           recurse: (MethodSig, Seq[IType]) => IType): (IType, AbstractInsnNode) = {
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
        new InsnList,
        false
      )
    }

    val narrowReturnType = merge(
      concreteSigs.map(recurse(_, inferredTypes))
    )

    if (Util.isCompatible(inferredTypes, originalTypes)) (narrowReturnType, insn) // No narrowing
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
      val newOwner =
        if (static) called.owner
        else JType.fromIType(
          frame.stack(frame.stack.length - originalTypes.map(_.getSize).sum).tpe,
          JType.Cls(called.owner)
        ).name

      (narrowReturnType, (isInterface(called.owner), isInterface(newOwner)) match{
        case (false, true) => ??? // cannot widen interface into class!
        case (true, false) => new MethodInsnNode(INVOKEVIRTUAL, newOwner, mangledName, mangledDesc.unparse)
        case _ =>
          called.owner = newOwner
          called.name = mangledName
          called.desc = mangledDesc.unparse
          called
      })
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
                          pure: Boolean)
}