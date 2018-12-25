package joptimize


import org.objectweb.asm.{Handle, Opcodes}

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
             exists: MethodSig => Boolean,
             merge: Seq[IType] => IType,
             dataflow: Dataflow,
             ignore: String => Boolean) {

  def walkMethod(sig: MethodSig,
                 originalInsns: InsnList,
                 args: Seq[IType],
                 maxLocals: Int,
                 maxStack: Int,
                 seenMethods0: Set[(MethodSig, Seq[IType])]): Walker.MethodResult = {

    visitedMethods.getOrElseUpdate((sig, args.drop(if (sig.static) 0 else 1)), {
      val seenMethods = seenMethods0 ++ Seq((sig, args))
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

      val visitedBlocks = new mutable.LinkedHashMap[(AbstractInsnNode, Frame[IType]), Walker.BlockInfo]

      // Equivalent to visitedBlocks.lastOption, but faster because
      // LinkedHashMap#lastOption isn't optimized and so is O(n) instead of O(1)

//      pprint.log(sig -> insns.size)

      val initialArgumentLValues = args
        .zipWithIndex
        .map{case (a, i) => new LValue(a, Left(i), Nil)}

      val merges = mutable.Buffer.empty[(Frame[LValue], Frame[LValue])]
      walkBlock(
        originalInsns.getFirst,
        Frame.initial(
          maxLocals, maxStack,
          initialArgumentLValues,
          new LValue(JType.Null, Left(-1), Nil)
        ),
        Set(), visitedBlocks, sig, seenMethods,
        recurse = (staticSig, types) => {
          val argOutCount = staticSig.desc.args.length + (if (staticSig.static) 0 else 1)
          if (seenMethods((staticSig, types))) {
            // When we hit recursive methods, simply assume that
            // they are impure and that all their arguments are live.

            Walker.MethodResult(
              Seq.fill(argOutCount)(true),
              staticSig.desc.ret,
              new InsnList,
              false
            )
          }
          else {
            val clinitSig = MethodSig(staticSig.cls, "<clinit>", Desc.read("()V"), true)
            if (!seenMethods.contains((clinitSig, Nil))) {
              for(clinit <- lookupMethod(clinitSig)){
                walkMethod(
                  clinitSig,
                  clinit.instructions,
                  Nil,
                  clinit.maxLocals,
                  clinit.maxStack,
                  seenMethods ++ Seq((clinitSig, Nil))
                )
              }
            }

            walkMethod(
              staticSig,
              lookupMethod(staticSig).get.instructions,
              types.toList,
              lookupMethod(staticSig).get.maxLocals,
              lookupMethod(staticSig).get.maxStack,
              seenMethods ++ Seq(staticSig -> types)
            )
          }
        },
        merges
      )

//      pprint.log(sig -> "END")

//      pprint.log((sig, pure, methodReturns))
      val allVisitedBlocks = visitedBlocks.values.map(_.asInstanceOf[Walker.BlockResult])
      val methodReturns = allVisitedBlocks.flatMap(_.methodReturns).toSeq
      val terminalInsns = allVisitedBlocks.flatMap(_.terminalInsns).toSeq
      val lineNumberNodes = allVisitedBlocks.flatMap(_.lineNumberNodes)
      val methodInsnMapping = allVisitedBlocks.flatMap(_.methodInsnMapping).toMap
      val subCallArgLiveness = allVisitedBlocks.flatMap(_.subCallArgLiveness).toMap

      val pure = allVisitedBlocks.forall(_.pure)
      val resultType =
        if (methodReturns.isEmpty) sig.desc.ret // abstract methods have no return insn
        else merge(methodReturns.map(_.tpe))

      val outputInsns = new InsnList
      allVisitedBlocks.foreach(t => outputInsns.add(t.insns))

      val liveArguments =
        if(false) (_: Any) => true
        else Liveness(outputInsns, terminalInsns, subCallArgLiveness.toMap, merges)

      val liveArgs =
        if (sig.static) sig.desc.args.indices.map(liveArguments)
        else Seq(true) ++ sig.desc.args.indices.map(_+1).map(liveArguments)

      for{
        lineNumNode <- lineNumberNodes
        label <- methodInsnMapping(lineNumNode.start)
      } outputInsns.add(new LineNumberNode(lineNumNode.line, label.asInstanceOf[LabelNode]))

      Walker.MethodResult(
        liveArgs,
        resultType,
        outputInsns,
        pure
      )
    })
  }

  def walkBlock(blockStart: AbstractInsnNode,
                blockState0: Frame[LValue],
                seenBlocks0: Set[AbstractInsnNode],
                visitedBlocks: mutable.LinkedHashMap[(AbstractInsnNode, Frame[IType]), Walker.BlockInfo],
                sig: MethodSig,
                seenMethods: Set[(MethodSig, scala.Seq[IType])],
                recurse: (MethodSig, Seq[IType]) => Walker.MethodResult,
                merges: mutable.Buffer[(Frame[LValue], Frame[LValue])]): Walker.BlockInfo = {

    val blockState =
      if (!seenBlocks0.contains(blockStart)) blockState0
      else blockState0.widen
    val seenBlocks = seenBlocks0 + blockStart

    val typeState = blockState.map(_.tpe)
    //        pprint.log("VISITING BLOCK" + Util.prettyprint(blockStart))
    visitedBlocks.get((blockStart, typeState)) match{
      case Some(res @ Walker.BlockStub(blockIndex, insns, frame)) =>
        merges.append((frame, blockState))
        res

      case Some(res: Walker.BlockResult) =>
        merges.append((res.startFrame, blockState))
        res

      case None =>
        val blockIndex = visitedBlocks.size
        //          println("NEW BLOCK " + (currentInsn, currentFrame))

        val insnList = new InsnList
        def walkBlock1 = walkBlock(_, _, seenBlocks, visitedBlocks, sig, seenMethods, recurse, merges)
        /**
          * Walk another block. Automatically inserts a GOTO if the next block isn't
          * going to be immediately after the current block, and elides the GOTO otherwise
          *
          * Shouldn't be used if there isn't any possibility of the next block
          * seamlessly following the current, e.g. in the case of switches where
          * every branch needs a jump.
          */
        def walkNextBlock(destBlockStart: AbstractInsnNode,
                          destBlockState: Frame[LValue]) = {

          val blockRes = walkBlock1(destBlockStart, destBlockState)

          if (blockRes.blockIndex != blockIndex + 1){
            insnList.add(new JumpInsnNode(GOTO, blockRes.leadingLabel))
          }
          blockRes
        }

        val blockInsnMapping = mutable.Map.empty[AbstractInsnNode, AbstractInsnNode]
        val methodReturns = mutable.Buffer.empty[LValue]
        val terminalInsns = mutable.Buffer.empty[Terminal]
        val pure = sig.name != "<init>" && sig.name != "<clinit>"
        val subCallArgLiveness = mutable.Map.empty[AbstractInsnNode, Seq[Boolean]]
        val lineNumberNodes = mutable.Set.empty[LineNumberNode]

        val ctx = Walker.InsnCtx(
          sig,
          finalInsnList = insnList,
          blockInsnMapping = blockInsnMapping,
          methodReturns = methodReturns,
          terminalInsns = terminalInsns,
          pure = pure,
          subCallArgLiveness = subCallArgLiveness,
          lineNumberNodes = lineNumberNodes,
          walkBlock = walkBlock1,
          walkNextBlock = walkNextBlock,
          seenMethods = seenMethods,
          walkMethod = recurse
        )

        visitedBlocks((blockStart, typeState)) = Walker.BlockStub(
          blockIndex,
          () => insnList.getFirst.asInstanceOf[LabelNode],
          blockState
        )

        walkInsn(blockStart, blockState, ctx)

        val methodInsnMapping = mutable.Map.empty[AbstractInsnNode, List[AbstractInsnNode]]
        for((k, v) <- ctx.blockInsnMapping){
          methodInsnMapping(k) = v :: methodInsnMapping.getOrElse(k, Nil)
        }
        val res = Walker.BlockResult(
          blockIndex,
          insnList,
          blockState,
          methodReturns,
          terminalInsns,
          ctx.pure,
          subCallArgLiveness.toMap,
          lineNumberNodes.toSet,
          methodInsnMapping.toMap
        )
        visitedBlocks((blockStart, typeState)) = res
        //          println("END BLOCK")
        res
    }
  }
  /**
    * Walks a single basic block, returning:
    *
    * - Instruction list of that basic block
    */
  @tailrec final def walkInsn(currentInsn: AbstractInsnNode,
                              currentFrame: Frame[LValue],
                              ctx: Walker.InsnCtx): Unit = {
    //          println("    " + currentFrame)
    //          val nextFrame = currentFrame.execute(currentInsn, dataflow)

    /**
      * Walk the next instruction as a new block, if it is a label. If not
      * then return `false` so we can tail-recursively walk it as a simple
      * instruction
      */
    def walkNextLabel(nextFrame1: Frame[LValue]) = {
      if (currentInsn.getNext.isInstanceOf[LabelNode]) {
        ctx.walkNextBlock(currentInsn.getNext, nextFrame1)
        true
      } else false
    }

    currentInsn match{
      case current: FieldInsnNode =>
        val clinitSig = MethodSig(current.owner, "<clinit>", Desc.read("()V"), true)

        val n = new FieldInsnNode(current.getOpcode, current.owner, current.name, current.desc)
        if (!ctx.seenMethods.contains((clinitSig, Nil))) {
          for(clinit <- lookupMethod(clinitSig)){
            walkMethod(
              clinitSig,
              clinit.instructions,
              Nil,
              clinit.maxLocals,
              clinit.maxStack,
              ctx.seenMethods ++ Seq((clinitSig, Nil))
            )
          }
        }
        ctx.terminalInsns.append(Terminal(
          n,
          currentFrame.stack.takeRight(
            Bytecode.stackEffect(currentInsn.getOpcode).pop(currentInsn)
          ),
          None
        ))

        ctx.finalInsnList.add(n)
        val nextFrame = currentFrame.execute(n, dataflow)
        if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)

      case current: FrameNode =>
        // We discard frame nodes; we're going to be doing a bunch of mangling
        // so they'll all be wrong, so easier just let ASM recompute them
        if (!walkNextLabel(currentFrame)) walkInsn(current.getNext, currentFrame, ctx)

      case _: LabelNode | _: IincInsnNode | _: IntInsnNode | _: LdcInsnNode | _: MultiANewArrayInsnNode =>
        val n = Util.clone(currentInsn, ctx.blockInsnMapping)
        ctx.finalInsnList.add(n)
        val nextFrame = currentFrame.execute(n, dataflow)
        if (!walkNextLabel(nextFrame)) walkInsn(currentInsn.getNext, nextFrame, ctx)

      case current: InsnNode =>
        current.getOpcode match{
          case IASTORE | LASTORE | FASTORE | DASTORE | AASTORE | BASTORE | CASTORE | SASTORE |
               IALOAD | LALOAD | FALOAD | DALOAD | AALOAD | BALOAD | CALOAD | SALOAD =>
            val (newInsns, nextFrame) = constantFold(currentInsn, currentFrame, ctx.blockInsnMapping)

            newInsns.foreach(ctx.finalInsnList.add)
            ctx.terminalInsns.append(Terminal(
              newInsns.last,
              currentFrame.stack.takeRight(
                Bytecode.stackEffect(currentInsn.getOpcode).pop(currentInsn)
              ),
              None
            ))

            if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
          case ARETURN | DRETURN | FRETURN | IRETURN | LRETURN =>
            val n = new InsnNode(current.getOpcode)
            ctx.finalInsnList.add(n)
            ctx.methodReturns.append(currentFrame.stack.last)
            ctx.terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last), None))
          case RETURN =>
            val n = new InsnNode(current.getOpcode)
            ctx.finalInsnList.add(n)
            ctx.terminalInsns.append(Terminal(n, Seq(), None))
          case _ =>
            val (nextInsns, nextFrame) = constantFold(currentInsn, currentFrame, ctx.blockInsnMapping)
            nextInsns.foreach(ctx.finalInsnList.add)
            if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
        }

      case current: InvokeDynamicInsnNode =>

        if (current.bsm == Util.metafactory || current.bsm == Util.altMetafactory){

          val target = current.bsmArgs(1).asInstanceOf[Handle]
          val targetSig = MethodSig(
            JType.Cls(target.getOwner),
            target.getName,
            Desc.read(target.getDesc),
            target.getTag == Opcodes.H_INVOKESTATIC
          )

          ctx.walkMethod(targetSig, targetSig.desc.args)

          val n = Util.clone(currentInsn, ctx.blockInsnMapping)
          ctx.finalInsnList.add(n)
          val nextFrame = currentFrame.execute(n, dataflow)
          if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
        }else if(current.bsm == Util.makeConcatWithConstants){
          val n = Util.clone(currentInsn, ctx.blockInsnMapping)
          ctx.finalInsnList.add(n)
          val nextFrame = currentFrame.execute(n, dataflow)
          if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
        } else{
          pprint.log(current.bsm)
          pprint.log(current.bsmArgs)
          pprint.log(current.bsmArgs.map(_.getClass))
          pprint.log(current.desc)
          pprint.log(current.name)
          ???
        }

      case current: JumpInsnNode =>
        walkJump(
          ctx.walkBlock, ctx.finalInsnList, ctx.walkNextBlock,
          currentFrame, ctx.terminalInsns, current
        )

      case current: LineNumberNode =>
        // We do not bother copying line number nodes in-place. Instead, we
        // just batch them all up and then append them to the end of the insn
        // list once we're done processing a method, since the position of the
        // line number node in the insn list doesn't matter (only the label
        // pointer does)
        ctx.lineNumberNodes.add(current)
        if (!walkNextLabel(currentFrame)) walkInsn(current.getNext, currentFrame, ctx)


      case current: LookupSwitchInsnNode =>
        val n = new LookupSwitchInsnNode(null, Array(), Array())
        ctx.terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last), None))
        ctx.finalInsnList.add(n)
        val nextFrame = currentFrame.execute(n, dataflow)
        n.dflt = ctx.walkBlock(current.dflt, nextFrame).leadingLabel
        n.keys = current.keys
        n.labels = current.labels.asScala.map(ctx.walkBlock(_, nextFrame).leadingLabel).asJava

      case current: MethodInsnNode =>
        val nextFrame1 = walkMethodInsn(currentFrame, ctx, walkNextLabel, current)
        if (!walkNextLabel(nextFrame1)) walkInsn(current.getNext, nextFrame1, ctx)

      case current: TableSwitchInsnNode =>
        val n = new TableSwitchInsnNode(current.min, current.max, null)
        ctx.finalInsnList.add(n)
        val nextFrame = currentFrame.execute(n, dataflow)
        ctx.terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last), None))
        n.dflt = ctx.walkBlock(current.dflt, nextFrame)
          .leadingLabel
        n.labels = current.labels
          .asScala
          .map(
            ctx.walkBlock(_, nextFrame)
              .leadingLabel
          )
          .asJava

      case current: TypeInsnNode =>
        if (!ignore(current.desc)) {
          visitedClasses.add(JType.Cls(current.desc))
        }
        val (newInsns, nextFrame) = constantFold(currentInsn, currentFrame, ctx.blockInsnMapping)
        newInsns.foreach(ctx.finalInsnList.add)
        if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)

      case current: VarInsnNode =>
        val (newInsns, nextFrame) = constantFold(currentInsn, currentFrame, ctx.blockInsnMapping)
        newInsns.foreach(ctx.finalInsnList.add)
        if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
    }
  }

  def walkMethodInsn(currentFrame: Frame[LValue],
                     ctx: Walker.InsnCtx,
                     walkNextLabel: Frame[LValue] => Boolean,
                     originalInsn: MethodInsnNode) = {

    val argOutCount = Desc.read(originalInsn.desc).args.length + (if (originalInsn.getOpcode == INVOKESTATIC) 0 else 1)
    if (ignore(originalInsn.owner)) {
      ctx.pure = false
      val copy = new MethodInsnNode(
        originalInsn.getOpcode, originalInsn.owner, originalInsn.name, originalInsn.desc, originalInsn.itf
      )
      ctx.terminalInsns.append(Terminal(
        copy,
        currentFrame.stack.takeRight(argOutCount),
        Some(Desc.read(originalInsn.desc).ret)
      ))
      ctx.finalInsnList.add(copy)
      currentFrame.execute(copy, dataflow)
    } else {

      val (argLivenesses, methodPure, narrowRet, mangled) = {
        val static = originalInsn.getOpcode == INVOKESTATIC
        val special = originalInsn.getOpcode == INVOKESPECIAL

        val calledDesc = Desc.read(originalInsn.desc)
        val calledSelf = if (static) Nil else Seq(JType.Cls(originalInsn.owner))
        val originalTypes = calledSelf ++ calledDesc.args.toSeq

        val inferredTypes =
          (currentFrame.stack.length - originalTypes.map(_.getSize).sum)
            .until(currentFrame.stack.length)
            .map(currentFrame.stack(_).tpe)

        val sig = MethodSig(originalInsn.owner, originalInsn.name, Desc.read(originalInsn.desc), static)

        val (concreteSigs, abstractSigs) =
          if (special) (Seq(sig), Nil)
          else {
            val subtypes = findSubtypes(sig.cls)
            val possibleSigs = subtypes.map(st => sig.copy(cls = st)) ++ Seq(sig)
            possibleSigs.filter(exists).partition(isConcrete)
          }

        for (interfaceSig <- abstractSigs) {
          visitedMethods((interfaceSig, originalTypes.drop(1))) = Walker.MethodResult(
            Array.fill(originalTypes.length - 1)(true),
            interfaceSig.desc.ret,
            new InsnList,
            false
          )
        }

        val recursedResults = concreteSigs.map(ctx.walkMethod(_, inferredTypes))
        val methodPure = recursedResults.forall(_.pure)
        val argLivenesses = recursedResults.map(_.liveArgs)
        val narrowReturnType = merge(recursedResults.map(_.inferredReturn))

        if (Util.isCompatible(inferredTypes, originalTypes)) (argLivenesses, methodPure, narrowReturnType, originalInsn) // No narrowing
        else {
          val descChanged =
            (static && inferredTypes != originalTypes) ||
              (!static && inferredTypes.drop(1) != originalTypes.drop(1)) // ignore self type

          val (mangledName, mangledDesc) =
            if (!descChanged) (originalInsn.name, Desc.read(originalInsn.desc))
            else {
              Util.mangle(
                originalInsn.name,
                if (static) inferredTypes else inferredTypes.drop(1),
                if (static) originalTypes else originalTypes.drop(1),
                narrowReturnType,
                calledDesc.ret
              )
            }

          // We do not bother narrowing the owner of the bytecode or
          // swapping from INVOKEDYNAMIC to INVOKEINTERFACE for now
          val returnNode = new MethodInsnNode(
            originalInsn.getOpcode,
            originalInsn.owner,
            mangledName,
            mangledDesc.unparse
          )
          (argLivenesses, methodPure, narrowReturnType, returnNode)
        }
      }

      if (!methodPure) {
        ctx.terminalInsns.append(Terminal(
          mangled,
          currentFrame.stack.takeRight(argOutCount),
          Some(narrowRet)
        ))
      }

      if (methodPure && narrowRet.isConstant) {
        val insns = popN(argOutCount) ++ Seq(Util.constantToInstruction(narrowRet.asInstanceOf[IType.Constant]))
        insns.foreach(ctx.finalInsnList.add)
        insns.foldLeft(currentFrame)(_.execute(_, dataflow))
      } else {
        val finalArgLiveness = argLivenesses.transpose.map(_.reduce(_ || _))
        ctx.subCallArgLiveness(mangled.asInstanceOf[MethodInsnNode]) = finalArgLiveness
        ctx.finalInsnList.add(mangled)

        currentFrame.execute(mangled, dataflow)
      }
    }


  }

  /**
    * Removes a jump if we already statically know the destination, replacing
    * it with POPs and an optional GOTO directly to the end destination.
    *
    * If we do not know the destination, we simply emit the jump in the final
    * bytecode, and walk the two jump targets to make sure we have somewhere
    * to jump to
    */
  def walkJump(walkBlock: (AbstractInsnNode, Frame[LValue]) => Walker.BlockInfo,
               finalInsnList: InsnList,
               walkNextBlock: (AbstractInsnNode, Frame[LValue]) => Walker.BlockInfo,
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

        val res = walkBlock(current.label, nextFrame)

        n.label = res.leadingLabel
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
                   currentFrame: Frame[LValue],
                   blockInsnMapping: mutable.Map[AbstractInsnNode, AbstractInsnNode]): (Seq[AbstractInsnNode], Frame[LValue]) = {
    val stackEffect = Bytecode.stackEffect(currentInsn.getOpcode)
    val tentativeNextFrame = currentFrame.execute(currentInsn, dataflow)
    val newInsns =
      if (stackEffect.push(currentInsn) == 1){
        tentativeNextFrame.stack.last.tpe match{
          case const: IType.Constant => popN(stackEffect.pop(currentInsn)) ++ Seq(Util.constantToInstruction(const))
          case _ => Seq(Util.clone(currentInsn, blockInsnMapping))
        }
      }else Seq(Util.clone(currentInsn, blockInsnMapping))
    (newInsns, newInsns.foldLeft(currentFrame)(_.execute(_, dataflow)))
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

  trait BlockInfo{
    def blockIndex: Int
    def leadingLabel: LabelNode
    def startFrame: Frame[LValue]
  }

  case class BlockStub(blockIndex: Int,
                       leadingLabel0: () => LabelNode,
                       startFrame: Frame[LValue]) extends BlockInfo{
    def leadingLabel = leadingLabel0()
  }

  case class BlockResult(blockIndex: Int,
                         insns: InsnList,
                         startFrame: Frame[LValue],
                         methodReturns: Seq[LValue],
                         terminalInsns: Seq[Terminal],
                         pure: Boolean,
                         subCallArgLiveness: Map[AbstractInsnNode, Seq[Boolean]],
                         lineNumberNodes: Set[LineNumberNode],
                         methodInsnMapping: Map[AbstractInsnNode, List[AbstractInsnNode]]) extends BlockInfo{
    def leadingLabel = insns.getFirst.asInstanceOf[LabelNode]
  }

  case class InsnCtx(sig: MethodSig,
                     finalInsnList: InsnList,
                     blockInsnMapping: mutable.Map[AbstractInsnNode, AbstractInsnNode],
                     methodReturns: mutable.Buffer[LValue],
                     terminalInsns: mutable.Buffer[Terminal],
                     var pure: Boolean,
                     subCallArgLiveness: mutable.Map[AbstractInsnNode, Seq[Boolean]],
                     lineNumberNodes: mutable.Set[LineNumberNode],
                     walkBlock: (AbstractInsnNode, Frame[LValue]) => BlockInfo,
                     walkNextBlock: (AbstractInsnNode, Frame[LValue]) => BlockInfo,
                     seenMethods: Set[(MethodSig, Seq[IType])],
                     walkMethod: (MethodSig, Seq[IType]) => Walker.MethodResult)
}