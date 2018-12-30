package joptimize


import java.util.IdentityHashMap

import org.objectweb.asm.{Handle, Opcodes}

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Analyzer, Interpreter}

import scala.annotation.tailrec
import scala.collection.mutable

class Walker(isInterface: JType.Cls => Boolean,
             lookupMethod: MethodSig => Option[MethodNode],
             visitedMethods: mutable.Map[(MethodSig, Seq[IType]), Walker.MethodResult],
             visitedClasses: mutable.Set[JType.Cls],
             findSubtypes: JType.Cls => List[JType.Cls],
             findSupertypes: JType.Cls => Seq[JType.Cls],
             isConcrete: MethodSig => Boolean,
             exists: MethodSig => Boolean,
             merge: Seq[IType] => IType,
             typer: Typer,
             ignore: String => Boolean) {

  def walkMethod(sig: MethodSig,
                 mn: MethodNode,
                 args: Seq[IType],
                 seenMethods0: Set[(MethodSig, Seq[IType])]): Walker.MethodResult = {

    visitedMethods.getOrElseUpdate((sig, args.drop(if (sig.static) 0 else 1)), {
      // - Build up non-terminal SSA graph using Analyzer (including Phis)
      // - Place terminal SSA nodes manually since analyzer can't see them
      // - Construct lookaside table for control dependencies, state dependencies (same thing???)
      val seenMethods = seenMethods0 ++ Seq((sig, args.map(_.widen)))

      println("+" * 20 + sig + "+" * 20)
      println(Renderer.render(mn.instructions))


      val merges = mutable.Buffer.empty[(Frame[SSA], Frame[SSA])]

      val analyzer = new Analyzer(StepEvaluator)
      val frames = analyzer.analyze(sig.cls.name, mn)

      val insns = mn.instructions.iterator().asScala.toVector

      def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)
      val terminals = insns.map(i => (i.getOpcode, i)).zipWithIndex.collect{
        case ((RETURN, insn), i) => (insn, SSA.Return(), i)

        case ((IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn), i) =>
          (insn, SSA.ReturnVal(frameTop(i, 0)), i)

        case ((ATHROW, insn), i) => (insn, SSA.AThrow(frameTop(i, 0)), i)

        case ((GOTO, insn), i) => (insn, SSA.Goto(), i)

        case ((IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE, insn), i) =>
          (insn, SSA.UnaryBranch(frameTop(i, 0), SSA.UnaryBranch.lookup(insn.getOpcode)), i)

        case ((IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE, insn), i) =>
          (insn, SSA.BinBranch(frameTop(i, 0), frameTop(i, 1), SSA.BinBranch.lookup(insn.getOpcode)), i)
      }

      val blockStarts = insns.take(0) ++ insns
        .collect{
          case n: TableSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
          case n: LookupSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
          case n: JumpInsnNode => Seq(n.label) ++ Option(n.getNext)
        }
        .flatten

      pprint.log(terminals)
      pprint.log(blockStarts)

      val controlDependencies = new IdentityHashMap[SSA, SSA]()

      pprint.log(frames.zipWithIndex.map(_.swap))

      ???
//      Walker.MethodResult(
//        liveArgs,
//        resultType,
//        outputInsns,
//        pure,
//        outputTcbs
//      )
    })
  }
//
//  def walkBlock(blockStart: AbstractInsnNode,
//                blockStartFrame: Frame[SSA],
//                seenBlocks0: Set[AbstractInsnNode],
//                visitedBlocks: mutable.LinkedHashMap[(AbstractInsnNode, Frame[IType]), Walker.BlockInfo],
//                sig: MethodSig,
//                seenMethods: Set[(MethodSig, scala.Seq[IType])],
//                recurse: (MethodSig, Seq[IType]) => Walker.MethodResult,
//                merges: mutable.Buffer[(Frame[SSA], Frame[SSA])],
//                insnTryCatchBlocks: Map[AbstractInsnNode, Map[Int, (JType.Cls, LabelNode)]],
//                ssaInterpreter: StepEvaluator,
//                jumpedBasicBlocks: (AbstractInsnNode, Frame[IType]) => Block,
//                renderer: AbstractInsnNode => fansi.Str): Walker.BlockInfo = {
//
//    val typeState =
//      if (!seenBlocks0.contains(blockStart)) blockStartFrame.map(ssaInterpreter.inferredTypes.get)
//      else blockStartFrame.map(ssaInterpreter.inferredTypes.get(_).widen)
//    val seenBlocks = seenBlocks0 + blockStart
//
//    //        pprint.log("VISITING BLOCK" + Util.prettyprint(blockStart))
//    visitedBlocks.get((blockStart, typeState)) match{
//      case Some(res: Walker.BlockStub) =>
//        merges.append((res.startFrame, blockStartFrame))
//        res
//
//      case Some(res: Walker.BlockResult) =>
//        merges.append((res.startFrame, blockStartFrame))
//        res
//
//      case None =>
//        val blockStartStr = renderer(blockStart)
//        val padding = " " * (30 - blockStartStr.length)
//        val blockIndex = visitedBlocks.size
//        println("  " * seenBlocks0.size + "NEW BLOCK" + blockIndex)
//
//        def walkBlock1 = walkBlock(
//          _, _,
//          seenBlocks, visitedBlocks, sig, seenMethods, recurse, merges,
//          insnTryCatchBlocks, ssaInterpreter, jumpedBasicBlocks, renderer
//        )
//        val blockInsns = jumpedBasicBlocks(blockStart, typeState)
//
//        val terminalInsns = mutable.Buffer.empty[SSA]
//        val pure = sig.name != "<init>" && sig.name != "<clinit>"
//        val subCallArgLiveness = mutable.Map.empty[AbstractInsnNode, Seq[Boolean]]
//        val lineNumberNodes = mutable.Set.empty[LineNumberNode]
//
//        val ctx = Walker.InsnCtx(
//          sig,
//          terminalInsns = terminalInsns,
//          pure = pure,
//          subCallArgLiveness = subCallArgLiveness,
//          lineNumberNodes = lineNumberNodes,
//          walkBlock = walkBlock1,
//          seenMethods = seenMethods,
//          walkMethod = recurse,
//          ssaInterpreter = ssaInterpreter,
//          basicBlock = blockInsns,
//          jumpedBasicBlocks = jumpedBasicBlocks,
//          renderer = i => fansi.Str("  " * seenBlocks0.size) ++ renderer(i)
//        )
//
//        visitedBlocks((blockStart, typeState)) = Walker.BlockStub(blockStartFrame)
//
//        val allHandlers = mutable.Buffer.empty[(LabelNode, JType.Cls)]
//
//        walkInsn(blockStart, blockStartFrame, ctx)
//
//        for ((_, (tpe, handler))<- insnTryCatchBlocks(blockStart)) {
//          val ssa = SSA.Arg(-1, tpe)
//          ssaInterpreter.inferredTypes.put(ssa, tpe)
//          val handlerFrame = blockStartFrame.handleException(ssa)
//          val dest = walkBlock1(handler, handlerFrame)
//          allHandlers.append((handler, tpe))
//        }
//
//        val res = Walker.BlockResult(
//          blockStartFrame,
//          terminalInsns,
//          ctx.pure,
//          subCallArgLiveness.toMap,
//          lineNumberNodes.toSet,
//          allHandlers,
//          blockInsns
//        )
//        visitedBlocks((blockStart, typeState)) = res
//
//        println("  " * seenBlocks0.size + "END BLOCK" + blockIndex)
//        res
//    }
//  }
//  /**
//    * Walks a single basic block, returning:
//    *
//    * - Instruction list of that basic block
//    */
//  @tailrec final def walkInsn(currentInsn: AbstractInsnNode,
//                              currentFrame: Frame[SSA],
//                              ctx: Walker.InsnCtx): Unit = {
//    /**
//      * Walk the next instruction as a new block, if it is a label. If not
//      * then return `false` so we can tail-recursively walk it as a simple
//      * instruction
//      */
//    def walkNextLabel(nextFrame1: Frame[SSA]) = {
//      if (currentInsn.getNext.isInstanceOf[LabelNode]) {
//        ctx.basicBlock.fallThrough = Some(
//          ctx.jumpedBasicBlocks(
//            currentInsn.getNext,
//            nextFrame1.map(ctx.ssaInterpreter.inferredTypes.get)
//          )
//        )
//        ctx.walkBlock(currentInsn.getNext, nextFrame1)
//        true
//      } else false
//    }
//
//    val rendered = ctx.renderer(currentInsn)
//    println(rendered + (" " * (50 - rendered.length)) + currentFrame.map(ctx.ssaInterpreter.inferredTypes.get(_)).render)
//
//    currentInsn match{
//      case current: FieldInsnNode =>
//        val clinitSig = MethodSig(current.owner, "<clinit>", Desc.read("()V"), true)
//
//        if (!ctx.seenMethods.contains((clinitSig, Nil))) {
//          for(clinit <- lookupMethod(clinitSig)){
//            walkMethod(
//              clinitSig,
//              clinit.instructions,
//              clinit.tryCatchBlocks.asScala,
//              Nil,
//              clinit.maxLocals,
//              clinit.maxStack,
//              ctx.seenMethods ++ Seq((clinitSig, Nil))
//            )
//          }
//        }
//        if (!ignore(current.owner)) {
//          visitedClasses.add(JType.Cls(current.owner))
//        }
//
//        val nextFrame = currentFrame.execute(current, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//
//        if (current.getOpcode == PUTFIELD || current.getOpcode == PUTSTATIC){
//          ctx.terminalInsns.append(ctx.basicBlock.value.last)
//          ctx.pure = false
//        }
//        if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
//
//      case current: FrameNode =>
//        // We discard frame nodes; we're going to be doing a bunch of mangling
//        // so they'll all be wrong, so easier just let ASM recompute them
//        if (!walkNextLabel(currentFrame)) walkInsn(current.getNext, currentFrame, ctx)
//
//      case _: LabelNode | _: IincInsnNode | _: IntInsnNode | _: LdcInsnNode |
//           _: MultiANewArrayInsnNode | _: VarInsnNode =>
//        val nextFrame = currentFrame.execute(currentInsn, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//        if (!walkNextLabel(nextFrame)) walkInsn(currentInsn.getNext, nextFrame, ctx)
//
//      case current: InsnNode =>
//        current.getOpcode match{
//          case ARETURN | DRETURN | FRETURN | IRETURN | LRETURN =>
//            ctx.basicBlock.value.append(SSA.ReturnVal(currentFrame.stack.last))
//            ctx.terminalInsns.append(ctx.basicBlock.value.last)
//
//          case RETURN =>
//            ctx.basicBlock.value.append(SSA.Return())
//            ctx.terminalInsns.append(ctx.basicBlock.value.last)
//
//          case ATHROW =>
//            ctx.basicBlock.value.append(SSA.AThrow(currentFrame.stack.last))
//            ctx.terminalInsns.append(ctx.basicBlock.value.last)
//
//          case _ =>
//            val nextFrame = currentFrame.execute(current, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//            if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
//        }
//
//      case current: InvokeDynamicInsnNode =>
//
//        if (current.bsm == Util.metafactory || current.bsm == Util.altMetafactory){
//
//          val target = current.bsmArgs(1).asInstanceOf[Handle]
//          val targetSig = MethodSig(
//            JType.Cls(target.getOwner),
//            target.getName,
//            Desc.read(target.getDesc),
//            target.getTag == Opcodes.H_INVOKESTATIC
//          )
//
//          ctx.walkMethod(targetSig, targetSig.desc.args)
//
//          val nextFrame = currentFrame.execute(currentInsn, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//          if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
//        }else if(current.bsm == Util.makeConcatWithConstants){
//
//          val nextFrame = currentFrame.execute(currentInsn, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//          if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
//        } else{
//          pprint.log(current.bsm)
//          pprint.log(current.bsmArgs)
//          pprint.log(current.bsmArgs.map(_.getClass))
//          pprint.log(current.desc)
//          pprint.log(current.name)
//          ???
//        }
//
//      case current: JumpInsnNode =>
//        walkJump(
//          ctx.walkBlock, currentFrame, ctx.terminalInsns, current,
//          ctx.ssaInterpreter, ctx.basicBlock
//        )
//
//      case current: LineNumberNode =>
//        // We do not bother copying line number nodes in-place. Instead, we
//        // just batch them all up and then append them to the end of the insn
//        // list once we're done processing a method, since the position of the
//        // line number node in the insn list doesn't matter (only the label
//        // pointer does)
//        ctx.lineNumberNodes.add(current)
//        if (!walkNextLabel(currentFrame)) walkInsn(current.getNext, currentFrame, ctx)
//
//
//      case current: LookupSwitchInsnNode =>
//        val nextFrame = currentFrame.execute(current, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//        ctx.terminalInsns.append(ctx.basicBlock.value.last)
//        ctx.walkBlock(current.dflt, nextFrame)
//        current.labels.asScala.foreach(ctx.walkBlock(_, nextFrame))
//        (None, Some(current))
//
//      case current: MethodInsnNode =>
//        val nextFrame = walkMethodInsn(currentFrame, ctx, current)
//        if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
//
//      case current: TableSwitchInsnNode =>
//        val nextFrame = currentFrame.execute(current, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//        ctx.terminalInsns.append(ctx.basicBlock.value.last)
//        ctx.walkBlock(current.dflt, nextFrame)
//        current.labels.asScala.foreach(ctx.walkBlock(_, nextFrame))
//
//      case current: TypeInsnNode =>
//        if (!ignore(current.desc)) visitedClasses.add(JType.Cls(current.desc))
//        val nextFrame = currentFrame.execute(current, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//        if (!walkNextLabel(nextFrame)) walkInsn(current.getNext, nextFrame, ctx)
//    }
//  }
//
//  def walkMethodInsn(currentFrame: Frame[SSA],
//                     ctx: Walker.InsnCtx,
//                     originalInsn: MethodInsnNode) = {
//
//    val argOutCount = Desc.read(originalInsn.desc).args.length + (if (originalInsn.getOpcode == INVOKESTATIC) 0 else 1)
//    if (ignore(originalInsn.owner)) {
//      ctx.pure = false
//      val nextFrame = currentFrame.execute(originalInsn, ctx.ssaInterpreter(ctx.basicBlock, currentFrame))
//      ctx.terminalInsns.append(ctx.basicBlock.value.last)
//      nextFrame
//    } else {
//
//      val (argLivenesses, methodPure, narrowRet, mangled) = {
//        val static = originalInsn.getOpcode == INVOKESTATIC
//        val special = originalInsn.getOpcode == INVOKESPECIAL
//
//        val calledDesc = Desc.read(originalInsn.desc)
//        val calledSelf = if (static) Nil else Seq(JType.Cls(originalInsn.owner))
//        val originalTypes = calledSelf ++ calledDesc.args.toSeq
//
//        val inferredArgumentTypes =
//          (currentFrame.stack.length - originalTypes.map(_.getSize).sum)
//            .until(currentFrame.stack.length)
//            .map(x => ctx.ssaInterpreter.inferredTypes.get(currentFrame.stack(x)))
//
//        val sig = MethodSig(originalInsn.owner, originalInsn.name, Desc.read(originalInsn.desc), static)
//
//        val (concreteSigs, abstractSigs) =
//          if (special) (Seq(sig), Nil)
//          else if (static) (
//            findSupertypes(sig.cls)
//              .iterator
//              .map(MethodSig(_, originalInsn.name, calledDesc, true))
//              .filter(exists)
//              .take(1)
//              .toSeq,
//            Nil
//          )
//          else {
//            val subtypes = findSubtypes(sig.cls)
//            val possibleSigs = subtypes.map(st => sig.copy(cls = st)) ++ Seq(sig)
//            possibleSigs.filter(exists).partition(isConcrete)
//          }
//
//        for (interfaceSig <- abstractSigs) {
//          visitedMethods((interfaceSig, originalTypes.drop(1))) = Walker.MethodResult(
//            Array.fill(originalTypes.length - 1)(true),
//            interfaceSig.desc.ret,
//            new InsnList,
//            false,
//            Nil
//          )
//        }
//
//        val recursedResults = concreteSigs.map(ctx.walkMethod(_, inferredArgumentTypes))
//        val methodPure = recursedResults.forall(_.pure)
//        val argLivenesses = recursedResults.map(_.liveArgs)
//
//        val narrowReturnType = merge(recursedResults.map(_.inferredReturn))
//
//        if (Util.isCompatible(inferredArgumentTypes, originalTypes)) (argLivenesses, methodPure, narrowReturnType, originalInsn) // No narrowing
//        else {
//          val descChanged =
//            (static && inferredArgumentTypes != originalTypes) ||
//              (!static && inferredArgumentTypes.drop(1) != originalTypes.drop(1)) // ignore self type
//
//          val (mangledName, mangledDesc) =
//            if (!descChanged) (originalInsn.name, Desc.read(originalInsn.desc))
//            else {
//              Util.mangle(
//                originalInsn.name,
//                if (static) inferredArgumentTypes else inferredArgumentTypes.drop(1),
//                if (static) originalTypes else originalTypes.drop(1),
//                narrowReturnType,
//                calledDesc.ret
//              )
//            }
//
//          // We do not bother narrowing the owner of the bytecode or
//          // swapping from INVOKEDYNAMIC to INVOKEINTERFACE for now
//          val returnNode = new MethodInsnNode(
//            originalInsn.getOpcode,
//            originalInsn.owner,
//            mangledName,
//            mangledDesc.unparse
//          )
//          (argLivenesses, methodPure, narrowReturnType, returnNode)
//        }
//      }
//
//      if (methodPure && narrowRet.isConstant) {
//        val insns = popN(argOutCount) ++ Seq(Util.constantToInstruction(narrowRet.asInstanceOf[IType.Constant[_]]))
//        insns.foldLeft(currentFrame)(_.execute(_, ctx.ssaInterpreter(ctx.basicBlock, currentFrame)))
//      } else {
//        ctx.subCallArgLiveness(mangled) = argLivenesses.transpose.map(_.reduce(_ || _))
//
//        val srcs = currentFrame.stack.takeRight(argOutCount)
//
//        val readDesc = Desc.read(mangled.desc)
//        val ssa = originalInsn.getOpcode match{
//          case INVOKESTATIC =>
//            SSA.InvokeStatic(ctx.ssaInterpreter.blockStates(ctx.basicBlock), srcs, mangled.owner, mangled.name, readDesc)
//          case INVOKESPECIAL =>
//            SSA.InvokeSpecial(ctx.ssaInterpreter.blockStates(ctx.basicBlock), srcs, mangled.owner, mangled.name, readDesc)
//          case INVOKEVIRTUAL =>
//            SSA.InvokeVirtual(ctx.ssaInterpreter.blockStates(ctx.basicBlock), srcs, mangled.owner, mangled.name, readDesc)
//
//        }
//
//        ctx.basicBlock.value.append(ssa)
//        if (readDesc.ret != JType.Prim.V) {
//          ctx.ssaInterpreter.inferredTypes.put(ssa, narrowRet)
//        }
//        if (!methodPure) {
//          ctx.terminalInsns.append(ctx.basicBlock.value.last)
//        }
//        currentFrame.popPush(argOutCount, if (readDesc.ret == JType.Prim.V) Nil else Seq(ssa))
//      }
//    }
//
//
//  }
//
//  /**
//    * Removes a jump if we already statically know the destination, replacing
//    * it with POPs and an optional GOTO directly to the end destination.
//    *
//    * If we do not know the destination, we simply emit the jump in the final
//    * bytecode, and walk the two jump targets to make sure we have somewhere
//    * to jump to
//    */
//  def walkJump(walkBlock: (AbstractInsnNode, Frame[SSA]) => Walker.BlockInfo,
//               currentFrame: Frame[SSA],
//               terminalInsns: mutable.Buffer[SSA],
//               current: JumpInsnNode,
//               ssaInterpreter: StepEvaluator,
//               basicBlock: Block): Unit = {
//    def jumpBlock(pred: Boolean) = {
//      val popInsns = popN(Bytecode.stackEffect(current.getOpcode).pop(current))
//      val nextFrame = popInsns.foldLeft(currentFrame)(_.execute(_, ssaInterpreter(basicBlock, currentFrame)))
//      walkBlock(if (pred) current.label else current.getNext, nextFrame)
//    }
//
//    (
//      current.getOpcode,
//      currentFrame.stack.lift(currentFrame.stack.length - 1).map(ssaInterpreter.inferredTypes.get),
//      currentFrame.stack.lift(currentFrame.stack.length - 2).map(ssaInterpreter.inferredTypes.get)
//    ) match {
//      case (IFEQ, Some(IType.I(i)), _) => jumpBlock(i == 0)
//      case (IFNE, Some(IType.I(i)), _) => jumpBlock(i != 0)
//      case (IFLT, Some(IType.I(i)), _) => jumpBlock(i < 0)
//      case (IFGE, Some(IType.I(i)), _) => jumpBlock(i >= 0)
//      case (IFGT, Some(IType.I(i)), _) => jumpBlock(i > 0)
//      case (IFLE, Some(IType.I(i)), _) => jumpBlock(i <= 0)
//      case (IF_ICMPEQ, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 == i2)
//      case (IF_ICMPNE, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 != i2)
//      case (IF_ICMPLT, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 > i2)
//      case (IF_ICMPGE, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 <= i2)
//      case (IF_ICMPGT, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 <= i2)
//      case (IF_ICMPLE, Some(IType.I(i1)), Some(IType.I(i2))) => jumpBlock(i1 >= i2)
//      case (GOTO, _, _) => jumpBlock(true)
//
//      case _ => // JSR, IFNULL, IFNONNULL, IF_ACMPEQ, IF_ACMPNE, anything else
//        // We don't know how to handle these, so walk both cases
//        val nextFrame = currentFrame.execute(current, ssaInterpreter(basicBlock, currentFrame))
//        terminalInsns.append(basicBlock.value.last)
//        walkBlock(current.getNext, nextFrame)
//        walkBlock(current.label, nextFrame)
//    }
//  }
//  def popN(n: Int) = {
//    for(_ <- 0 until n) yield new InsnNode(POP)
//  }
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
                          pure: Boolean,
                          seenTryCatchBlocks: Seq[TryCatchBlockNode])
//
//  trait BlockInfo{
//    def startFrame: Frame[SSA]
//  }
//
//  case class BlockStub(startFrame: Frame[SSA]) extends BlockInfo
//
//  case class BlockResult(startFrame: Frame[SSA],
//                         terminalInsns: Seq[SSA],
//                         pure: Boolean,
//                         subCallArgLiveness: Map[AbstractInsnNode, Seq[Boolean]],
//                         lineNumberNodes: Set[LineNumberNode],
//                         tryHandlers: Seq[(LabelNode, JType.Cls)],
//                         blockInsns: Block) extends BlockInfo
//
//  case class InsnCtx(sig: MethodSig,
//                     terminalInsns: mutable.Buffer[SSA],
//                     var pure: Boolean,
//                     subCallArgLiveness: mutable.Map[AbstractInsnNode, Seq[Boolean]],
//                     lineNumberNodes: mutable.Set[LineNumberNode],
//                     walkBlock: (AbstractInsnNode, Frame[SSA]) => BlockInfo,
//                     seenMethods: Set[(MethodSig, Seq[IType])],
//                     walkMethod: (MethodSig, Seq[IType]) => Walker.MethodResult,
//                     ssaInterpreter: StepEvaluator,
//                     basicBlock: Block,
//                     jumpedBasicBlocks: (AbstractInsnNode, Frame[IType]) => Block,
//                     renderer: AbstractInsnNode => fansi.Str)
}