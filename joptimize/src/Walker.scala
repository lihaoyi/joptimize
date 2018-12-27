package joptimize


import java.util.IdentityHashMap

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
             findSupertypes: JType.Cls => Seq[JType.Cls],
             isConcrete: MethodSig => Boolean,
             exists: MethodSig => Boolean,
             merge: Seq[IType] => IType,
             dataflow: ITypeInterpreter,
             ignore: String => Boolean) {

  def walkMethod(sig: MethodSig,
                 originalInsns: InsnList,
                 tryCatchBlocks: Seq[TryCatchBlockNode],
                 args: Seq[IType],
                 maxLocals: Int,
                 maxStack: Int,
                 seenMethods0: Set[(MethodSig, Seq[IType])]): Walker.MethodResult = {

    visitedMethods.getOrElseUpdate((sig, args.drop(if (sig.static) 0 else 1)), {
      val inferredTypes = new IdentityHashMap[SSA, IType]()
      val seenMethods = seenMethods0 ++ Seq((sig, args.map(_.widen)))
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

//      pprint.log(sig)

      val initialArgumentLValues = for((a, i) <- args.zipWithIndex) yield {
        val ssa = SSA.Arg(i, a.getSize)
        inferredTypes.put(ssa, a)
        ssa
      }

      val merges = mutable.Buffer.empty[(Frame[SSA], Frame[SSA])]

      val insnTryCatchBlocks = {
        val tryCatchBlockIndices = tryCatchBlocks.zipWithIndex.toMap
        val tryCatchBlockStarts = tryCatchBlocks.groupBy(_.start)
        val tryCatchBlockEnds = tryCatchBlocks.groupBy(_.end)
        val mapping = mutable.LinkedHashMap.empty[AbstractInsnNode, Map[Int, (JType.Cls, LabelNode)]]

        var currentTryCatchBlocks = Map.empty[Int, (JType.Cls, LabelNode)]

        originalInsns.iterator().asScala.foreach{
          case insn: LabelNode =>
            for(tcb <- tryCatchBlockStarts.getOrElse(insn, Nil)){
              currentTryCatchBlocks += ((
                tryCatchBlockIndices(tcb),
                (JType.Cls(tcb.`type`), tcb.handler)
              ))
            }
            for(tcb <- tryCatchBlockEnds.getOrElse(insn, Nil)){
              currentTryCatchBlocks -= tryCatchBlockIndices(tcb)
            }
            mapping(insn) = currentTryCatchBlocks

          case insn =>
            mapping(insn) = currentTryCatchBlocks
        }

        mapping
      }

      walkBlock(
        originalInsns.getFirst,
        Frame.initial(
          maxLocals, maxStack,
          initialArgumentLValues,
          null
        ),
        Set(), visitedBlocks, sig, seenMethods,
        recurse = (staticSig, types0) => {
          val argOutCount = staticSig.desc.args.length + (if (staticSig.static) 0 else 1)
          val types = if (seenMethods((staticSig, types0.map(_.widen)))) types0.map(_.widen) else types0
          if (seenMethods((staticSig, types))) {
            // When we hit recursive methods, simply assume that
            // they are impure and that all their arguments are live.

            Walker.MethodResult(
              Seq.fill(argOutCount)(true),
              staticSig.desc.ret,
              new InsnList,
              false,
              Nil
            )
          }
          else {
            val clinitSig = MethodSig(staticSig.cls, "<clinit>", Desc.read("()V"), true)
            if (!seenMethods.contains((clinitSig, Nil))) {
              for(clinit <- lookupMethod(clinitSig)){
                walkMethod(
                  clinitSig,
                  clinit.instructions,
                  clinit.tryCatchBlocks.asScala,
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
              lookupMethod(staticSig).get.tryCatchBlocks.asScala,
              types.toList,
              lookupMethod(staticSig).get.maxLocals,
              lookupMethod(staticSig).get.maxStack,
              seenMethods ++ Seq(staticSig -> types)
            )
          }
        },
        merges,
        insnTryCatchBlocks.toMap,
        inferredTypes
      )

//      pprint.log(sig -> "END")

//      pprint.log((sig, pure, methodReturns))
      val allVisitedBlocks = visitedBlocks.values.map(_.asInstanceOf[Walker.BlockResult]).toSeq
      val methodReturns = allVisitedBlocks.flatMap(_.methodReturns).toSeq
      val lineNumberNodes = allVisitedBlocks.flatMap(_.lineNumberNodes)
      val methodInsnMapping = allVisitedBlocks.flatMap(_.methodInsnMapping).toMap

      val pure = allVisitedBlocks.forall(_.pure)
      val resultType =
        if (methodReturns.isEmpty) sig.desc.ret // abstract methods have no return insn
        else merge(methodReturns.map(inferredTypes.get))


      val outputTcbs = mutable.Buffer.empty[TryCatchBlockNode]
//      for((block, blockIndex) <- allVisitedBlocks.zipWithIndex){
//        for(handler <- block.tryHandlers) {
//          val tcb = new TryCatchBlockNode(
//            (
//              allVisitedBlocks(blockIndex).insns.getFirst match {
//                case l: LabelNode => Some(l)
//                case _ => None
//              }
//              ).orElse(
//              allVisitedBlocks.lift(blockIndex - 1).flatMap(b => b.insns.getLast match {
//                case l: LabelNode => Some(l)
//                case _ => None
//              }
//              )).getOrElse {
//              val l = new LabelNode()
//              allVisitedBlocks(blockIndex).insns.insert(l)
//              l
//            },
//            (
//              allVisitedBlocks(blockIndex).insns.getLast match {
//                case l: LabelNode => Some(l)
//                case _ => None
//              }
//              ).orElse(
//              allVisitedBlocks.lift(blockIndex + 1).flatMap(b => b.insns.getFirst match {
//                case l: LabelNode => Some(l)
//                case _ => None
//              }
//              )).getOrElse {
//              val l = new LabelNode()
//              allVisitedBlocks(blockIndex).insns.add(l)
//              l
//            },
//            handler._1,
//            handler._2.name
//          )
//          if (tcb.start != tcb.end) outputTcbs.append(tcb)
//        }
//      }


      val (outputInsns, liveArguments) = Liveness(allVisitedBlocks, merges)

      val liveArgs =
        if (sig.static) sig.desc.args.indices.map(liveArguments)
        else Seq(true) ++ sig.desc.args.indices.map(_+1).map(liveArguments)

      Walker.MethodResult(
        liveArgs,
        resultType,
        outputInsns,
        pure,
        outputTcbs
      )
    })
  }

  def walkBlock(blockStart: AbstractInsnNode,
                blockState: Frame[SSA],
                seenBlocks0: Set[AbstractInsnNode],
                visitedBlocks: mutable.LinkedHashMap[(AbstractInsnNode, Frame[IType]), Walker.BlockInfo],
                sig: MethodSig,
                seenMethods: Set[(MethodSig, scala.Seq[IType])],
                recurse: (MethodSig, Seq[IType]) => Walker.MethodResult,
                merges: mutable.Buffer[(Frame[SSA], Frame[SSA])],
                insnTryCatchBlocks: Map[AbstractInsnNode, Map[Int, (JType.Cls, LabelNode)]],
                inferredTypes: IdentityHashMap[SSA, IType]): Walker.BlockInfo = {

    val seenBlocks = seenBlocks0 + blockStart

    val typeState = blockState.map(inferredTypes.get)
    //        pprint.log("VISITING BLOCK" + Util.prettyprint(blockStart))
    visitedBlocks.get((blockStart, typeState)) match{
      case Some(res @ Walker.BlockStub(blockIndex, frame)) =>
        merges.append((frame, blockState))
        res

      case Some(res: Walker.BlockResult) =>
        merges.append((res.startFrame, blockState))
        res

      case None =>
        val blockIndex = visitedBlocks.size
        //          println("NEW BLOCK " + (currentInsn, currentFrame))

        def walkBlock1 = walkBlock(
          _, _,
          seenBlocks, visitedBlocks, sig, seenMethods, recurse, merges, insnTryCatchBlocks, inferredTypes
        )

        val blockInsnMapping = mutable.Map.empty[AbstractInsnNode, AbstractInsnNode]
        val methodReturns = mutable.Buffer.empty[SSA]
        val terminalInsns = mutable.Buffer.empty[Terminal]
        val pure = sig.name != "<init>" && sig.name != "<clinit>"
        val subCallArgLiveness = mutable.Map.empty[AbstractInsnNode, Seq[Boolean]]
        val lineNumberNodes = mutable.Set.empty[LineNumberNode]

        val ctx = Walker.InsnCtx(
          sig,
          blockInsnMapping = blockInsnMapping,
          methodReturns = methodReturns,
          terminalInsns = terminalInsns,
          pure = pure,
          subCallArgLiveness = subCallArgLiveness,
          lineNumberNodes = lineNumberNodes,
          walkBlock = walkBlock1,
          seenMethods = seenMethods,
          walkMethod = recurse,
          inferredTypes = inferredTypes
        )

        visitedBlocks((blockStart, typeState)) = Walker.BlockStub(
          blockIndex,
          blockState
        )

        val allHandlers = mutable.Buffer.empty[(LabelNode, JType.Cls)]

        val blockEnd = walkInsn(blockStart, blockState, ctx)

        for ((_, (tpe, handler))<- insnTryCatchBlocks(blockStart)) {
          val ssa = SSA.Arg(-1, 1)
          inferredTypes.put(ssa, tpe)
          val handlerFrame = blockState.handleException(ssa)
          val dest = walkBlock1(handler, handlerFrame)
          allHandlers.append((handler, tpe))
        }

        val methodInsnMapping = mutable.Map.empty[AbstractInsnNode, List[AbstractInsnNode]]
        for((k, v) <- ctx.blockInsnMapping){
          methodInsnMapping(k) = v :: methodInsnMapping.getOrElse(k, Nil)
        }

        val res = Walker.BlockResult(
          blockIndex,
          blockState,
          methodReturns,
          terminalInsns,
          ctx.pure,
          subCallArgLiveness.toMap,
          lineNumberNodes.toSet,
          methodInsnMapping.toMap,
          allHandlers,
          blockEnd
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
                              currentFrame: Frame[SSA],
                              ctx: Walker.InsnCtx): (Option[Int], Option[AbstractInsnNode]) = {
    /**
      * Walk the next instruction as a new block, if it is a label. If not
      * then return `false` so we can tail-recursively walk it as a simple
      * instruction
      */
    def walkNextLabel(nextFrame1: Frame[SSA]) = {
      if (currentInsn.getNext.isInstanceOf[LabelNode]) {
        val res = ctx.walkBlock(currentInsn.getNext, nextFrame1)
        Some((Some(res.blockIndex), None))
      } else None
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
              clinit.tryCatchBlocks.asScala,
              Nil,
              clinit.maxLocals,
              clinit.maxStack,
              ctx.seenMethods ++ Seq((clinitSig, Nil))
            )
          }
        }
        if (!ignore(current.owner)) {
          visitedClasses.add(JType.Cls(current.owner))
        }

        if (current.getOpcode == PUTFIELD || current.getOpcode == PUTSTATIC){
          ctx.terminalInsns.append(Terminal(
            n,
            currentFrame.stack.takeRight(
              Bytecode.stackEffect(currentInsn.getOpcode).pop(currentInsn)
            ),
          ))
          ctx.pure = false
        }

        val nextFrame = currentFrame.execute(n, SSAInterpreter)
        walkNextLabel(nextFrame) match {
          case Some(t) => t
          case None => walkInsn(current.getNext, nextFrame, ctx)
        }

      case current: FrameNode =>
        // We discard frame nodes; we're going to be doing a bunch of mangling
        // so they'll all be wrong, so easier just let ASM recompute them
        walkNextLabel(currentFrame) match {
          case Some(t) => t
          case None => walkInsn(current.getNext, currentFrame, ctx)
        }

      case _: LabelNode | _: IincInsnNode | _: IntInsnNode | _: LdcInsnNode | _: MultiANewArrayInsnNode =>
        val n = Util.clone(currentInsn, ctx.blockInsnMapping)
        val nextFrame = currentFrame.execute(n, SSAInterpreter)
        walkNextLabel(nextFrame) match {
          case Some(t) => t
          case None => walkInsn(currentInsn.getNext, nextFrame, ctx)
        }

      case current: InsnNode =>
        current.getOpcode match{
          case IASTORE | LASTORE | FASTORE | DASTORE | AASTORE | BASTORE | CASTORE | SASTORE |
               IALOAD | LALOAD | FALOAD | DALOAD | AALOAD | BALOAD | CALOAD | SALOAD =>
            val (newInsns, nextFrame) = constantFold(
              currentInsn, currentFrame, ctx.blockInsnMapping, ctx.inferredTypes
            )

            ctx.terminalInsns.append(Terminal(
              newInsns.last,
              currentFrame.stack.takeRight(
                Bytecode.stackEffect(currentInsn.getOpcode).pop(currentInsn)
              ),
            ))

            walkNextLabel(nextFrame) match {
              case Some(t) => t
              case None => walkInsn(current.getNext, nextFrame, ctx)
            }
          case ARETURN | DRETURN | FRETURN | IRETURN | LRETURN =>
            val n = new InsnNode(current.getOpcode)
            ctx.methodReturns.append(currentFrame.stack.last)
            ctx.terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last)))
            (None, None)
          case RETURN =>
            val n = new InsnNode(current.getOpcode)
            ctx.terminalInsns.append(Terminal(n, Seq()))
            (None, None)
          case ATHROW =>
            val n = new InsnNode(current.getOpcode)
            ctx.terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last)))
            (None, None)
          case _ =>
            val (nextInsns, nextFrame) = constantFold(
              currentInsn, currentFrame, ctx.blockInsnMapping, ctx.inferredTypes
            )
            walkNextLabel(nextFrame) match {
              case Some(t) => t
              case None => walkInsn(current.getNext, nextFrame, ctx)
            }
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
          val nextFrame = currentFrame.execute(n, SSAInterpreter)
          walkNextLabel(nextFrame) match {
            case Some(t) => t
            case None => walkInsn(current.getNext, nextFrame, ctx)
          }
        }else if(current.bsm == Util.makeConcatWithConstants){
          val n = Util.clone(currentInsn, ctx.blockInsnMapping)
          val nextFrame = currentFrame.execute(n, SSAInterpreter)
          walkNextLabel(nextFrame) match {
            case Some(t) => t
            case None => walkInsn(current.getNext, nextFrame, ctx)
          }
        } else{
          pprint.log(current.bsm)
          pprint.log(current.bsmArgs)
          pprint.log(current.bsmArgs.map(_.getClass))
          pprint.log(current.desc)
          pprint.log(current.name)
          ???
        }

      case current: JumpInsnNode =>
        walkJump(ctx.walkBlock, currentFrame, ctx.terminalInsns, current, ctx.blockInsnMapping, ctx.inferredTypes)

      case current: LineNumberNode =>
        // We do not bother copying line number nodes in-place. Instead, we
        // just batch them all up and then append them to the end of the insn
        // list once we're done processing a method, since the position of the
        // line number node in the insn list doesn't matter (only the label
        // pointer does)
        ctx.lineNumberNodes.add(current)
        walkNextLabel(currentFrame) match {
          case Some(t) => t
          case None => walkInsn(current.getNext, currentFrame, ctx)
        }


      case current: LookupSwitchInsnNode =>
        val n = new LookupSwitchInsnNode(null, Array(), Array())
        ctx.terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last)))
        val nextFrame = currentFrame.execute(n, SSAInterpreter)
        ctx.walkBlock(current.dflt, nextFrame)
        n.dflt = current.dflt
        n.keys = current.keys
        current.labels.asScala.foreach(ctx.walkBlock(_, nextFrame))
        n.labels =  current.labels.asScala.asJava
        (None, Some(current))
      case current: MethodInsnNode =>
        val nextFrame = walkMethodInsn(currentFrame, ctx, current)
        walkNextLabel(nextFrame) match {
          case Some(t) => t
          case None => walkInsn(current.getNext, nextFrame, ctx)
        }

      case current: TableSwitchInsnNode =>
        val n = new TableSwitchInsnNode(current.min, current.max, null)
        val nextFrame = currentFrame.execute(n, SSAInterpreter)
        ctx.terminalInsns.append(Terminal(n, Seq(currentFrame.stack.last)))
        ctx.walkBlock(current.dflt, nextFrame)
        n.dflt = current.dflt
        current.labels.asScala.foreach(ctx.walkBlock(_, nextFrame))
        n.labels = current.labels.asScala.asJava
        (None, Some(current))
      case current: TypeInsnNode =>
        if (!ignore(current.desc)) {
          visitedClasses.add(JType.Cls(current.desc))
        }
        val (newInsns, nextFrame) = constantFold(
          currentInsn, currentFrame, ctx.blockInsnMapping, ctx.inferredTypes
        )
        walkNextLabel(nextFrame) match {
          case Some(t) => t
          case None => walkInsn(current.getNext, nextFrame, ctx)
        }

      case current: VarInsnNode =>
        val (newInsns, nextFrame) = constantFold(
          currentInsn, currentFrame, ctx.blockInsnMapping, ctx.inferredTypes
        )
        walkNextLabel(nextFrame) match {
          case Some(t) => t
          case None => walkInsn(current.getNext, nextFrame, ctx)
        }
    }
  }

  def walkMethodInsn(currentFrame: Frame[SSA],
                     ctx: Walker.InsnCtx,
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
      ))
      currentFrame.execute(copy, SSAInterpreter)
    } else {

      val (argLivenesses, methodPure, narrowRet, mangled) = {
        val static = originalInsn.getOpcode == INVOKESTATIC
        val special = originalInsn.getOpcode == INVOKESPECIAL

        val calledDesc = Desc.read(originalInsn.desc)
        val calledSelf = if (static) Nil else Seq(JType.Cls(originalInsn.owner))
        val originalTypes = calledSelf ++ calledDesc.args.toSeq

        val inferredArgumentTypes =
          (currentFrame.stack.length - originalTypes.map(_.getSize).sum)
            .until(currentFrame.stack.length)
            .map(x => ctx.inferredTypes.get(currentFrame.stack(_)))

        val sig = MethodSig(originalInsn.owner, originalInsn.name, Desc.read(originalInsn.desc), static)

        val (concreteSigs, abstractSigs) =
          if (special) (Seq(sig), Nil)
          else if (static) (
            findSupertypes(sig.cls)
              .iterator
              .map(MethodSig(_, originalInsn.name, calledDesc, true))
              .filter(exists)
              .take(1)
              .toSeq,
            Nil
          )
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
            false,
            Nil
          )
        }

        val recursedResults = concreteSigs.map(ctx.walkMethod(_, inferredArgumentTypes))
        val methodPure = recursedResults.forall(_.pure)
        val argLivenesses = recursedResults.map(_.liveArgs)

        val narrowReturnType = merge(recursedResults.map(_.inferredReturn))

        if (Util.isCompatible(inferredArgumentTypes, originalTypes)) (argLivenesses, methodPure, narrowReturnType, originalInsn) // No narrowing
        else {
          val descChanged =
            (static && inferredArgumentTypes != originalTypes) ||
              (!static && inferredArgumentTypes.drop(1) != originalTypes.drop(1)) // ignore self type

          val (mangledName, mangledDesc) =
            if (!descChanged) (originalInsn.name, Desc.read(originalInsn.desc))
            else {
              Util.mangle(
                originalInsn.name,
                if (static) inferredArgumentTypes else inferredArgumentTypes.drop(1),
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
        ))
      }

      if (methodPure && narrowRet.isConstant) {
        val insns = popN(argOutCount) ++ Seq(Util.constantToInstruction(narrowRet.asInstanceOf[IType.Constant]))
        insns.foldLeft(currentFrame)(_.execute(_, SSAInterpreter))
      } else {
        val finalArgLiveness = argLivenesses.transpose.map(_.reduce(_ || _))
        ctx.subCallArgLiveness(mangled.asInstanceOf[MethodInsnNode]) = finalArgLiveness

        currentFrame.execute(mangled, SSAInterpreter)
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
  def walkJump(walkBlock: (AbstractInsnNode, Frame[SSA]) => Walker.BlockInfo,
               currentFrame: Frame[SSA],
               terminalInsns: mutable.Buffer[Terminal],
               current: JumpInsnNode,
               blockInsnMapping: mutable.Map[AbstractInsnNode, AbstractInsnNode],
               inferredTypes: IdentityHashMap[SSA, IType]): (Option[Int], Option[AbstractInsnNode]) = {
    def jumpBlock(pred: Boolean): (Option[Int], Option[AbstractInsnNode]) = {
      val popInsns = popN(Bytecode.stackEffect(current.getOpcode).pop(current))
      val nextFrame = popInsns.foldLeft(currentFrame)(_.execute(_, SSAInterpreter))
      val res = walkBlock(if (pred) current.label else current.getNext, nextFrame)
      (Some(res.blockIndex), None)
    }

    (
      current.getOpcode,
      currentFrame.stack.lift(currentFrame.stack.length - 1).map(inferredTypes.get),
      currentFrame.stack.lift(currentFrame.stack.length - 2).map(inferredTypes.get)
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
        val n = new JumpInsnNode(current.getOpcode, current.label)
        blockInsnMapping(current) = n
        terminalInsns.append(Terminal(
          n,
          currentFrame.stack.takeRight(Bytecode.stackEffect(current.getOpcode).pop(current)),
        ))

        val nextFrame = currentFrame.execute(n, SSAInterpreter)
        val fallThrough = walkBlock(current.getNext, nextFrame)
        val jumpTarget = walkBlock(current.label, nextFrame)

        n.label = current.getNext.asInstanceOf[LabelNode]
        (Some(fallThrough.blockIndex), Some(n))
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
                   currentFrame: Frame[SSA],
                   blockInsnMapping: mutable.Map[AbstractInsnNode, AbstractInsnNode],
                   inferredTypes: IdentityHashMap[SSA, IType]): (Seq[AbstractInsnNode], Frame[SSA]) = {
    val stackEffect = Bytecode.stackEffect(currentInsn.getOpcode)
    val tentativeNextFrame = currentFrame.execute(currentInsn, SSAInterpreter)
    val newInsns =
      if (stackEffect.push(currentInsn) == 1){
        inferredTypes.get(tentativeNextFrame.stack.last) match{
          case const: IType.Constant => popN(stackEffect.pop(currentInsn)) ++ Seq(Util.constantToInstruction(const))
          case _ => Seq(Util.clone(currentInsn, blockInsnMapping))
        }
      }else Seq(Util.clone(currentInsn, blockInsnMapping))
    (newInsns, newInsns.foldLeft(currentFrame)(_.execute(_, SSAInterpreter)))
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
                          pure: Boolean,
                          seenTryCatchBlocks: Seq[TryCatchBlockNode])

  trait BlockInfo{
    def blockIndex: Int
    def startFrame: Frame[SSA]
  }

  case class BlockStub(blockIndex: Int,
                       startFrame: Frame[SSA]) extends BlockInfo

  case class BlockResult(blockIndex: Int,
                         startFrame: Frame[SSA],
                         methodReturns: Seq[SSA],
                         terminalInsns: Seq[Terminal],
                         pure: Boolean,
                         subCallArgLiveness: Map[AbstractInsnNode, Seq[Boolean]],
                         lineNumberNodes: Set[LineNumberNode],
                         methodInsnMapping: Map[AbstractInsnNode, List[AbstractInsnNode]],
                         tryHandlers: Seq[(LabelNode, JType.Cls)],
                         end: (Option[Int], Option[AbstractInsnNode])) extends BlockInfo

  case class InsnCtx(sig: MethodSig,
                     blockInsnMapping: mutable.Map[AbstractInsnNode, AbstractInsnNode],
                     methodReturns: mutable.Buffer[SSA],
                     terminalInsns: mutable.Buffer[Terminal],
                     var pure: Boolean,
                     subCallArgLiveness: mutable.Map[AbstractInsnNode, Seq[Boolean]],
                     lineNumberNodes: mutable.Set[LineNumberNode],
                     walkBlock: (AbstractInsnNode, Frame[SSA]) => BlockInfo,
                     seenMethods: Set[(MethodSig, Seq[IType])],
                     walkMethod: (MethodSig, Seq[IType]) => Walker.MethodResult,
                     inferredTypes: IdentityHashMap[SSA, IType])
}