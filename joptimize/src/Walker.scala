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

      val initialControl = new SSA.Control{}

      val phiMerges0 = mutable.Set.empty[(SSA.Phi, SSA)]
      val analyzer = new Analyzer(new StepEvaluator(phiMerges0))
      val frames = analyzer.analyze(sig.cls.name, mn)

      val insns = mn.instructions.iterator().asScala.toVector

      def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)
      val terminals = insns.map(i => (i.getOpcode, i)).zipWithIndex.collect{
        case ((RETURN, insn), i) => (insn, SSA.Return(initialControl), i)

        case ((IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn), i) =>
          (insn, SSA.ReturnVal(initialControl, frameTop(i, 0)), i)

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

      val phiMerges = phiMerges0.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap
      pprint.log(terminals)
      pprint.log(blockStarts)
      pprint.log(phiMerges)

      val controlDependencies = new IdentityHashMap[SSA, SSA]()

      pprint.log(frames.zipWithIndex.map(_.swap))
      println(Renderer.render(terminals.map(_._2), phiMerges))

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