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
      println(Renderer.renderInsns(mn.instructions))

      val phiMerges0 = mutable.Set.empty[(SSA.Phi, SSA)]
      val analyzer = new Analyzer(new StepEvaluator(phiMerges0))
      val frames = analyzer.analyze(sig.cls.name, mn)

      val insns = mn.instructions.iterator().asScala.toVector

      val blockStarts = insns.take(1) ++ insns
        .collect{
          case n: TableSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
          case n: LookupSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
          case n: JumpInsnNode => Seq(n.label) ++ Option(n.getNext)
        }
        .flatten
        .sortBy(insns.indexOf)

      val regionStarts = mutable.LinkedHashMap(blockStarts.map(_ -> new SSA.Region()):_*)


      def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)

//      regionStarts.keys.map("RSK " + Renderer.render(mn.instructions, _)).foreach(println)
      def findStartRegion(insn: AbstractInsnNode): SSA.Region = {
        var current = insn
        var region: SSA.Region = null
        while({
//          println("XXX " + Renderer.render(mn.instructions, current))
          regionStarts.get(current) match{
            case None =>
              current = current.getPrevious
              true
            case Some(x) =>
              region = x
              false
          }
        })()

        region
      }

      val regionMerges = mutable.LinkedHashMap.empty[SSA.Region, Set[SSA.Control]]
      regionMerges(regionStarts(insns.head)) = Set.empty

      val terminals = insns.map(i => (i.getOpcode, i)).zipWithIndex.collect{
        case ((RETURN, insn), i) => (insn, SSA.Return(findStartRegion(insn)), i)

        case ((IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn), i) =>
          (insn, SSA.ReturnVal(findStartRegion(insn), frameTop(i, 0)), i)

        case ((ATHROW, insn), i) => (insn, SSA.AThrow(frameTop(i, 0)), i)

        case ((GOTO, insn: JumpInsnNode), i) =>
          val n = SSA.Goto(findStartRegion(insn))
          regionMerges(regionStarts(insn.label)) =
            regionMerges.getOrElse(regionStarts(insn.label), Set.empty) + SSA.True(n)
          (insn, n, i)

        case ((IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE, insn: JumpInsnNode), i) =>
          val n = SSA.UnaryBranch(findStartRegion(insn), frameTop(i, 0), SSA.UnaryBranch.lookup(insn.getOpcode))
          regionMerges(regionStarts(insn.label)) =
            regionMerges.getOrElse(regionStarts(insn.label), Set.empty) + SSA.True(n)
          regionMerges(regionStarts(insn.getNext)) =
            regionMerges.getOrElse(regionStarts(insn.getNext), Set.empty) + SSA.False(n)

          (insn, n, i)

        case ((IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE, insn: JumpInsnNode), i) =>
          val n = SSA.BinBranch(findStartRegion(insn), frameTop(i, 0), frameTop(i, 1), SSA.BinBranch.lookup(insn.getOpcode))
          regionMerges(regionStarts(insn.label)) =
            regionMerges.getOrElse(regionStarts(insn.label), Set.empty) + SSA.True(n)
          regionMerges(regionStarts(insn.getNext)) =
            regionMerges.getOrElse(regionStarts(insn.getNext), Set.empty) + SSA.False(n)
          (insn, n, i)
      }

      val regionMerges2 = regionStarts.map{case (k, v) => (v, regionMerges(v))}

      val phiMerges = phiMerges0.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap
//      pprint.log(terminals)
//      pprint.log(blockStarts)
//      pprint.log(phiMerges)


//      pprint.log(frames.zipWithIndex.map(_.swap))
      println(regionMerges2.keysIterator.map(r => Renderer.renderInsns(mn.instructions, regionStarts.map(_.swap).apply(r)).toString).toList)
      println(Renderer.renderSSA(terminals.map(_._2), regionMerges2, phiMerges))

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
}