package joptimize.analysis



import joptimize.model._

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import scala.collection.mutable

class Walker(isInterface: JType.Cls => Boolean,
             lookupMethod: MethodSig => Option[MethodNode],
             visitedMethods: mutable.LinkedHashMap[(MethodSig, Seq[IType]), Walker.MethodResult],
             visitedClasses: mutable.LinkedHashSet[JType.Cls],
             findSubtypes: JType.Cls => List[JType.Cls],
             findSupertypes: JType.Cls => Seq[JType.Cls],
             isConcrete: MethodSig => Boolean,
             exists: MethodSig => Boolean,
             merge: Seq[IType] => IType,
//             typer: Typer,
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

      val phiMerges0 = mutable.LinkedHashSet.empty[SSA.Phi]

      val insns = mn.instructions.iterator().asScala.toVector

      val blockStarts = insns.take(1) ++ insns
        .collect{
          case n: TableSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
          case n: LookupSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
          case n: JumpInsnNode => Seq(n.label) ++ Option(n.getNext)
          case n if n == insns.head => Seq(n)
        }
        .flatten
        .sortBy(insns.indexOf)
      val regionStarts = mutable.LinkedHashMap(blockStarts.map(i => i -> (new SSA.Merge(insns.indexOf(i), Set()): SSA.Block)):_*)

      //      regionStarts.keys.map("RSK " + Renderer.render(mn.instructions, _)).foreach(println)
      def findStartRegion(insn: AbstractInsnNode): SSA.Block = {
        var current = insn
        var region: SSA.Block = null
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
      val frames = joptimize.bytecode.Analyzer.analyze(
        sig.cls.name, mn,
        new StepEvaluator(
          phiMerges0,
          x => regionStarts.contains(insns(x)),
          i => findStartRegion(insns(i)),
          i => regionStarts(insns(i))
        )
      )
      def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)

      def mergeBlocks(lhs0: AbstractInsnNode, rhs: SSA.Block, rhsInsn: Option[AbstractInsnNode] = None): Unit = {
        println(Renderer.renderInsns(mn.instructions, lhs0))
        val lhs = regionStarts(lhs0)
        (lhs, rhs) match{
          case (l: SSA.Merge, r) =>
            l.incoming += r
            r.downstream += l
        }
      }


      val terminals = insns.map(i => (i.getOpcode, i)).zipWithIndex.collect{
        case ((RETURN, insn), i) => (insn, new SSA.Return(findStartRegion(insn)), i) :: Nil

        case ((IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn), i) =>
          (insn, new SSA.ReturnVal(findStartRegion(insn), frameTop(i, 0)), i) :: Nil

        case ((ATHROW, insn), i) => (insn, new SSA.AThrow(frameTop(i, 0)), i) :: Nil

        case ((GOTO, insn: JumpInsnNode), i) =>
          mergeBlocks(insn.label, findStartRegion(insn), Some(insn))
          Nil

        case ((IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE, insn: JumpInsnNode), i) =>
          val n = new SSA.UnaBranch(findStartRegion(insn), frameTop(i, 0), SSA.UnaBranch.lookup(insn.getOpcode))
          mergeBlocks(insn.label, new SSA.True(n))
          mergeBlocks(insn.getNext, new SSA.False(n))

          Nil

        case ((IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE, insn: JumpInsnNode), i) =>
          val startReg = findStartRegion(insn)
          val n = new SSA.BinBranch(startReg, frameTop(i, 0), frameTop(i, 1), SSA.BinBranch.lookup(insn.getOpcode))
          mergeBlocks(insn.label, new SSA.True(n))
          mergeBlocks(insn.getNext, new SSA.False(n))
          Nil

        case ((_, insn), i) if Option(insn.getNext).exists(regionStarts.contains) =>
          mergeBlocks(insn.getNext, findStartRegion(insn), Some(insn))
          Nil
      }.flatten

      simplifyPhiMerges(phiMerges0, regionStarts)

      val program = Program(terminals.map(_._2))

      val (printed, mapping) = Renderer.renderSSA(program)
      println(printed)
      pprint.log(mapping)

      CodeGen(program, mapping)
      ???
    })
  }

  def simplifyPhiMerges(phiMerges0: mutable.LinkedHashSet[SSA.Phi],
                        regionStarts: mutable.LinkedHashMap[AbstractInsnNode, SSA.Block]) = {
    val queue = phiMerges0 ++ regionStarts.values
    queue.foreach(_.checkLinks())

    while (queue.nonEmpty) {
      val current = queue.head
      queue.remove(current)
      val replacement = current match {
        case phi: SSA.Phi =>
          val filteredValues = phi.incoming.filter(_._2 != phi)

          if (filteredValues.map(_._2).size == 1) Some(filteredValues.head._2)
          else None

        case reg: SSA.Merge =>
          if (reg.incoming.size == 1) Some(reg.incoming.head)
          else None

        case _ => None
      }
      for (replacement <- replacement) {
        for (v <- current.upstream) v.downstream.remove(current)
        replacement.downstream.remove(current)
        val deltaDownstream = current.downstream.filter(_ != current)
        replacement.downstream ++= deltaDownstream
        for (down <- deltaDownstream) SSA.update(down, current, replacement)
        replacement.downstream.foreach(queue.add)
      }
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
                          pure: Boolean,
                          seenTryCatchBlocks: Seq[TryCatchBlockNode])
}