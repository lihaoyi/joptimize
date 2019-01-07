package joptimize.analysis



import joptimize.model._

import collection.JavaConverters._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
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

      val regionStarts0 = mutable.LinkedHashMap.empty[Int, SSA.Region]
      val frames = joptimize.bytecode.Analyzer.analyze(sig.cls.name, mn, new StepEvaluator(regionStarts0))

      val insns = mn.instructions.iterator().asScala.toVector

      val blockStarts = insns.take(1) ++ insns
        .collect{
          case n: TableSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
          case n: LookupSwitchInsnNode => Seq(n.dflt) ++ n.labels.asScala
          case n: JumpInsnNode => Seq(n.label) ++ Option(n.getNext)
        }
        .flatten
        .sortBy(insns.indexOf)

      val regionStarts = regionStarts0.map{case (k, v) => (insns(k), v: SSA.Ctrl)}

      def frameTop(i: Int, n: Int) = frames(i).getStack(frames(i).getStackSize - 1 - n)

//      regionStarts.keys.map("RSK " + Renderer.render(mn.instructions, _)).foreach(println)
      def findStartRegion(insn: AbstractInsnNode): SSA.Ctrl = {
        var current = insn
        var region: SSA.Ctrl = null
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

      def mergeControls(lhs0: AbstractInsnNode, rhs: SSA.Ctrl, rhsInsn: Option[AbstractInsnNode] = None): Unit = {
        val lhs = regionStarts.getOrElseUpdate(lhs0, new SSA.Region(Set()))
        (lhs, rhs) match{
          case (l: SSA.Region, r) if l.incoming.isEmpty =>
            regionStarts(lhs0) = r
            l.replaceWith(r)

          case (l, r: SSA.Region) if r.incoming.isEmpty =>
            regionStarts(rhsInsn.get) = l
            r.replaceWith(l)

          case (l: SSA.Region, r: SSA.Region) =>
            r.replaceWith(l)
            l.incoming ++= r.incoming
            l.update()
            regionStarts(lhs0) = r

          case (l: SSA.Region, r: SSA.Ctrl) =>
            r.replaceWith(l)
            l.incoming += r
            l.update()

          case (l: SSA.Ctrl, r: SSA.Region) =>
            l.replaceWith(r)
            r.incoming += l
            r.update()
            regionStarts(lhs0) = r

          case (l: SSA.Ctrl, r: SSA.Ctrl) =>
            val reg = new SSA.Region(Set())
            l.replaceWith(reg)
            r.replaceWith(reg)
            reg.incoming += l
            reg.incoming += r
            reg.update()
            regionStarts(lhs0) = reg
        }
      }
      regionStarts.update(insns.head, new SSA.Region(Set()))

      val terminals = insns.map(i => (i.getOpcode, i)).zipWithIndex.collect{
        case ((RETURN, insn), i) => (insn, SSA.Return(findStartRegion(insn)), i) :: Nil

        case ((IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn), i) =>
          (insn, SSA.ReturnVal(findStartRegion(insn), frameTop(i, 0)), i) :: Nil

        case ((ATHROW, insn), i) => (insn, SSA.AThrow(frameTop(i, 0)), i) :: Nil

        case ((GOTO, insn: JumpInsnNode), i) =>
          mergeControls(insn.label, findStartRegion(insn), Some(insn))
          Nil

        case ((IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE, insn: JumpInsnNode), i) =>
          val n = SSA.UnaBranch(findStartRegion(insn), frameTop(i, 0), SSA.UnaBranch.lookup(insn.getOpcode))
          mergeControls(insn.label, new SSA.True(n))
          mergeControls(insn.getNext, new SSA.False(n))

          Nil

        case ((IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE, insn: JumpInsnNode), i) =>
          val startReg = findStartRegion(insn)
          val n = SSA.BinBranch(startReg, frameTop(i, 0), frameTop(i, 1), SSA.BinBranch.lookup(insn.getOpcode))

          mergeControls(insn.label, new SSA.True(n))

          mergeControls(insn.getNext, new SSA.False(n))
          Nil

        case ((_, insn), i) if Option(insn.getNext).exists(regionStarts.contains) =>
          mergeControls(insn.getNext, findStartRegion(insn), Some(insn))
          Nil
      }.flatten

      val program = Program(terminals.map(_._2))

      val program2 = program/*.transform(
        onValue = {
          case phi: SSA.Phi if phi.incoming.count(_._2 != phi) == 1 => phi.incoming.filter(_._2 != phi).head._2
        },
        onControl = {
          case r: SSA.Region if r.incoming.size == 1 => r.incoming.head
        }
      )*/


      val (printed, mapping) = Renderer.renderSSA(program2)
      println(printed)
      CodeGen(program2, mapping)
      ???
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