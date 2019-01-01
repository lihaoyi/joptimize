package joptimize


import java.util
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
        case ((RETURN, insn), i) => (insn, SSA.Return(findStartRegion(insn)), i) :: Nil

        case ((IRETURN | LRETURN | FRETURN | DRETURN | ARETURN, insn), i) =>
          (insn, SSA.ReturnVal(findStartRegion(insn), frameTop(i, 0)), i) :: Nil

        case ((ATHROW, insn), i) => (insn, SSA.AThrow(frameTop(i, 0)), i) :: Nil

        case ((GOTO, insn: JumpInsnNode), i) =>
          regionMerges(regionStarts(insn.label)) =
            regionMerges.getOrElse(regionStarts(insn.label), Set.empty) + findStartRegion(insn)
          Nil

        case ((IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE, insn: JumpInsnNode), i) =>
          val n = SSA.UnaryBranch(findStartRegion(insn), frameTop(i, 0), SSA.UnaryBranch.lookup(insn.getOpcode))
          regionMerges(regionStarts(insn.label)) =
            regionMerges.getOrElse(regionStarts(insn.label), Set.empty) + SSA.True(n)
          regionMerges(regionStarts(insn.getNext)) =
            regionMerges.getOrElse(regionStarts(insn.getNext), Set.empty) + SSA.False(n)

          (insn, n, i) :: Nil

        case ((IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE, insn: JumpInsnNode), i) =>
          val n = SSA.BinBranch(findStartRegion(insn), frameTop(i, 0), frameTop(i, 1), SSA.BinBranch.lookup(insn.getOpcode))
          regionMerges(regionStarts(insn.label)) =
            regionMerges.getOrElse(regionStarts(insn.label), Set.empty) + SSA.True(n)

          regionMerges(regionStarts(insn.getNext)) =
            regionMerges.getOrElse(regionStarts(insn.getNext), Set.empty) + SSA.False(n)
          (insn, n, i) :: Nil

        case ((_, insn), i) if Option(insn.getNext).exists(regionStarts.contains) =>
          regionMerges(regionStarts(insn.getNext)) =
            regionMerges.getOrElse(regionStarts(insn.getNext), Set.empty) + findStartRegion(insn)
          Nil
      }.flatten

      val regionMerges2 = regionStarts.map{case (k, v) => (v, regionMerges(v))}

      val phiMerges = phiMerges0.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap

      val (compoundRegions, simpleRegions0) = regionMerges2.partition(_._2.size != 1)
      val simpleRegions = simpleRegions0.flatMap{case (k, v) => v.map((k, _))}

      val (terminals2, phiMerges2, compoundRegions2) = collapseSimpleRegions(
        terminals.map(_._2),
        phiMerges,
        simpleRegions,
        compoundRegions
      )

      println(regionMerges2.keysIterator.map(r => Renderer.renderInsns(mn.instructions, regionStarts.map(_.swap).apply(r)).toString).toList)
      println(Renderer.renderSSA(terminals2, compoundRegions2, phiMerges2))

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
  def collapseSimpleRegions(allTerminals: Seq[SSA],
                            phiMerges:  Map[SSA.Phi, Set[SSA]],
                            simpleRegions: mutable.LinkedHashMap[SSA.Region, SSA.Control],
                            compoundRegions: mutable.LinkedHashMap[SSA.Region, Set[SSA.Control]])
  : (Seq[SSA], Map[SSA.Phi, Set[SSA]], mutable.LinkedHashMap[SSA.Region, Set[SSA.Control]]) = {
    pprint.log(simpleRegions)
    val current = new util.IdentityHashMap[SSA, SSA]()
    def rec(x: SSA): SSA = {
      if (current.containsKey(x)) current.get(x)
      else {
        val res: SSA = x match{
          case phi: SSA.Phi => phi
          case SSA.Arg(index, typeSize) => SSA.Arg(index, typeSize)
          case SSA.BinOp(a, b, opcode) => SSA.BinOp(rec(a), rec(b), opcode)
          case SSA.UnaryOp(a, opcode) => SSA.UnaryOp(rec(a), opcode)
          case SSA.UnaryBranch(ctrl, a, opcode) => SSA.UnaryBranch(rec2(ctrl), rec(a), opcode)
          case SSA.BinBranch(ctrl, a, b, opcode) => SSA.BinBranch(rec2(ctrl), rec(a), rec(b), opcode)
          case SSA.ReturnVal(ctrl, a) => SSA.ReturnVal(rec2(ctrl), rec(a))
          case SSA.Return(ctrl) => SSA.Return(rec2(ctrl))
          case SSA.AThrow(src) => SSA.AThrow(rec(src))
          case SSA.TableSwitch(src, min, max) => SSA.TableSwitch(rec(src), min, max)
          case SSA.LookupSwitch(src, keys) => SSA.LookupSwitch(rec(src), keys)
          case SSA.CheckCast(src, desc) => SSA.CheckCast(rec(src), desc)
          case SSA.ArrayLength(src) => SSA.ArrayLength(rec(src))
          case SSA.InstanceOf(src, desc) => SSA.InstanceOf(rec(src), desc)
          case SSA.PushI(value) => SSA.PushI(value)
          case SSA.PushJ(value) => SSA.PushJ(value)
          case SSA.PushF(value) => SSA.PushF(value)
          case SSA.PushD(value) => SSA.PushD(value)
          case SSA.PushS(value) => SSA.PushS(value)
          case SSA.PushNull() => SSA.PushNull()
          case SSA.PushCls(value) => SSA.PushCls(value)
          case SSA.InvokeStatic(srcs, cls, name, desc) => SSA.InvokeStatic(srcs.map(rec), cls, name, desc)
          case SSA.InvokeSpecial(srcs, cls, name, desc) => SSA.InvokeSpecial(srcs.map(rec), cls, name, desc)
          case SSA.InvokeVirtual(srcs, cls, name, desc) => SSA.InvokeVirtual(srcs.map(rec), cls, name, desc)
          case SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs) => SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs)
          case SSA.NewArray(src, typeRef) => SSA.NewArray(rec(src), typeRef)
          case SSA.MultiANewArray(desc, dims) => SSA.MultiANewArray(desc, dims)
          case SSA.PutStatic(src, cls, name, desc) => SSA.PutStatic(rec(src), cls, name, desc)
          case SSA.GetStatic(cls, name, desc) => SSA.GetStatic(cls, name, desc)
          case SSA.PutField(src, obj, owner, name, desc) => SSA.PutField(rec(src), rec(obj), owner, name, desc)
          case SSA.GetField(obj, owner, name, desc) => SSA.GetField(rec(obj), owner, name, desc)
          case SSA.PutArray(src, indexSrc, array) => SSA.PutArray(rec(src), rec(indexSrc), rec(array))
          case SSA.GetArray(indexSrc, array, typeSize) => SSA.GetArray(rec(indexSrc), rec(array), typeSize)
          case SSA.MonitorEnter(indexSrc) => SSA.MonitorEnter(indexSrc)
          case SSA.MonitorExit(indexSrc) => SSA.MonitorExit(indexSrc)
        }
        current.put(x, res)
        res
      }
    }
    def rec2(x: SSA.Control): SSA.Control = {
      x match{
        case r: SSA.Region =>
          simpleRegions.get(r) match{
            case None => r
            case Some(simple) => rec2(simple)
          }
        case SSA.True(inner) => SSA.True(rec(inner))
        case SSA.False(inner) => SSA.False(rec(inner))
      }
    }
    (
      allTerminals.map(rec),
      phiMerges.map{case (k, v) => (k, v.map(rec))},
      compoundRegions.map{case (k, v) => (k, v.map(rec2))}
    )
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