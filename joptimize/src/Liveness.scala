package joptimize

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import collection.JavaConverters._

/**
  * Rudimentary Method-level liveness analysis: we report on which arguments a
  * method ends up using, and we remove outgoing pure method calls in segments
  * of the dataflow graph that do not contribute to the terminal instructions
  * in this method. In the latter case, we simply replace the bytecodes with
  * the appropriate number of POPs and a *CONST with the default value of that
  * instruction, which should not cause any issue since any instructions
  * downstream will also get similarly stubbed out
  */
object Liveness {
  def apply(insns: InsnList,
            allTerminals: Seq[Terminal]): (InsnList, Set[Int]) = {

    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[Either[LValue, Terminal]](allTerminals.map(Right(_)).toSet){
        case Left(x) => (x.upstream ++ x.merges).map(Left[LValue, Terminal])
        case Right(y) => y.inputs.map(Left[LValue, Terminal])
      }

    val allLiveMethodInsns = allVertices
      .collect{case Left(lv) => lv.insn}
      .collect{case Right(mn: MethodInsnNode) => mn}

    val liveArgumentIndices = roots
      .collect{case Left(lv) => lv.insn}
      .collect{case Left(n) => n}

    for (insn <- insns.iterator().asScala){
      insn match{
        case mn: MethodInsnNode if !allLiveMethodInsns.contains(mn) && mn.name != "<init>" =>
          val desc = Desc.read(mn.desc)
          val argOutCount = desc.args.length + (if (mn.getOpcode == Opcodes.INVOKESTATIC) 0 else 1)
          for(_ <- 0 until argOutCount) insns.insertBefore(mn, new InsnNode(Opcodes.POP))
          insns.insertBefore(mn, new InsnNode(desc.ret.widen match{
            case JType.Prim.I | JType.Prim.S | JType.Prim.C | JType.Prim.B | JType.Prim.Z =>
              Opcodes.ICONST_0
            case JType.Prim.J => Opcodes.LCONST_0
            case JType.Prim.F => Opcodes.FCONST_0
            case JType.Prim.D => Opcodes.DCONST_0
            case _ => Opcodes.ACONST_NULL
          }))
          insns.remove(mn)

        case _ => //do nothing
      }
    }

    (insns, liveArgumentIndices)
  }
}


/**
  * Terminal instructions have an instruction, its inputs, and an optional
  * return type.
  *
  * The terminal instruction's return value is not kept track of specially; we
  * only need to ensure that the terminal instruction is called with the input
  * in the right order, not that anything in particular happens to the return
  * value. Anyone who needs the return value will depend on it like any other
  * LValue in the dataflow graph
  *
  * Note that we cannot model terminals as their returned LValue, because some
  * terminals such as RETURN or void method INVOKEs return nothing.
  */
case class Terminal(insn: AbstractInsnNode, inputs: Seq[LValue], ret: Option[IType])

