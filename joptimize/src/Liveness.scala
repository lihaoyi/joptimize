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
            allTerminals: Seq[Terminal],
            subCallArgLiveness: Map[AbstractInsnNode, scala.Seq[Boolean]]): Set[Int] = {

    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[Either[LValue, Terminal]](allTerminals.map(Right(_)).toSet){
        case Left(x) =>
          x.insn match{
            case Left(i) => (x.upstream ++ x.merges).map(Left[LValue, Terminal])
            case Right(insn) =>
              subCallArgLiveness.get(insn) match{
                case None => (x.upstream ++ x.merges).map(Left[LValue, Terminal])
                case Some(liveness) =>
                  (x.upstream.zip(liveness).collect{case (k, true) => k} ++ x.merges).map(Left[LValue, Terminal])
              }
          }
        case Right(y) =>
          subCallArgLiveness.get(y.insn) match{
            case None => y.inputs.map(Left[LValue, Terminal])
            case Some(liveness) =>
              y.inputs.zip(liveness).collect{case (k, true) => k}.map(Left[LValue, Terminal])
          }

      }

    val allLiveInsns = allVertices
      .collect{case Left(lv) => lv.insn}
      .collect{case Right(mn) => mn}

    val liveArgumentIndices = roots
      .collect{case Left(lv) => lv.insn}
      .collect{case Left(n) => n}

    for (insn <- insns.iterator().asScala){
      insn match{
        case current: MethodInsnNode =>
          if (!allLiveInsns.contains(current) && current.name != "<init>")stubOut(insns, current)

        case current: AbstractInsnNode =>
          if (current.getOpcode != -1
          && !allLiveInsns.contains(current)
          && Bytecode.stackEffect(current.getOpcode).push(current) == 1){
            stubOut(insns, current)
          }
      }
    }

    liveArgumentIndices
  }

  def stubOut(insns: InsnList, current: AbstractInsnNode) = {
    for (_ <- 0 until Bytecode.stackEffect(current.getOpcode).pop(current)) {
      insns.insertBefore(current, new InsnNode(Opcodes.POP))
    }
    insns.insertBefore(current,
      new InsnNode(
        Bytecode.stackEffect(current.getOpcode).nullType(current).get match {
          case JType.Prim.Z | JType.Prim.B | JType.Prim.C | JType.Prim.S | JType.Prim.I =>
            Opcodes.ICONST_0
          case JType.Prim.F => Opcodes.FCONST_0
          case JType.Prim.J => Opcodes.LCONST_0
          case JType.Prim.D => Opcodes.DCONST_0
          case _ => Opcodes.ACONST_NULL
        }
      )
    )
    insns.remove(current)
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

