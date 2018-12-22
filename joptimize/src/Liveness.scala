package joptimize

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import collection.JavaConverters._

import scala.collection.mutable

/*
- Core considerations:
    - Returned values must be computed with the same input
    - Side effects (both reads & writes!) must happen in the same order
        - This includes side effects happening in loops, or after conditionals

Stack/Local bytecode -> Dataflow graph -> Stack/Local bytecode

- Capture dataflow graph of `LValue`s depending on each other, in
  `Interpreter[LValue]`

- Convert flat list of instructions into controlflow graph of basic blocks

- Within each basic block, each side effecting instruction must be called
  and in the same original order with the same arguments

- Traverse the dataflow graph upstream from all terminal instructions to
  find the set of live `LValue`s:

    - *RETURN VALUE
    - Method calls to non-pure methods (ARGS...)
    - PUTSTATIC VALUE
    - PUTFIELD VALUE, *ASTORE VALUE for escaping objects only
    - RETURN

- Re-generate each basic block, in order of terminal instructions, in order
  to ensure each terminal instruction is called with the correct LValues,
  and the necessary LValues are left on the stack/locals for downstream
  basic blocks.
    - Walk dataflow graph upstream from that terminal instruction and find its
      transitive closure
        - For terminal instructions which do not require input, they can simply
          be emitted immediately!

    - Find expressions: segments of the subgraph which are tree-shaped:
      all nodes in the graph have only one downstream use case, except the
      terminal node which may have one or more.

    - Every expression can be evaluated purely on the stack, loading and
      evaluating sub-expressions smallest-to-largest starting from the left-most
      smallest expression

    - Evaluation between expressions cannot be evaluated purely on the stack,
      and must use either local variables, or DUP bytecodes

- Any un-used input arguments are removed from the method signature, and that
  change propagated to the caller method's callsite.
*/
object Liveness {
  def tabulateLineNumbers(insns: Seq[AbstractInsnNode]) = {
    val insnToLineNumber = mutable.Map.empty[AbstractInsnNode, Int]
    var currentLineNumber = -1
    val labelToLineNumber = insns.collect{ case l: LineNumberNode => l.start -> l.line}.toMap
    insns.foreach{
      case l: LabelNode if labelToLineNumber.contains(l) =>
        currentLineNumber = labelToLineNumber(l)
        insnToLineNumber(l) = currentLineNumber
      case n => insnToLineNumber(n) = currentLineNumber
    }

    insnToLineNumber.toMap
  }

  def apply(sig: MethodSig,
            basicBlocks: Seq[InsnList],
            allTerminals: Seq[Terminal],
            initialArgumentLValues: Seq[LValue]): InsnList = {

    for(bb <- basicBlocks){


      pprint.log(bb.iterator().asScala.toSeq.map(Util.prettyprint).mkString("\n"))
    }

    // Three states for a node:
    // - Not visited at all
    // - Visited, and delegated to a local
    // - Visited, but not delegated to a local
    val localsMap = mutable.LinkedHashMap.empty[LValue, Option[Int]]
    for((lv, i) <- initialArgumentLValues.zipWithIndex)localsMap(lv) = Some(i)

    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[Either[LValue, Terminal]](allTerminals.map(Right(_)).toSet){
        case Left(x) => x.upstream.flatten.map(Left[LValue, Terminal])
        case Right(y) => y.inputs.map(Left[LValue, Terminal])
      }

    val jumpTargets = basicBlocks.flatMap(_.iterator().asScala.toSeq).flatMap{
      case j: JumpInsnNode => Seq(j.label)
      case j: TableSwitchInsnNode => Option(j.dflt).toSeq ++ j.labels.asScala
      case j: LookupSwitchInsnNode => Option(j.dflt).toSeq ++ j.labels.asScala
      case _ => Nil
    }.toSet

    // Downstream edges from an LValue to either an LValue or a terminal instruction.
    val downstream = downstreamEdges
      .groupBy(_._1.left.get)
      .mapValues(_.map(_._2))
      .toMap

    val outputInsns = new InsnList

    def saveToLocal(lv: LValue) = localsMap.getOrElseUpdate(lv, {
      downstream.get(lv) match{
        case Some(x) if x.size >= 2 => localsMap(lv) = Some(localsMap.size); localsMap(lv)
        case Some(x) => localsMap(lv) = None; None
        case None => None
      }
    })

    val oldNewInsnMapping = mutable.Map.empty[AbstractInsnNode, AbstractInsnNode]

    for(terminal <- allTerminals){
      println("=" * 10 + "WALKING TERMINAL " + Util.prettyprint(terminal.insn).trim + "=" * 10)
      generateBlockTerminalInsns(
        sig,
        terminal.inputs,
        saveToLocal = saveToLocal,
        loadFromLocal = localsMap.get,
        outputInsns,
        oldNewInsnMapping
      )

//      saveToLocal(Right(terminal.insn)).foreach{ i =>
//        terminal.ret.foreach{ terminalType =>
//          outputInsns.add(new InsnNode(Opcodes.DUP))
//          outputInsns.add(
//            new VarInsnNode(
//              terminalType.widen match{
//                case JType.Prim.I => Opcodes.ISTORE
//                case JType.Prim.J => Opcodes.LSTORE
//                case JType.Prim.F => Opcodes.FSTORE
//                case JType.Prim.D => Opcodes.DSTORE
//                case _ => Opcodes.ASTORE
//              },
//              i
//            )
//          )
//        }
//      }
      val terminalInsn = Util.clone(terminal.insn, oldNewInsnMapping)
      outputInsns.add(terminalInsn)
    }

    oldNewInsnMapping.collect{ case (oldInsn: JumpInsnNode, newInsn: JumpInsnNode) =>
      newInsn.label = oldNewInsnMapping(oldInsn.label).asInstanceOf[LabelNode]
    }

    pprint.log(outputInsns.iterator().asScala.toSeq.map(Util.prettyprint))

    outputInsns
  }

  /**
    * Processes the upstream dependencies of this terminal node, returning a
    * list of instructions to compute those dependencies, as well as the
    * stack profile of the start of the basic block (if found)
    */
  def generateBlockTerminalInsns(sig: MethodSig,
                                 terminalValues: Seq[LValue],
                                 saveToLocal: LValue => Option[Int],
                                 loadFromLocal: LValue => Option[Option[Int]],
                                 outputInsns: InsnList,
                                 oldNewInsnMapping: mutable.Map[AbstractInsnNode, AbstractInsnNode]): Unit = {
    def rec(value: LValue, depth: Int): Unit = {
      println(fansi.Color.Cyan("REC " + "    " * depth + value.insn.map(Util.prettyprint(_).trim)))
      loadFromLocal(value) match{
        // if the value is available in a local, just load it
        case Some(Some(i)) =>
          pprint.log("A")
          val loadInsn = new VarInsnNode(
            value.tpe.widen match{
              case JType.Prim.I => Opcodes.ILOAD
              case JType.Prim.J => Opcodes.LLOAD
              case JType.Prim.F => Opcodes.FLOAD
              case JType.Prim.D => Opcodes.DLOAD
              case _ => Opcodes.ALOAD
            },
            i
          )
          outputInsns.add(loadInsn)
        case Some(None) => pprint.log("B")
        case None =>
          pprint.log("C")
          // Otherwise, compute the arguments necessary for this value
          for(valueList <- value.upstream) valueList.foreach(rec(_, depth + 1))

          // compute this value using it's instruction
          value.insn.foreach { i =>
            val newInsn = Util.clone(i, oldNewInsnMapping)
            if (!outputInsns.contains(newInsn)){
              outputInsns.add(newInsn)
            }
          }

          val stl = saveToLocal(value)
          // and save it to a local if necessary
          stl.foreach{ i =>
            outputInsns.add(new InsnNode(Opcodes.DUP))
            outputInsns.add(
              new VarInsnNode(
                value.tpe.widen match{
                  case JType.Prim.I => Opcodes.ISTORE
                  case JType.Prim.J => Opcodes.LSTORE
                  case JType.Prim.F => Opcodes.FSTORE
                  case JType.Prim.D => Opcodes.DSTORE
                  case _ => Opcodes.ASTORE
                },
                i
              )
            )
          }
      }
    }

    terminalValues.foreach(rec(_, 0))
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

