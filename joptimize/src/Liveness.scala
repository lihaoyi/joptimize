package joptimize

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{AbstractInsnNode, InsnList, InsnNode, VarInsnNode}

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
  def apply(sig: MethodSig,
            basicBlocks: Seq[InsnList],
            allTerminals: Seq[(Seq[LValue], AbstractInsnNode, Option[IType])]): InsnList = {
    println("="*20 +sig + "="*20)

    // Three states for a node:
    // - Not visited at all
    // - Visited, and delegated to a local
    // - Visited, but not delegated to a local
    val localsMap = mutable.LinkedHashMap.empty[Either[Int, AbstractInsnNode], Option[Int]]
    if (sig.name == "simpleBaz" && sig.static){
      import collection.JavaConverters._
      pprint.log(basicBlocks.map(_.iterator().asScala.toArray))
      pprint.log(allTerminals.map(_._2))
    }

    val terminalUpstream = allTerminals.map(x => (x._2, x._1)).toMap
    val (_, roots, downstreamEdges) = Util.breadthFirstAggregation[Either[LValue, AbstractInsnNode]](
      allTerminals.map(x => Right(x._2)).toSet ++ allTerminals.flatMap(_._1.map(Left(_))).toSet,
      {
        case Left(x) => x.upstream.flatten.map(Left[LValue, AbstractInsnNode](_))
        case Right(y) => terminalUpstream(y).map(Left[LValue, AbstractInsnNode](_))
      }
    )

    pprint.log(roots)
    pprint.log(downstreamEdges)
    for (root <- roots) toInsn(root)match{
      case Left(n) => localsMap(toInsn(root)) = Some(n)
      case _ =>
    }

    def toInsn(x: Either[LValue, AbstractInsnNode]) = x.left.flatMap(_.insn)

    val downstream = downstreamEdges.map{case (k, v) => (toInsn(k), toInsn(v))}.toSeq.groupBy(_._1).mapValues(_.map(_._2)).toMap
    val insnDownstream = downstream.map{case (k, v) => (k, v)}

    pprint.log(downstream)
    pprint.log(insnDownstream)
    val outputInsns = new InsnList
    def saveToLocal(insn: Either[Int, AbstractInsnNode]) = {
      pprint.log(insn -> localsMap.size)
      insnDownstream.get(insn) match{
        case Some(x) if x.size >= 2 => localsMap(insn) = Some(localsMap.size); localsMap(insn)
        case Some(x) => localsMap(insn) = None; None
        case None => None
      }
    }
    for((terminalValues, terminalInsn, terminalTypeOpt) <- allTerminals){
      println("=" * 10 + "WALKING TERMINAL " + terminalInsn + "=" * 10)
      generateBlockTerminalInsns(
        sig,
        terminalValues,
        saveToLocal = lv => saveToLocal(lv.insn),
        loadFromLocal = lv => lv.insn.fold(i => Some(Some(i)), c => localsMap.get(Right(c))),
        outputInsns
      )

      saveToLocal(Right(terminalInsn)).foreach{ i =>
        terminalTypeOpt.foreach{ terminalType =>
          outputInsns.add(new InsnNode(Opcodes.DUP))
          outputInsns.add(
            new VarInsnNode(
              terminalType.widen match{
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
      outputInsns.add(Util.clone(terminalInsn))
    }

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
                                 outputInsns: InsnList): Unit = {
    def rec(value: LValue): Unit = {


      loadFromLocal(value) match{
        // if the value is available in a local, just load it
        case Some(Some(i)) =>
          outputInsns.add(new VarInsnNode(
            value.tpe.widen match{
              case JType.Prim.I => Opcodes.ILOAD
              case JType.Prim.J => Opcodes.LSTORE
              case JType.Prim.F => Opcodes.FLOAD
              case JType.Prim.D => Opcodes.DLOAD
              case _ => Opcodes.ALOAD
            },
            i
          ))
        case Some(None) =>

        case None =>
          // Otherwise, compute the arguments necessary for this value
          for(valueList <- value.upstream) valueList.foreach(rec)

          // compute this value using it's instruction

          value.insn.foreach(i => outputInsns.add(Util.clone(i)))

          // and save it to a local if necessary
          saveToLocal(value).foreach{ i =>
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

    terminalValues.foreach(rec)
  }
}

