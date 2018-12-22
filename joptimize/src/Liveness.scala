package joptimize

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import collection.JavaConverters._

import scala.collection.mutable

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

  def apply(maxLocals: Int,
            maxStack: Int,
            sig: MethodSig,
            basicBlocks: Seq[InsnList],
            allTerminals: Seq[Terminal],
            initialArgumentLValues: Seq[LValue]): InsnList = {

    // Graph of jumps or transitions between basic blocks; will have cycles in
    // the case of loops or similar!
    val blockGraphEdges = for{
      (b1, i1) <- basicBlocks.zipWithIndex
      (b2, i2) <- basicBlocks.zipWithIndex
      if ((b1.getLast, b2.getFirst) match{
        case (j: JumpInsnNode, l: LabelNode) if j.label == l => true
        case (lhs, _)
          if i2 == i1 + 1
          && lhs.getOpcode != Opcodes.GOTO
          && lhs.getOpcode != Opcodes.RETURN
          && lhs.getOpcode != Opcodes.IRETURN
          && lhs.getOpcode != Opcodes.FRETURN
          && lhs.getOpcode != Opcodes.LRETURN
          && lhs.getOpcode != Opcodes.DRETURN
          && lhs.getOpcode != Opcodes.ARETURN
          && lhs.getOpcode != Opcodes.ATHROW =>
          true
        case _ => false
      })
    } yield (i1, i2)

    val blockGraphDownstream = blockGraphEdges.groupBy(_._1).mapValues(_.map(_._2)).toMap

    val blockGraphUpstream = blockGraphEdges.groupBy(_._2).mapValues(_.map(_._1)).toMap

    val insnToBlockLookup = basicBlocks
      .zipWithIndex
      .flatMap{case (bb, i) => bb.iterator().asScala.map(_ -> i)}
      .toMap

    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[Either[LValue, Terminal]](allTerminals.map(Right(_)).toSet){
        case Left(x) => x.upstream.flatten.map(Left[LValue, Terminal])
        case Right(y) => y.inputs.map(Left[LValue, Terminal])
      }

    val allLValues = allVertices.collect{case Left(lv) => lv}

    // Left means it's a method arg, Right means it's a block index
    val lvalueToBlock = allLValues.map{lv => lv -> lv.insn.map(insnToBlockLookup)}.toMap

    // Three states for a node:
    // - Not visited at all
    // - Visited, and delegated to a local
    // - Visited, but not delegated to a local
    val localsMap = mutable.LinkedHashMap.empty[LValue, Option[Int]]
    for((lv, i) <- initialArgumentLValues.zipWithIndex)localsMap(lv) = Some(i)

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

    val terminalInsns = allTerminals.map(_.insn).toSet
    val insnsToTerminals = allTerminals.map(t => t.insn -> t).toMap

    val terminalBlocks = allTerminals.map(t => insnToBlockLookup(t.insn)).distinct
    val seenBlockIndices = mutable.Set.empty[Int]

    // Walk over every basic block in arbitrary order (cannot do topological
    // due to presence of loops), as long as they are reachable from one of the
    // blocks containing terminal instructions
    def recurseBlock(nextBlockIndex: Int): Frame[LValue] = {
      if (!seenBlockIndices(nextBlockIndex)) ???
      else {
        seenBlockIndices.add(nextBlockIndex)
        val startFrame = blockGraphUpstream(nextBlockIndex).map(recurseBlock) match{
          case Nil =>
            Frame.initial[LValue](
              maxLocals, maxStack,
              initialArgumentLValues,
              new LValue(JType.Null, Left(-1), mutable.Buffer())
            )
          case Seq(single) => single
          case multiple =>
            // somehow merge the disparate frames together; probably need to
            // shuffle things around on the stack or in locals
            ???
        }

        val blockInsns = walkBasicBlock(
          blockTerminals = basicBlocks(nextBlockIndex)
            .iterator()
            .asScala
            .filter(terminalInsns)
            .toSeq
            .map(insnsToTerminals),

          // We already know all the LValues that need to be computed in this
          // method as a whole, so we can trivially filter for the LValues that
          // need to be computed by instructions in this particular basic block
          downstreamLValues = allLValues
            .filter(_.insn.exists(basicBlocks(nextBlockIndex).contains))
        )

        blockInsns.iterator().asScala.foldLeft(startFrame)(_.execute(_, ???))
      }
    }

    terminalBlocks.foreach(recurseBlock)

    outputInsns
  }

  /**
    * Each basic block is walked starting from its terminals that need to
    * be run (in order) as well as the LValues that downstream basic blocks
    * require (unordered). We assume the set of necessary LValues is precomputed
    *
    * It then returns its own instruction list.
    */
  def walkBasicBlock(blockTerminals: Seq[Terminal],
                     downstreamLValues: Set[LValue]): InsnList = {

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

