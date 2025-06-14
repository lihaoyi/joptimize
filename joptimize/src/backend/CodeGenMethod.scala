package joptimize.backend

import joptimize.Logger
import joptimize.analyzer.Renderer
import joptimize.model.JType
import joptimize.model.{MethodBody, SSA}
import org.objectweb.asm.Handle
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._

import scala.collection.mutable

/**
  * Converts the hybrid dataflow/controlflow graph generated by the Walker
  * back into executable bytecode, without all the unreachable, unused or
  * constant-folded instructions
  */
class CodeGenMethod(
  log: Logger.InferredMethod,
  isInterface: JType.Cls => Option[Boolean],
  methodBody: MethodBody,
  nodesToBlocks: mutable.LinkedHashMap[SSA.ValOrState, SSA.Block],
  cfg: collection.Seq[(SSA.Control, SSA.Control)]
) {
  val blocksToNodes = nodesToBlocks.groupBy(_._2).map { case (k, v) => (k, v.keys.toSeq) }
  val sortedBlocks: collection.Set[SSA.Block] =
    CodeGenMethod.sortControlFlowGraph(cfg).collect { case b: SSA.Block => b }
  val blockIndices = sortedBlocks.zipWithIndex.toMap
  val labels = sortedBlocks.map(_ -> new LabelNode()).toMap
  val sortedAllNodes = sortedBlocks.flatMap(b => blocksToNodes(b).map(_ -> b))
  val allProps = sortedAllNodes.collect {
    case (k: SSA.Val, b) => (k, CodeGenMethod.getValProperties(k, b, nodesToBlocks))
  }.toMap

  val (savedLocalNumbers, seen) = CodeGenMethod.computeSavedLocalNumbers(
    methodBody,
    sortedAllNodes,
    allProps
  )

  def apply() = log.block {
    // Generates serialized bytecode on a block-by-block basis, walking the blocks in
    // depth-first order, starting from the terminal nodes of each block and walking
    // upstream in depth-first post-order. This includes:
    //
    // - Values computed in that block but used unchanged in other blocks, saved
    //   to a local for other blocks to access
    //
    // - Operations that are required to produce the final state of the block. If
    //   these return an unused value, the value is POPed and discarded
    //
    // - If the block is followed by a jump, the value needed for that jump is left
    //   on the stack for the jump to use
    //
    // - If the block is followed by a merge, values needed for those phis are
    //   accumulated on the stack, then assigned to their respective phi's local slot
    //
    // Each block starts ends with an empty operand stack, and any values that are
    // passed between blocks do so in local slots
    val insnsPerBlock = for (block <- sortedBlocks.toArray) yield {
//      log.pprint(block -> block.next)
      val (valsForNextControlInsns, nextStateNodes, suffixInsns) = block.next match {
        case next: SSA.Merge =>
          val (nextVals, phiStores) = next
            .phis
            .map { phi =>
              val value = phi.incoming.collect { case (k, v) if k == block => v }
              val flatted = value.flatMap(rec(_, fromVal = true, block))
              (flatted, new VarInsnNode(CodeGenMethod.saveOp(phi.jtype), savedLocalNumbers(phi)))
            }
            .unzip

          val nextStateNodes = next.incoming.collect { case (k, v) if k == block => v }

          (nextVals.flatten, nextStateNodes, phiStores.reverse)

        case next: SSA.Jump =>
          val nextVals = next.upstreamVals.flatMap(rec(_, fromVal = true, block))
          (nextVals, Seq(next.state), generateJumpBytecode(next))
      }

      val valsSavedForOtherBlockInsns = saveValueInsnsForOtherBlocks(block)

      val gotoFooterInsns = block.downstreamList.collect { case b: SSA.Control => b } match {
        case Seq(d: SSA.Block) if blockIndices(d) != blockIndices(block) + 1 =>
          Some(new JumpInsnNode(GOTO, labels(d)))
        case _ => None
      }

      val nextStateInsns = nextStateNodes.flatMap(rec(_, fromVal = false, block))

      Seq(labels(block)) ++
      valsForNextControlInsns ++
      valsSavedForOtherBlockInsns ++
      nextStateInsns ++
      suffixInsns ++
      gotoFooterInsns
    }

    val finalInsns = new InsnList()
    for (blockInsns <- insnsPerBlock) {
      blockInsns.foreach(finalInsns.add)
    }

    log.println("================ OUTPUT BYTECODE ================")
    log(Renderer.renderBlockCode(insnsPerBlock, finalInsns))

    log.pprint(savedLocalNumbers)
    finalInsns
  }

  def saveValueInsnsForOtherBlocks(block: SSA.Block) = {
    blocksToNodes(block).collect {
      case n: SSA.Val if n.upstream.nonEmpty && !n.isInstanceOf[SSA.Phi] =>
        val props = allProps(n)
        if (props.valUses == 0 && !props.stateUse && props.otherBlockUse) {
          recVal(n, block) ++
          Seq(new VarInsnNode(CodeGenMethod.saveOp(n.jtype), savedLocalNumbers(n)))
        } else Nil
    }.flatten
  }

  def generateJumpBytecode(ssa: SSA.Jump): Seq[AbstractInsnNode] = {

    def computeSwitchLabels(n: SSA.Switch) = (
      this.labels(n.downstreamList.collect { case t: SSA.Default => t }.head),
      n.downstreamList.collect { case t: SSA.Case => t }.map(this.labels)
    )

    ssa match {
      case n: SSA.Branch =>
        val destination = n.downstreamList.collect { case t: SSA.False => t }.head
        val goto =
          if (blockIndices(destination) == blockIndices(ssa.block) + 1) None
          else Some(new JumpInsnNode(GOTO, labels(destination)))

        val jump = Seq(
          new JumpInsnNode(
            n.opcode.i,
            labels(n.downstreamList.collect { case t: SSA.True => t }.head)
          )
        )
        jump ++ goto

      case n: SSA.ReturnVal => Seq(new InsnNode(CodeGenMethod.returnOp(n.src.jtype)))

      case n: SSA.Return => Seq(new InsnNode(RETURN))
      case n: SSA.AThrow => Seq(new InsnNode(ATHROW))
      case n: SSA.TableSwitch =>
        val (default, labels) = computeSwitchLabels(n)
        Seq(new TableSwitchInsnNode(n.min, n.max, default, labels.toArray: _*))
      case n: SSA.LookupSwitch =>
        val (default, labels) = computeSwitchLabels(n)
        Seq(new LookupSwitchInsnNode(default, n.keys.toArray, labels.toArray))
    }
  }

  def rec(ssa: SSA.ValOrState, fromVal: Boolean, currentBlock: SSA.Block): Seq[AbstractInsnNode] =
    ssa match {

      case ssa: SSA.State =>
        ssa
          .upstream
          .collect { case s: SSA.ValOrState => s }
          .flatMap(rec(_, fromVal = false, currentBlock))
      case n: SSA.Val =>
        if (n.upstream.isEmpty && !n.isInstanceOf[SSA.Arg] && !n.isInstanceOf[SSA.Invoke]) recTrivialVal(n)
        else if (seen(n)) {
          if (fromVal) Seq(new VarInsnNode(CodeGenMethod.loadOp(n.jtype), savedLocalNumbers(n)))
          else Nil
        } else {
          val props = allProps(n)

          (props.valUses, props.stateUse, props.otherBlockUse, fromVal) match {

            case (0, true, false, false) =>
              recVal(n, currentBlock) ++
              (n.getSize match {
                case 0 => Nil
                case 1 => Seq(new InsnNode(POP))
                case 2 => Seq(new InsnNode(POP2))
              })

            case (0, true, true, false) =>
              recVal(n, currentBlock) ++
              Seq(new VarInsnNode(CodeGenMethod.saveOp(n.jtype), savedLocalNumbers(n)))

            case (1, _, false, true) => recVal(n, currentBlock)

            case (_, true, _, false) => Nil

            case (_, _, _, true) =>
              recVal(n, currentBlock) ++
              Seq(
                new InsnNode(if (n.getSize == 1) DUP else DUP2),
                new VarInsnNode(CodeGenMethod.saveOp(n.jtype), savedLocalNumbers(n))
              )

            case _ => ???
          }
        }
    }

  def recTrivialVal(ssa: SSA.Val): Seq[AbstractInsnNode] = ssa match {
    case n: SSA.ConstI =>
      Seq(n.value match {
        case -1 => new InsnNode(ICONST_M1)
        case 0 => new InsnNode(ICONST_0)
        case 1 => new InsnNode(ICONST_1)
        case 2 => new InsnNode(ICONST_2)
        case 3 => new InsnNode(ICONST_3)
        case 4 => new InsnNode(ICONST_4)
        case 5 => new InsnNode(ICONST_5)
        case _ =>
          if (-128 <= n.value && n.value < 127) new IntInsnNode(BIPUSH, n.value)
          else if (-32768 <= n.value && n.value <= 32767) new IntInsnNode(SIPUSH, n.value)
          else new LdcInsnNode(n.value)
      })
    case n: SSA.ConstJ =>
      Seq(n.value match {
        case 0 => new InsnNode(LCONST_0)
        case 1 => new InsnNode(LCONST_1)
        case _ => new LdcInsnNode(n.value)
      })
    case n: SSA.ConstF =>
      Seq(n.value match {
        case 0 => new InsnNode(FCONST_0)
        case 1 => new InsnNode(FCONST_1)
        case 2 => new InsnNode(FCONST_2)
        case _ => new LdcInsnNode(n.value)
      })
    case n: SSA.ConstD =>
      Seq(n.value match {
        case 0 => new InsnNode(DCONST_0)
        case 1 => new InsnNode(DCONST_1)
        case _ => new LdcInsnNode(n.value)
      })
    case n: SSA.ConstStr => Seq(new LdcInsnNode(n.value))
    case n: SSA.ConstNull => Seq(new InsnNode(ACONST_NULL))
    case n: SSA.ConstCls =>
      Seq(new LdcInsnNode(org.objectweb.asm.Type.getObjectType(n.value.name)))
  }

  def recVal(ssa: SSA.Val, currentBlock: SSA.Block): Seq[AbstractInsnNode] = {
    seen.add(ssa)
    def upstreams =
      ssa
        .upstream
        .collect { case v: SSA.ValOrState => v }
        .flatMap(rec(_, fromVal = true, currentBlock))

    ssa match {
      case n: SSA.BinOp => upstreams ++ Seq(new InsnNode(n.opcode.i))
      case n: SSA.UnaOp => upstreams ++ Seq(new InsnNode(n.opcode.i))
      case n: SSA.CheckCast => upstreams ++ Seq(new TypeInsnNode(CHECKCAST, n.desc.name))
      case n: SSA.ArrayLength => upstreams ++ Seq(new InsnNode(ARRAYLENGTH))
      case n: SSA.InstanceOf => upstreams ++ Seq(new TypeInsnNode(INSTANCEOF, n.desc.name))
      case n: SSA.InvokeStatic =>
        upstreams ++
        Seq(new MethodInsnNode(INVOKESTATIC, n.cls.name, n.name, n.desc.render, n.interface)) ++
        (if (n.desc.ret == JType.Bottom && n.downstreamList.count(!_.isInstanceOf[SSA.State]) == 0)
           Seq(new InsnNode(POP))
         else Nil)
      case n: SSA.InvokeSpecial =>
        upstreams ++
        Seq(new MethodInsnNode(INVOKESPECIAL, n.cls.name, n.name, n.desc.render, n.interface)) ++
        (if (n.desc.ret == JType.Bottom && n.downstreamList.count(!_.isInstanceOf[SSA.State]) == 0)
           Seq(new InsnNode(POP))
         else Nil)
      case n: SSA.InvokeVirtual =>
        val opcode =
          if (isInterface(n.cls).getOrElse(n.interface)) INVOKEINTERFACE else INVOKEVIRTUAL
        upstreams ++
        Seq(new MethodInsnNode(opcode, n.cls.name, n.name, n.desc.render)) ++
        (if (n.desc.ret == JType.Bottom && n.downstreamList.count(!_.isInstanceOf[SSA.State]) == 0)
           Seq(new InsnNode(POP))
         else Nil)
      case n: SSA.InvokeDynamic =>
        upstreams ++ Seq(
          new InvokeDynamicInsnNode(
            n.name,
            n.desc.render,
            new Handle(
              n.bootstrap.tag,
              n.bootstrap.owner.name,
              n.bootstrap.name,
              n.bootstrap.desc.render
            ),
            n.bootstrapArgs.map(SSA.InvokeDynamic.argToAny): _*
          )
        )
      case n: SSA.New =>
        Seq(new TypeInsnNode(NEW, n.cls.name), new InsnNode(DUP)) ++
        upstreams ++
        Seq(new MethodInsnNode(INVOKESPECIAL, n.cls.name, "<init>", n.desc.render))

      case n: SSA.NewArray => upstreams ++ Seq(CodeGenMethod.newArrayOp(n.typeRef))
      case n: SSA.MultiANewArray =>
        upstreams ++ Seq(new MultiANewArrayInsnNode(n.desc.name, n.dims.length))
      case n: SSA.PutStatic =>
        upstreams ++ Seq(new FieldInsnNode(PUTSTATIC, n.cls.name, n.name, n.desc.internalName))
      case n: SSA.GetStatic =>
        upstreams ++ Seq(new FieldInsnNode(GETSTATIC, n.cls.name, n.name, n.desc.internalName))
      case n: SSA.PutField =>
        upstreams ++ Seq(new FieldInsnNode(PUTFIELD, n.owner.name, n.name, n.desc.internalName))
      case n: SSA.GetField =>
        upstreams ++ Seq(new FieldInsnNode(GETFIELD, n.owner.name, n.name, n.desc.internalName))
      case n: SSA.PutArray => upstreams ++ Seq(new InsnNode(CodeGenMethod.arrayStoreOp(n.src)))
      case n: SSA.GetArray => upstreams ++ Seq(new InsnNode(CodeGenMethod.arrayLoadOp(n.tpe)))
      case n: SSA.MonitorEnter => ???
      case n: SSA.MonitorExit => ???
    }

  }

}
object CodeGenMethod {
  case class ValDownstreamProps(valUses: Int, stateUse: Boolean, otherBlockUse: Boolean)
  def getValProperties(
    n: SSA.Val,
    currentBlock: SSA.Block,
    nodesToBlocks: collection.Map[SSA.ValOrState, SSA.Block]
  ) = {
    var downstreamValsOrJumps = 0
    var downstreamState = false
    var downstreamOtherBlock = false
    n.downstreamList.foreach {
      case down: SSA.Phi =>
        if (down.incoming.find(_._2 == n).get._1 == currentBlock) downstreamValsOrJumps += 1
        else downstreamOtherBlock = true
      case down: SSA.Val =>
        if (nodesToBlocks(down) == currentBlock) downstreamValsOrJumps += 1
        else downstreamOtherBlock = true
      case down: SSA.State => downstreamState = true
      case down: SSA.Jump =>
        if (down.block == currentBlock) downstreamValsOrJumps += 1
        else downstreamOtherBlock = true

      case _ => // do nothing
    }
    ValDownstreamProps(downstreamValsOrJumps, downstreamState, downstreamOtherBlock)
  }
  def computeSavedLocalNumbers(
    methodBody: MethodBody,
    sortedAllNodes: collection.Set[(SSA.ValOrState, SSA.Block)],
    allProps: Map[SSA.Val, ValDownstreamProps]
  ): (collection.Map[SSA.Val, Int], mutable.LinkedHashSet[SSA.Val]) = {

    val seen = mutable.LinkedHashSet.empty[SSA.Val]
    val savedLocalNumbers = mutable.LinkedHashMap.empty[SSA.Val, Int]
    var currentLocalsSize = 0

    def saveLocal(n: SSA.Val) = {
      val localEntry = currentLocalsSize
      savedLocalNumbers(n) = currentLocalsSize
      currentLocalsSize += n.getSize
      localEntry
    }

    for (arg <- methodBody.args) {
      saveLocal(arg)
      seen.add(arg)
    }

    sortedAllNodes.foreach {
      case (p: SSA.Phi, _) =>
        seen.add(p)
        saveLocal(p)
      case (n: SSA.Val, _) if n.upstream.nonEmpty =>
        allProps(n) match {
          case ValDownstreamProps(0, false, true) => saveLocal(n)
          case ValDownstreamProps(0, true, true) => saveLocal(n)
          case ValDownstreamProps(1, true, _) => saveLocal(n)
          case ValDownstreamProps(1, _, true) => saveLocal(n)
          case ValDownstreamProps(downs, _, _) if downs > 1 => saveLocal(n)
          case _ => // do nothing
        }
      case _ => // do nothing
    }
    (savedLocalNumbers, seen)
  }

  def sortControlFlowGraph(cfg: collection.Seq[(SSA.Control, SSA.Control)]) = {
    val collection.Seq(startBlock) = cfg.collect { case (s: SSA.Start, _) => s }
    val successor = cfg.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    val sortedControls = mutable.LinkedHashSet.empty[SSA.Control]

    def sortBlocks(block: SSA.Control): Unit = if (!sortedControls(block)) {
      sortedControls.add(block)
      for (s <- successor.getOrElse(block, Nil)) sortBlocks(s)
    }

    sortBlocks(startBlock)
    sortedControls
  }

  def newArrayOp(typeRef: JType) = {
    typeRef match {
      case JType.Prim.Z => new IntInsnNode(NEWARRAY, T_BOOLEAN)
      case JType.Prim.C => new IntInsnNode(NEWARRAY, T_CHAR)
      case JType.Prim.B => new IntInsnNode(NEWARRAY, T_BYTE)
      case JType.Prim.S => new IntInsnNode(NEWARRAY, T_SHORT)
      case JType.Prim.I => new IntInsnNode(NEWARRAY, T_INT)
      case JType.Prim.F => new IntInsnNode(NEWARRAY, T_FLOAT)
      case JType.Prim.D => new IntInsnNode(NEWARRAY, T_DOUBLE)
      case JType.Prim.J => new IntInsnNode(NEWARRAY, T_LONG)
      case t => new TypeInsnNode(ANEWARRAY, t.name)
    }
  }

  def arrayStoreOp(src: SSA.Val) = {
    src.jtype match {
      case JType.Prim.Z => IASTORE
      case JType.Prim.C => CASTORE
      case JType.Prim.B => BASTORE
      case JType.Prim.S => SASTORE
      case JType.Prim.I => IASTORE
      case JType.Prim.F => FASTORE
      case JType.Prim.D => DASTORE
      case JType.Prim.J => LASTORE
      case t => AASTORE
    }
  }

  def arrayLoadOp(tpe: JType) = {
    tpe match {
      case JType.Prim.Z => IALOAD
      case JType.Prim.C => CALOAD
      case JType.Prim.B => BALOAD
      case JType.Prim.S => SALOAD
      case JType.Prim.I => IALOAD
      case JType.Prim.F => FALOAD
      case JType.Prim.D => DALOAD
      case JType.Prim.J => LALOAD
      case t => AALOAD
    }
  }

  def loadOp(jtype: JType) = {
    jtype match {
      case JType.Prim.I | JType.Prim.S | JType.Prim.Z | JType.Prim.B | JType.Prim.C => ILOAD
      case JType.Prim.J => LLOAD
      case JType.Prim.F => FLOAD
      case JType.Prim.D => DLOAD
      case _ => ALOAD
    }
  }

  def saveOp(jtype: JType) = {
    jtype match {
      case JType.Prim.I | JType.Prim.S | JType.Prim.Z | JType.Prim.B | JType.Prim.C => ISTORE
      case JType.Prim.J => LSTORE
      case JType.Prim.F => FSTORE
      case JType.Prim.D => DSTORE
      case _ => ASTORE
    }
  }

  def returnOp(jtype: JType) = {
    jtype match {
      case JType.Prim.I | JType.Prim.S | JType.Prim.Z | JType.Prim.B | JType.Prim.C => IRETURN
      case JType.Prim.J => LRETURN
      case JType.Prim.F => FRETURN
      case JType.Prim.D => DRETURN
      case _ => ARETURN
    }
  }
}
