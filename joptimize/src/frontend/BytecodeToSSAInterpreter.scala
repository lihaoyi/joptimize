package joptimize.frontend

import joptimize.model.{Desc, JType, SSA}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import org.objectweb.asm.tree._
import org.objectweb.asm.{Handle, Type}

import scala.collection.mutable

/**
  * Takes in java bytecode instructions one at a time and converts them to
  * [[SSA]] nodes.
  *
  * Takes in a [[Typer]] that it uses to perform type inference on each of the
  * generated SSA nodes; we do this to allow immediate constant folding if the
  * node's type is specific enough to be a concrete value.
  */
class BytecodeToSSAInterpreter(merges: mutable.LinkedHashSet[SSA.Phi],
                               findBlockStart: Int => SSA.Block,
                               findBlockDest: Int => Option[SSA.Merge],
                               argMapping: Map[Int, Int]) extends Interpreter[SSA.Val, SSA.State]{

  /**
    * ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5,
    * LCONST_0, LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0, DCONST_1, BIPUSH, SIPUSH, LDC, JSR,
    * NEW
    */
  def constOperation(insn: AbstractInsnNode): SSA.Val = {
    insn.getOpcode match {
      case ACONST_NULL => new SSA.ConstNull()
      case ICONST_M1 => new SSA.ConstI(-1)
      case ICONST_0 => new SSA.ConstI(0)
      case ICONST_1 => new SSA.ConstI(1)
      case ICONST_2 => new SSA.ConstI(2)
      case ICONST_3 => new SSA.ConstI(3)
      case ICONST_4 => new SSA.ConstI(4)
      case ICONST_5 => new SSA.ConstI(5)
      case LCONST_0 => new SSA.ConstJ(0)
      case LCONST_1 => new SSA.ConstJ(1)
      case FCONST_0 => new SSA.ConstF(0)
      case FCONST_1 => new SSA.ConstF(1)
      case FCONST_2 => new SSA.ConstF(2)
      case DCONST_0 => new SSA.ConstD(0)
      case DCONST_1 => new SSA.ConstD(1)
      case BIPUSH | SIPUSH => new SSA.ConstI(insn.asInstanceOf[IntInsnNode].operand)
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match {
          case i: java.lang.Integer => new SSA.ConstI(i)
          case i: java.lang.Long => new SSA.ConstJ(i)
          case f: java.lang.Float => new SSA.ConstF(f)
          case d: java.lang.Double => new SSA.ConstD(d)
          case s: java.lang.String => new SSA.ConstStr(s)
          case value: org.objectweb.asm.Type =>
            value.getSort match {
              case OBJECT | ARRAY => new SSA.ConstCls(JType.Cls(value.getClassName))
              case METHOD => ???
            }
          case _: Handle => ???
        }
      case JSR => ???
      case NEW => new SSA.New(insn.asInstanceOf[TypeInsnNode].desc)
    }
  }
  /**
    * GETSTATIC
    */
  def getStaticOperation(insn: AbstractInsnNode, state: SSA.State): (SSA.Val, SSA.State) = {
    val insn2 = insn.asInstanceOf[FieldInsnNode]
    val op = new SSA.GetStatic(state, JType.Cls(insn2.owner), insn2.name, JType.read(insn2.desc))
    (op, new SSA.ChangedState(op))
  }

  /**
    * ILOAD, LLOAD, FLOAD, DLOAD, ALOAD, ISTORE, LSTORE, FSTORE, DSTORE, ASTORE, DUP, DUP_X1,
    * DUP_X2, DUP2, DUP2_X1, DUP2_X2, SWAP
    */
  def copyOperation(insn: AbstractInsnNode, value: SSA.Val): SSA.Val = value

  /**
    * INEG, LNEG, FNEG, DNEG, IINC, I2L, I2F, I2D, L2I, L2F, L2D, F2I, F2L, F2D, D2I, D2L, D2F,
    * I2B, I2C, I2S, INSTANCEOF
    */

  def unaryOp(insn: AbstractInsnNode, value: SSA.Val): SSA.Val = {
    insn.getOpcode match {
      case IINC =>
        val n = insn.asInstanceOf[IincInsnNode].incr
        val const = new SSA.ConstI(n)
        new SSA.BinOp(null, const, value, SSA.BinOp.IADD)

      case INEG | L2I | F2I | D2I | I2B | I2C | I2S | FNEG | I2F | L2F |
           D2F | LNEG | I2L | F2L | D2L | DNEG | I2D | L2D | F2D =>
        new SSA.UnaOp(value, SSA.UnaOp.lookup(insn.getOpcode))

      case INSTANCEOF => new SSA.InstanceOf(value, JType.read(insn.asInstanceOf[TypeInsnNode].desc))
    }
  }
  /**
    * CHECKCAST, NEWARRAY, ANEWARRAY, ARRAYLENGTH
    */

  def unaryOpUnsafe(insn: AbstractInsnNode, value: SSA.Val, state: SSA.State): (SSA.Val, SSA.State) = {
    val op = insn.getOpcode match {
      case NEWARRAY =>
        new SSA.NewArray(
          state,
          value,
          insn.asInstanceOf[IntInsnNode].operand match {
            case T_BOOLEAN => JType.Prim.Z
            case T_CHAR => JType.Prim.C
            case T_BYTE => JType.Prim.B
            case T_SHORT => JType.Prim.S
            case T_INT => JType.Prim.I
            case T_FLOAT => JType.Prim.F
            case T_DOUBLE => JType.Prim.D
            case T_LONG => JType.Prim.J
          }
        )

      case ANEWARRAY =>
        new SSA.NewArray(
          state,
          value,
          JType.read(insn.asInstanceOf[TypeInsnNode].desc)
        )

      case ARRAYLENGTH => new SSA.ArrayLength(state, value)

      case CHECKCAST => new SSA.CheckCast(state, value, JType.read(insn.asInstanceOf[TypeInsnNode].desc))
    }
    (op, new SSA.ChangedState(op))
  }

  /**
    * GETFIELD
    */
  def getFieldOp(insn: AbstractInsnNode, value: SSA.Val, state: SSA.State): (SSA.Val, SSA.State) = {
    val insn2 = insn.asInstanceOf[FieldInsnNode]
    val op = new SSA.GetField(state, value, insn2.owner, insn2.name, JType.read(insn2.desc))
    (op, new SSA.ChangedState(op))
  }

  /**
    * IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE, TABLESWITCH, LOOKUPSWITCH, IRETURN, LRETURN,
    * * FRETURN, DRETURN, ARETURN, PUTSTATIC, ATHROW,
    * * MONITORENTER, MONITOREXIT, IFNULL, IFNONNULL
    */
  def unaryCommand(insn: AbstractInsnNode, value: SSA.Val): Unit = {
    insn.getOpcode match {
      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IFNULL | IFNONNULL =>
      case TABLESWITCH =>
      case LOOKUPSWITCH =>
      case IRETURN | LRETURN | FRETURN | DRETURN | ARETURN =>
      case MONITORENTER =>
      case MONITOREXIT =>
      case ATHROW =>
    }
  }

  /**
    * PUTSTATIC
    */
  def putStaticCommand(insn: AbstractInsnNode, value: SSA.Val, state: SSA.State): SSA.State = {
    val insn2 = insn.asInstanceOf[FieldInsnNode]
    val res = new SSA.PutStatic(state, value, insn2.owner, insn2.name, JType.read(insn2.desc))
    new SSA.ChangedState(res)
  }
  /**
    * IADD, LADD, FADD, DADD, ISUB, LSUB, FSUB, DSUB, IMUL, LMUL, FMUL, DMUL,
    * ISHL, LSHL, ISHR, LSHR, IUSHR, LUSHR, IAND, LAND, IOR, LOR, IXOR, LXOR,
    * LCMP, FCMPL, FCMPG, DCMPL, DCMPG
    */
  def binaryOp(insn: AbstractInsnNode, v1: SSA.Val, v2: SSA.Val): SSA.Val = {
    insn.getOpcode match {
      case IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR |
           FADD | FSUB | FMUL | FDIV | FREM | LCMP | FCMPL | FCMPG | DCMPL | DCMPG |
           LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR |
           DADD | DSUB | DMUL | DDIV | DREM =>
        new SSA.BinOp(null, v1, v2, SSA.BinOp.lookup(insn.getOpcode))
    }
  }

  /**
    * IALOAD, LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD,
    * IDIV, LDIV, FDIV, DDIV, IREM, LREM, FREM, DREM,
    */
  def binaryOpUnsafe(insn: AbstractInsnNode, v1: SSA.Val, v2: SSA.Val, state: SSA.State): (SSA.Val, SSA.State) = {
    val op = insn.getOpcode match {
      case IALOAD => new SSA.GetArray(state, v1, v2, JType.Prim.I)
      case BALOAD => new SSA.GetArray(state, v1, v2, JType.Prim.B)
      case CALOAD => new SSA.GetArray(state, v1, v2, JType.Prim.C)
      case SALOAD => new SSA.GetArray(state, v1, v2, JType.Prim.S)
      case FALOAD => new SSA.GetArray(state, v1, v2, JType.Prim.F)
      case AALOAD => new SSA.GetArray(state, v1, v2, JType.Cls("java/lang/Object"))
      case LALOAD => new SSA.GetArray(state, v1, v2, JType.Prim.J)
      case DALOAD => new SSA.GetArray(state, v1, v2, JType.Prim.D)

      case IDIV | IREM | FDIV | FREM | LDIV | LREM | DDIV | DREM =>
        new SSA.BinOp(state, v1, v2, SSA.BinOp.lookup(insn.getOpcode))
    }
    (op, new SSA.ChangedState(op))
  }

  /**
    * IF_ICMPEQ, IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ACMPEQ,
    * IF_ACMPNE
    */
  def binaryCommand(insn: AbstractInsnNode, value1: SSA.Val, value2: SSA.Val): Unit = {
    insn.getOpcode match {
      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT |
           IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE =>
    }
  }

  /**
    * PUTFIELD
    */
  def putFieldOp(insn: AbstractInsnNode, value1: SSA.Val, value2: SSA.Val, state: SSA.State): SSA.State = {
    val insn2 = insn.asInstanceOf[FieldInsnNode]
    val op = new SSA.PutField(state, value1, value2, insn2.owner, insn2.name, JType.read(insn2.desc))
    new SSA.ChangedState(op)
  }

  /**
    * IASTORE, LASTORE, FASTORE, DASTORE, AASTORE, BASTORE, CASTORE, SASTORE
    */
  def ternaryOperation(insn: AbstractInsnNode, value1: SSA.Val, value2: SSA.Val, value3: SSA.Val, state: SSA.State): SSA.State = {
    val op = new SSA.PutArray(state, value1, value2, value3)
    new SSA.ChangedState(op)
  }

  /**
    * INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC, INVOKEINTERFACE, MULTIANEWARRAY and
    * INVOKEDYNAMIC
    */
  def naryOperation(insn: AbstractInsnNode, vs: Seq[SSA.Val], state: SSA.State): (SSA.Val, SSA.State) = {
    insn.getOpcode match{
      case MULTIANEWARRAY =>
        val insn2 = insn.asInstanceOf[MultiANewArrayInsnNode]
        val op = new SSA.MultiANewArray(state, JType.read(insn2.desc), vs)
        (op, new SSA.ChangedState(op))

      case INVOKEDYNAMIC =>
        val insn2 = insn.asInstanceOf[InvokeDynamicInsnNode]
        val bsm = insn2.bsm
        val op = new SSA.InvokeDynamic(
          insn2.name, Desc.read(insn2.desc),
          SSA.InvokeDynamic.bootstrapFromHandle(bsm),
          insn2.bsmArgs.map(SSA.InvokeDynamic.anyToArg),
          vs
        )
        (op, new SSA.ChangedState(op))

      case INVOKESTATIC =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        val op = new SSA.InvokeStatic(state, vs, insn2.owner, insn2.name, Desc.read(insn2.desc))
        (op, new SSA.ChangedState(op))
      case INVOKEVIRTUAL =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        val op = new SSA.InvokeVirtual(state, vs, insn2.owner, insn2.name, Desc.read(insn2.desc))
        (op, new SSA.ChangedState(op))
      case INVOKEINTERFACE =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        val op = new SSA.InvokeInterface(state, vs, insn2.owner, insn2.name, Desc.read(insn2.desc))
        (op, new SSA.ChangedState(op))
      case INVOKESPECIAL =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        val op = new SSA.InvokeSpecial(state, vs, insn2.owner, insn2.name, Desc.read(insn2.desc))
        (op, new SSA.ChangedState(op))
    }

  }


  def returnOperation(insn: AbstractInsnNode, value: SSA.Val, expected: SSA.Val) = ()

  def merge[N <: SSA.Val](v1: N, v2: N, insnIndex: Int, targetInsnIndex: Int) = {
    val src = findBlockStart(insnIndex).asInstanceOf[SSA.Merge]
    val phi = v1.asInstanceOf[SSA.Phi]
    phi.incoming += (src -> v2)
    src.nextPhis = Seq(phi) ++ src.nextPhis

    v2.downstreamAdd(v1)
    v1
  }

  def merge0[N <: SSA.Val](value1: N, insnIndex: Int, targetInsnIndex: Int) = {
    findBlockDest(targetInsnIndex) match{
      case Some(dest) if insnIndex != targetInsnIndex =>
        val src = findBlockStart(insnIndex)
        val phiStub = new SSA.Phi(dest, Set(src -> value1), value1.jtype)
        src.nextPhis ++= Seq(phiStub)
        dest.phis ++= Seq(phiStub)
        merges.add(phiStub)
        phiStub.asInstanceOf[N]
      case _ => value1
    }
  }

  def newParameterValue(local: Int, tpe: Type) = {
    new SSA.Arg(argMapping(local), JType.read(tpe.getInternalName))
  }

  def newEmptyValue(local: Int) = {
    new SSA.Arg(-1, JType.Prim.V)
  }

  def newExceptionValue(tryCatchBlockNode: TryCatchBlockNode,
                        exceptionType: Type) = {
    new SSA.Arg(-1, JType.read(exceptionType.getInternalName))
  }
}
