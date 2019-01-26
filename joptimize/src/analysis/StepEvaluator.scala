package joptimize.analysis
import java.util

import joptimize.model.{Desc, JType, SSA}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.{Handle, Type}
import org.objectweb.asm.tree._

import collection.JavaConverters._
import scala.collection.mutable

/**
  * Takes in java bytecode instructions one at a time and converts them to
  * [[SSA]] nodes.
  *
  * Takes in a [[Typer]] that it uses to perform type inference on each of the
  * generated SSA nodes; we do this to allow immediate constant folding if the
  * node's type is specific enough to be a concrete value.
  */
class StepEvaluator(merges: mutable.LinkedHashSet[SSA.Phi],
                    findBlockStart: Int => SSA.Block,
                    findBlockDest: Int => Option[SSA.Block]) extends joptimize.bytecode.Interpreter[SSA.Val, SSA.State]{

  /**
    * ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5,
    * LCONST_0, LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0, DCONST_1, BIPUSH, SIPUSH, LDC, JSR,
    * NEW
    */
  def constOperation(insn: AbstractInsnNode): SSA.Val = {
    insn.getOpcode match {
      case ACONST_NULL => new SSA.PushNull()
      case ICONST_M1 => new SSA.PushI(-1)
      case ICONST_0 => new SSA.PushI(0)
      case ICONST_1 => new SSA.PushI(1)
      case ICONST_2 => new SSA.PushI(2)
      case ICONST_3 => new SSA.PushI(3)
      case ICONST_4 => new SSA.PushI(4)
      case ICONST_5 => new SSA.PushI(5)
      case LCONST_0 => new SSA.PushJ(0)
      case LCONST_1 => new SSA.PushJ(1)
      case FCONST_0 => new SSA.PushF(0)
      case FCONST_1 => new SSA.PushF(1)
      case FCONST_2 => new SSA.PushF(2)
      case DCONST_0 => new SSA.PushD(0)
      case DCONST_1 => new SSA.PushD(1)
      case BIPUSH | SIPUSH => new SSA.PushI(insn.asInstanceOf[IntInsnNode].operand)
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match {
          case i: java.lang.Integer => new SSA.PushI(i)
          case f: java.lang.Float => new SSA.PushF(f)
          case d: java.lang.Double => new SSA.PushD(d)
          case s: java.lang.String => new SSA.PushS(s)
          case value: org.objectweb.asm.Type =>
            value.getSort match {
              case OBJECT | ARRAY => new SSA.PushCls(JType.Cls(value.getClassName))
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
    val op = new SSA.GetStatic(state, JType.Cls(insn2.owner), insn2.name, insn2.desc)
    (op, new SSA.State(op))
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
        val const = new SSA.PushI(n)
        new SSA.BinOp(const, value, SSA.BinOp.IADD)

      case INEG | L2I | F2I | D2I | I2B | I2C | I2S | FNEG | I2F | L2F |
           D2F | LNEG | I2L | F2L | D2L | DNEG | I2D | L2D | F2D =>
        new SSA.UnaOp(value, SSA.UnaOp.lookup(insn.getOpcode))

      case INSTANCEOF => new SSA.InstanceOf(value, insn.asInstanceOf[TypeInsnNode].desc)
    }
  }
  /**
    * CHECKCAST, NEWARRAY, ANEWARRAY, ARRAYLENGTH
    */

  def unaryOpUnsafe(insn: AbstractInsnNode, value: SSA.Val, state: SSA.State): (SSA.Val, SSA.State) = {
    val op = insn.getOpcode match {
      case NEWARRAY =>
        new SSA.NewArray(
          value,
          insn.asInstanceOf[IntInsnNode].operand match {
            case T_BOOLEAN => JType.Arr(JType.Prim.Z)
            case T_CHAR => JType.Arr(JType.Prim.C)
            case T_BYTE => JType.Arr(JType.Prim.B)
            case T_SHORT => JType.Arr(JType.Prim.S)
            case T_INT => JType.Arr(JType.Prim.I)
            case T_FLOAT => JType.Arr(JType.Prim.F)
            case T_DOUBLE => JType.Arr(JType.Prim.D)
            case T_LONG => JType.Arr(JType.Prim.J)
          }
        )

      case ANEWARRAY =>
        new SSA.NewArray(
          value,
          JType.Arr(JType.read(insn.asInstanceOf[TypeInsnNode].desc))
        )

      case ARRAYLENGTH => new SSA.ArrayLength(value)

      case CHECKCAST => new SSA.CheckCast(value, insn.asInstanceOf[TypeInsnNode].desc)
    }
    (op, new SSA.State(op))
  }

  /**
    * GETFIELD
    */
  def getFieldOp(insn: AbstractInsnNode, value: SSA.Val, state: SSA.State): (SSA.Val, SSA.State) = {
    val insn2 = insn.asInstanceOf[FieldInsnNode]
    val op = new SSA.GetField(state, value, insn2.owner, insn2.name, insn2.desc)
    (op, new SSA.State(op))
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
    }
  }

  /**
    * PUTSTATIC
    */
  def putStaticCommand(insn: AbstractInsnNode, value: SSA.Val, state: SSA.State): SSA.State = {
    val insn2 = insn.asInstanceOf[FieldInsnNode]
    val res = new SSA.PutStatic(state, value, insn2.owner, insn2.name, insn2.desc)
    new SSA.State(res)
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
        new SSA.BinOp(v1, v2, SSA.BinOp.lookup(insn.getOpcode))
    }
  }

  /**
    * IALOAD, LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD,
    * IDIV, LDIV, FDIV, DDIV, IREM, LREM, FREM, DREM,
    */
  def binaryOpUnsafe(insn: AbstractInsnNode, v1: SSA.Val, v2: SSA.Val, state: SSA.State): (SSA.Val, SSA.State) = {
    val op = insn.getOpcode match {
      case IALOAD => new SSA.GetArray(null, v1, v2, JType.Prim.I)
      case BALOAD => new SSA.GetArray(null, v1, v2, JType.Prim.B)
      case CALOAD => new SSA.GetArray(null, v1, v2, JType.Prim.C)
      case SALOAD => new SSA.GetArray(null, v1, v2, JType.Prim.S)
      case FALOAD => new SSA.GetArray(null, v1, v2, JType.Prim.F)
      case AALOAD => new SSA.GetArray(null, v1, v2, JType.Cls("java/lang/Object"))
      case LALOAD => new SSA.GetArray(null, v1, v2, JType.Prim.J)
      case DALOAD => new SSA.GetArray(null, v1, v2, JType.Prim.D)

      case IDIV | IREM | FDIV | FREM | LDIV | LREM | DDIV | DREM =>
        new SSA.BinOp(v1, v2, SSA.BinOp.lookup(insn.getOpcode))
    }
    (op, new SSA.State(op))
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
    val op = new SSA.PutField(state, value1, value2, insn2.owner, insn2.name, insn2.desc)
    new SSA.State(op)
  }

  /**
    * IASTORE, LASTORE, FASTORE, DASTORE, AASTORE, BASTORE, CASTORE, SASTORE
    */
  def ternaryOperation(insn: AbstractInsnNode, value1: SSA.Val, value2: SSA.Val, value3: SSA.Val, state: SSA.State): SSA.State = {
    val op = new SSA.PutArray(state, value1, value2, value3)
    new SSA.State(op)
  }

  /**
    * INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC, INVOKEINTERFACE, MULTIANEWARRAY and
    * INVOKEDYNAMIC
    */
  def naryOperation(insn: AbstractInsnNode, vs: Seq[SSA.Val], state: SSA.State): (SSA.Val, SSA.State) = {
    val op = insn.getOpcode match{
      case MULTIANEWARRAY =>
        val insn2 = insn.asInstanceOf[MultiANewArrayInsnNode]
        new SSA.MultiANewArray(insn2.desc, vs)

      case INVOKEDYNAMIC =>
        val insn2 = insn.asInstanceOf[InvokeDynamicInsnNode]
        val bsm = insn2.bsm
        new SSA.InvokeDynamic(
          insn2.name, insn2.desc, bsm.getTag, bsm.getOwner,
          bsm.getName, bsm.getDesc, insn2.bsmArgs
        )

      case INVOKESTATIC =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        new SSA.InvokeStatic(vs, insn2.owner, insn2.name, Desc.read(insn2.desc))

      case INVOKEVIRTUAL =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        new SSA.InvokeVirtual(vs, insn2.owner, insn2.name, Desc.read(insn2.desc))

      case INVOKESPECIAL =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        new SSA.InvokeSpecial(vs, insn2.owner, insn2.name, Desc.read(insn2.desc))
    }
    (op, new SSA.State(op))
  }


  def returnOperation(insn: AbstractInsnNode, value: SSA.Val, expected: SSA.Val) = ()

  def merge(v1: SSA.Val, v2: SSA.Val, insnIndex: Int, targetInsnIndex: Int) = {
    if (v1 == v2) v1
    else{
      if (findBlockDest(targetInsnIndex).isDefined && insnIndex != targetInsnIndex) {
        v1.asInstanceOf[SSA.Phi].incoming += (findBlockStart(insnIndex) -> v2)
        findBlockStart(insnIndex).downstreamAdd(v1)
        v2.downstreamAdd(v1)
      }
      v1.checkLinks()
      v1
    }
  }

  def merge0(value1: SSA.Val, insnIndex: Int, targetInsnIndex: Int) = {
    findBlockDest(targetInsnIndex) match{
      case Some(dest) if insnIndex != targetInsnIndex =>
        val phiStub = new SSA.Phi(dest, Set(findBlockStart(insnIndex) -> value1), value1.jtype)
        merges.add(phiStub)
        phiStub
      case _ => value1
    }
  }

  def newParameterValue(local: Int, tpe: Type) = {
    val jtype = JType.read(tpe.getInternalName)
    val res = new SSA.Arg(local, jtype)
    res
  }

  def newEmptyValue(local: Int) = {
    new SSA.Arg(-1, JType.Prim.V)
  }

  def newExceptionValue(tryCatchBlockNode: TryCatchBlockNode,
                        exceptionType: Type) = {
    new SSA.Arg(-1, JType.read(exceptionType.getInternalName))
  }
}
