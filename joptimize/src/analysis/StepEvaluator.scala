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
                    blockStartIndex: Int => Boolean,
                    findBlockStart: Int => SSA.Ctrl,
                    findBlockDest: Int => SSA.Ctrl) extends joptimize.bytecode.Interpreter[SSA.Val]{

  def newOperation(insn: AbstractInsnNode) = {
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
      case GETSTATIC =>
        val insn2 = insn.asInstanceOf[FieldInsnNode]
        new SSA.GetStatic(JType.Cls(insn2.owner), insn2.name, insn2.desc)
      case NEW => new SSA.New(insn.asInstanceOf[TypeInsnNode].desc)
    }
  }

  // We do not record any of these copy operations in the SSA dataflow graph
  // that we construct during abstract interpretation. Those do not meaningfully
  // affect the shape of the graph, and we will generate our own copy operations
  // as-necessary when serializing the graph back out to bytecode
  def copyOperation(insn: AbstractInsnNode, value: SSA.Val) = value

  def unaryOperation(insn: AbstractInsnNode, value: SSA.Val) = {
    insn.getOpcode match {
      case IINC =>
        val n = insn.asInstanceOf[IincInsnNode].incr
        val const = new SSA.PushI(n)
        new SSA.BinOp(const, value, SSA.BinOp.IADD)

      case INEG | L2I | F2I | D2I | I2B | I2C | I2S | FNEG | I2F | L2F |
           D2F | LNEG | I2L | F2L | D2L | DNEG | I2D | L2D | F2D =>
        new SSA.UnaOp(value, SSA.UnaOp.lookup(insn.getOpcode))

      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IFNULL | IFNONNULL => null
      case TABLESWITCH => null
      case LOOKUPSWITCH => null
      case IRETURN | LRETURN | FRETURN | DRETURN | ARETURN => null

      case PUTSTATIC =>
        val insn2 = insn.asInstanceOf[FieldInsnNode]
        val res = new SSA.PutStatic(value, insn2.owner, insn2.name, insn2.desc)
        res

      case GETFIELD =>
        val insn2 = insn.asInstanceOf[FieldInsnNode]
        new SSA.GetField(value, insn2.owner, insn2.name, insn2.desc)

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

      case INSTANCEOF => new SSA.InstanceOf(value, insn.asInstanceOf[TypeInsnNode].desc)

      case MONITORENTER => new SSA.MonitorEnter(value)

      case MONITOREXIT => new SSA.MonitorExit(value)
    }
  }

  def binaryOperation(insn: AbstractInsnNode, v1: SSA.Val, v2: SSA.Val) = {
    insn.getOpcode match {
      case IALOAD | BALOAD | CALOAD | SALOAD | FALOAD | AALOAD =>
        new SSA.GetArray(v1, v2, 1)

      case LALOAD | DALOAD => new SSA.GetArray(v1, v2, 2)

      case IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR |
           FADD | FSUB | FMUL | FDIV | FREM | LCMP | FCMPL | FCMPG | DCMPL | DCMPG |
           LADD | LSUB | LMUL | LDIV | LREM | LSHL |
           LSHR | LUSHR | LAND | LOR | LXOR | DADD | DSUB | DMUL | DDIV | DREM =>
        new SSA.BinOp(v1, v2, SSA.BinOp.lookup(insn.getOpcode))


      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT |
           IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE => null

      case PUTFIELD => null
    }
  }

  def naryOperation(insn: AbstractInsnNode, vs: Seq[SSA.Val]) = {
    insn.getOpcode match{
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
  }

  def returnOperation(insn: AbstractInsnNode, value: SSA.Val, expected: SSA.Val) = ()

  def merge(v1: SSA.Val, v2: SSA.Val, insnIndex: Int, targetInsnIndex: Int) = {
    if (v1 == v2) v1
    else{
      if (blockStartIndex(targetInsnIndex) && insnIndex != targetInsnIndex) {
        v1.asInstanceOf[SSA.Phi].incoming += (findBlockStart(insnIndex) -> v2)
        findBlockStart(insnIndex).downstream += v1
        v2.downstream += v1
      }
      v1.checkLinks()
      v1
    }
  }

  def merge0(value1: SSA.Val, insnIndex: Int, targetInsnIndex: Int) = {
    if (blockStartIndex(targetInsnIndex) && insnIndex != targetInsnIndex) {
      val phiStub = new SSA.Phi(findBlockDest(targetInsnIndex), Set(findBlockStart(insnIndex) -> value1), value1.getSize)
      merges.add(phiStub)
      phiStub
    }else{
      value1
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
