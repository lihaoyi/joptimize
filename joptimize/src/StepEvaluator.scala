package joptimize
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.Handle
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
class StepEvaluator(merges: mutable.Set[(SSA.Phi, SSA)]) extends Interpreter[SSA](ASM4){

  def newValue(tpe: org.objectweb.asm.Type) = {
    if (tpe == null) SSA.Arg(-1, JType.Prim.V)
    else {
      val jtype = JType.read(tpe.getInternalName)
      val res = SSA.Arg(-1, jtype)
      res
    }
  }

  def newOperation(insn: AbstractInsnNode) = {
    insn.getOpcode match {
      case ACONST_NULL => SSA.PushNull()
      case ICONST_M1 => SSA.PushI(-1)
      case ICONST_0 => SSA.PushI(0)
      case ICONST_1 => SSA.PushI(1)
      case ICONST_2 => SSA.PushI(2)
      case ICONST_3 => SSA.PushI(3)
      case ICONST_4 => SSA.PushI(4)
      case ICONST_5 => SSA.PushI(5)
      case LCONST_0 => SSA.PushJ(0)
      case LCONST_1 => SSA.PushJ(1)
      case FCONST_0 => SSA.PushF(0)
      case FCONST_1 => SSA.PushF(1)
      case FCONST_2 => SSA.PushF(2)
      case DCONST_0 => SSA.PushD(0)
      case DCONST_1 => SSA.PushD(1)
      case BIPUSH | SIPUSH => SSA.PushI(insn.asInstanceOf[IntInsnNode].operand)
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match {
          case i: java.lang.Integer => SSA.PushI(i)
          case f: java.lang.Float => SSA.PushF(f)
          case d: java.lang.Double => SSA.PushD(d)
          case s: java.lang.String => SSA.PushS(s)
          case value: org.objectweb.asm.Type =>
            value.getSort match {
              case OBJECT | ARRAY => SSA.PushCls(JType.Cls(value.getClassName))
              case METHOD => ???
            }
          case _: Handle => ???
        }
      case JSR => ???
      case GETSTATIC =>
        val insn2 = insn.asInstanceOf[FieldInsnNode]
        SSA.GetStatic(JType.Cls(insn2.owner), insn2.name, insn2.desc)
      case NEW => SSA.New(insn.asInstanceOf[TypeInsnNode].desc)
    }
  }

  // We do not record any of these copy operations in the SSA dataflow graph
  // that we construct during abstract interpretation. Those do not meaningfully
  // affect the shape of the graph, and we will generate our own copy operations
  // as-necessary when serializing the graph back out to bytecode
  def copyOperation(insn: AbstractInsnNode, value: SSA) = value

  def unaryOperation(insn: AbstractInsnNode, value: SSA) = {
    insn.getOpcode match {
      case IINC =>
        val n = insn.asInstanceOf[IincInsnNode].incr
        val const = SSA.PushI(n)
        SSA.BinOp(const, value, SSA.BinOp.IADD)

      case INEG | L2I | F2I | D2I | I2B | I2C | I2S | FNEG | I2F | L2F |
           D2F | LNEG | I2L | F2L | D2L | DNEG | I2D | L2D | F2D =>
        SSA.UnaryOp(value, SSA.UnaryOp.lookup(insn.getOpcode))

      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IFNULL | IFNONNULL => null
      case TABLESWITCH => null
      case LOOKUPSWITCH => null
      case IRETURN | LRETURN | FRETURN | DRETURN | ARETURN => null

      case PUTSTATIC =>
        val insn2 = insn.asInstanceOf[FieldInsnNode]
        val res = SSA.PutStatic(value, insn2.owner, insn2.name, insn2.desc)
        res

      case GETFIELD =>
        val insn2 = insn.asInstanceOf[FieldInsnNode]
        SSA.GetField(value, insn2.owner, insn2.name, insn2.desc)

      case NEWARRAY =>
        SSA.NewArray(
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
        SSA.NewArray(
          value,
          JType.Arr(JType.read(insn.asInstanceOf[TypeInsnNode].desc))
        )

      case ARRAYLENGTH => SSA.ArrayLength(value)

      case ATHROW => SSA.AThrow(value)

      case CHECKCAST => SSA.CheckCast(value, insn.asInstanceOf[TypeInsnNode].desc)

      case INSTANCEOF => SSA.InstanceOf(value, insn.asInstanceOf[TypeInsnNode].desc)

      case MONITORENTER => SSA.MonitorEnter(value)

      case MONITOREXIT => SSA.MonitorExit(value)
    }
  }

  def binaryOperation(insn: AbstractInsnNode, v1: SSA, v2: SSA) = {
    insn.getOpcode match {
      case IALOAD | BALOAD | CALOAD | SALOAD | FALOAD | AALOAD =>
        SSA.GetArray(v1, v2, 1)

      case LALOAD | DALOAD => SSA.GetArray(v1, v2, 2)

      case IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR |
           FADD | FSUB | FMUL | FDIV | FREM | LCMP | FCMPL | FCMPG | DCMPL | DCMPG |
           LADD | LSUB | LMUL | LDIV | LREM | LSHL |
           LSHR | LUSHR | LAND | LOR | LXOR | DADD | DSUB | DMUL | DDIV | DREM =>
        SSA.BinOp(v1, v2, SSA.BinOp.lookup(insn.getOpcode))


      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT |
           IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE => null

      case PUTFIELD => null
    }
  }

  def ternaryOperation(insn: AbstractInsnNode, v1: SSA, v2: SSA, v3: SSA) = null

  def naryOperation(insn: AbstractInsnNode, vs: java.util.List[_ <: SSA]) = {
    insn.getOpcode match{
      case MULTIANEWARRAY =>
        val insn2 = insn.asInstanceOf[MultiANewArrayInsnNode]
        SSA.MultiANewArray(insn2.desc, vs.asScala)

      case INVOKEDYNAMIC =>
        val insn2 = insn.asInstanceOf[InvokeDynamicInsnNode]
        val bsm = insn2.bsm
        SSA.InvokeDynamic(
          insn2.name, insn2.desc, bsm.getTag, bsm.getOwner,
          bsm.getName, bsm.getDesc, insn2.bsmArgs
        )

      case INVOKESTATIC =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        SSA.InvokeStatic(vs.asScala, insn2.owner, insn2.name, Desc.read(insn2.desc))

      case INVOKEVIRTUAL =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        SSA.InvokeVirtual(vs.asScala, insn2.owner, insn2.name, Desc.read(insn2.desc))

      case INVOKESPECIAL =>
        val insn2 = insn.asInstanceOf[MethodInsnNode]
        SSA.InvokeSpecial(vs.asScala, insn2.owner, insn2.name, Desc.read(insn2.desc))
    }
  }

  def returnOperation(insn: AbstractInsnNode, value: SSA, expected: SSA) = ()

  def merge(v1: SSA, v2: SSA) = {
    if (v1 == v2) v1
    else (v1, v2) match{
      case (x, phi: SSA.Phi) =>
        merges.add(phi -> x)
        phi
      case (phi: SSA.Phi, y) =>
        merges.add(phi -> y)
        phi
      case (x, y) =>
        val phi = new SSA.Phi(x.getSize)
        merges.add(phi -> x)
        merges.add(phi -> y)
        phi
    }
  }
}
