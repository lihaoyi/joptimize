package joptimize
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.{Handle}
import org.objectweb.asm.tree._

class Dataflow(merge0: (JType, JType) => JType) extends Interpreter[JType](ASM4){
  def newValue(tpe: org.objectweb.asm.Type) = {
    if (tpe == null) JType.Null
    else JType.read(tpe.getInternalName)
  }

  def newOperation(insn: AbstractInsnNode) = {
    insn.getOpcode match{
      case ACONST_NULL => JType.Null
      case ICONST_M1 | ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 =>
        JType.Prim.I
      case LCONST_0 | LCONST_1 => JType.Prim.J
      case FCONST_0 | FCONST_1 | FCONST_2 => JType.Prim.F
      case DCONST_0 | DCONST_1 => JType.Prim.D
      case BIPUSH | SIPUSH => JType.Prim.I
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match{
          case _: java.lang.Integer => JType.Prim.I
          case _: java.lang.Float => JType.Prim.F
          case _: java.lang.Double => JType.Prim.D
          case _: java.lang.String => JType.Cls("java/lang/String")
          case value: org.objectweb.asm.Type =>
            value.getSort match{
              case OBJECT | ARRAY => JType.Cls("java/lang/Class")
              case METHOD => JType.Cls("java/lang/invoke/MethodType")
            }
          case _: Handle => JType.Cls("java/lang/invoke/MethodHandle")
        }
      case JSR => JType.Null
      case GETSTATIC => JType.read(insn.asInstanceOf[FieldInsnNode].desc)
      case NEW => JType.read(insn.asInstanceOf[TypeInsnNode].desc)
    }
  }

  def copyOperation(insn: AbstractInsnNode, value: JType) = value

  def unaryOperation(insn: AbstractInsnNode, value: JType) = {
    insn.getOpcode match {
      case INEG | IINC | L2I | F2I | D2I | I2B | I2C | I2S => JType.Prim.I
      case FNEG | I2F| L2F| D2F => JType.Prim.J
      case LNEG | I2L | F2L | D2L => JType.Prim.J
      case DNEG | I2D | L2D | F2D => JType.Prim.D
      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | TABLESWITCH | LOOKUPSWITCH |
           IRETURN | LRETURN | FRETURN | DRETURN | ARETURN | PUTSTATIC =>
        JType.Null
      case GETFIELD => JType.read(insn.asInstanceOf[FieldInsnNode].desc)
      case NEWARRAY =>
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
      case ANEWARRAY => JType.Arr(JType.read(insn.asInstanceOf[TypeInsnNode].desc))
      case ARRAYLENGTH => JType.Prim.I
      case ATHROW => JType.Null
      case CHECKCAST => JType.read(insn.asInstanceOf[TypeInsnNode].desc)
      case INSTANCEOF => JType.Prim.Z
      case MONITORENTER | MONITOREXIT | IFNULL | IFNONNULL => JType.Null
    }
  }
  def binaryOperation(insn: AbstractInsnNode, v1: JType, v2: JType) = {
    insn.getOpcode match {
      case IALOAD | BALOAD | CALOAD | SALOAD | IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR =>
        JType.Prim.I
      case FALOAD | FADD | FSUB | FMUL | FDIV | FREM => JType.Prim.F
      case LALOAD | LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR =>
        JType.Prim.J
      case DALOAD | DADD | DSUB | DMUL | DDIV | DREM =>
        JType.Prim.D
      case AALOAD => v1.asInstanceOf[JType.Arr].innerType
      case LCMP | FCMPL | FCMPG | DCMPL | DCMPG => JType.Prim.I
      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE | PUTFIELD =>
        JType.Null
    }
  }
  def ternaryOperation(insn: AbstractInsnNode, v1: JType, v2: JType, v3: JType) = {
    JType.Null
  }
  def naryOperation(insn: AbstractInsnNode, vs: java.util.List[_ <: JType]) = {
    insn.getOpcode match{
      case MULTIANEWARRAY => JType.read(insn.asInstanceOf[MultiANewArrayInsnNode].desc)
      case INVOKEDYNAMIC => Desc.read(insn.asInstanceOf[InvokeDynamicInsnNode].desc).ret
      case _ => Desc.read(insn.asInstanceOf[MethodInsnNode].desc).ret
    }
  }
  def returnOperation(insn: AbstractInsnNode, value: JType, expected: JType) = {}
  def merge(v1: JType, v2: JType) = merge0(v1, v2)
}
