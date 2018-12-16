package joptimize
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.{Handle}
import org.objectweb.asm.tree._

class Dataflow(merge0: (Type, Type) => Type) extends Interpreter[Type](ASM4){
  def newValue(tpe: org.objectweb.asm.Type) = {
    if (tpe == null) Type.Null
    else Type.read(tpe.getInternalName)
  }

  def newOperation(insn: AbstractInsnNode) = {
    insn.getOpcode match{
      case ACONST_NULL => Type.Null
      case ICONST_M1 | ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 =>
        Type.Prim.I
      case LCONST_0 | LCONST_1 => Type.Prim.J
      case FCONST_0 | FCONST_1 | FCONST_2 => Type.Prim.F
      case DCONST_0 | DCONST_1 => Type.Prim.D
      case BIPUSH | SIPUSH => Type.Prim.I
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match{
          case _: java.lang.Integer => Type.Prim.I
          case _: java.lang.Float => Type.Prim.F
          case _: java.lang.Double => Type.Prim.D
          case _: java.lang.String => Type.Cls("java/lang/String")
          case value: org.objectweb.asm.Type =>
            value.getSort match{
              case OBJECT | ARRAY => Type.Cls("java/lang/Class")
              case METHOD => Type.Cls("java/lang/invoke/MethodType")
            }
          case _: Handle => Type.Cls("java/lang/invoke/MethodHandle")
        }
      case JSR => Type.Null
      case GETSTATIC => Type.read(insn.asInstanceOf[FieldInsnNode].desc)
      case NEW => Type.read(insn.asInstanceOf[TypeInsnNode].desc)
    }
  }

  def copyOperation(insn: AbstractInsnNode, value: Type) = value

  def unaryOperation(insn: AbstractInsnNode, value: Type) = {
    insn.getOpcode match {
      case INEG | IINC | L2I | F2I | D2I | I2B | I2C | I2S => Type.Prim.I
      case FNEG | I2F| L2F| D2F => Type.Prim.J
      case LNEG | I2L | F2L | D2L => Type.Prim.J
      case DNEG | I2D | L2D | F2D => Type.Prim.D
      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | TABLESWITCH | LOOKUPSWITCH |
           IRETURN | LRETURN | FRETURN | DRETURN | ARETURN | PUTSTATIC =>
        Type.Null
      case GETFIELD => Type.read(insn.asInstanceOf[FieldInsnNode].desc)
      case NEWARRAY =>
        insn.asInstanceOf[IntInsnNode].operand match {
          case T_BOOLEAN => Type.Arr(Type.Prim.Z)
          case T_CHAR => Type.Arr(Type.Prim.C)
          case T_BYTE => Type.Arr(Type.Prim.B)
          case T_SHORT => Type.Arr(Type.Prim.S)
          case T_INT => Type.Arr(Type.Prim.I)
          case T_FLOAT => Type.Arr(Type.Prim.F)
          case T_DOUBLE => Type.Arr(Type.Prim.D)
          case T_LONG => Type.Arr(Type.Prim.J)
        }
      case ANEWARRAY => Type.Arr(Type.read(insn.asInstanceOf[TypeInsnNode].desc))
      case ARRAYLENGTH => Type.Prim.I
      case ATHROW => Type.Null
      case CHECKCAST => Type.read(insn.asInstanceOf[TypeInsnNode].desc)
      case INSTANCEOF => Type.Prim.Z
      case MONITORENTER | MONITOREXIT | IFNULL | IFNONNULL => Type.Null
    }
  }
  def binaryOperation(insn: AbstractInsnNode, v1: Type, v2: Type) = {
    insn.getOpcode match {
      case IALOAD | BALOAD | CALOAD | SALOAD | IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR =>
        Type.Prim.I
      case FALOAD | FADD | FSUB | FMUL | FDIV | FREM => Type.Prim.F
      case LALOAD | LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR =>
        Type.Prim.J
      case DALOAD | DADD | DSUB | DMUL | DDIV | DREM =>
        Type.Prim.D
      case AALOAD => v1.asInstanceOf[Type.Arr].innerType
      case LCMP | FCMPL | FCMPG | DCMPL | DCMPG => Type.Prim.I
      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE | PUTFIELD =>
        Type.Null
    }
  }
  def ternaryOperation(insn: AbstractInsnNode, v1: Type, v2: Type, v3: Type) = {
    Type.Null
  }
  def naryOperation(insn: AbstractInsnNode, vs: java.util.List[_ <: Type]) = {
    insn.getOpcode match{
      case MULTIANEWARRAY => Type.read(insn.asInstanceOf[MultiANewArrayInsnNode].desc)
      case INVOKEDYNAMIC => Desc.read(insn.asInstanceOf[InvokeDynamicInsnNode].desc).ret
      case _ => Desc.read(insn.asInstanceOf[MethodInsnNode].desc).ret
    }
  }
  def returnOperation(insn: AbstractInsnNode, value: Type, expected: Type) = {}
  def merge(v1: Type, v2: Type) = merge0(v1, v2)
}
