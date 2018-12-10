package joptimize
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.{Handle, Opcodes, Type}
import org.objectweb.asm.tree._

case class Box(inferredType: Type) extends Value {
  def getSize = inferredType.getSize
}

class Dataflow() extends Interpreter[Box](ASM4){
  def newValue(tpe: org.objectweb.asm.Type) = Box(tpe)

  def newOperation(insn: AbstractInsnNode) = {
    val tpe: Type = insn.getOpcode match{
      case ACONST_NULL => BasicInterpreter.NULL_TYPE
      case ICONST_M1 | ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 =>
        Type.INT_TYPE
      case LCONST_0 | LCONST_1 => Type.LONG_TYPE
      case FCONST_0 | FCONST_1 | FCONST_2 => Type.FLOAT_TYPE
      case DCONST_0 | DCONST_1 => Type.DOUBLE_TYPE
      case BIPUSH | SIPUSH => Type.INT_TYPE
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match{
          case _: java.lang.Integer => Type.INT_TYPE
          case _: java.lang.Float => Type.FLOAT_TYPE
          case _: java.lang.Double => Type.DOUBLE_TYPE
          case _: java.lang.String => Type.getObjectType("java/lang/String")
          case value: Type =>
            value.getSort match{
              case Type.OBJECT | Type.ARRAY => Type.getObjectType("java/lang/Class")
              case Type.METHOD => Type.getObjectType("java/lang/invoke/MethodType")
            }
          case _: Handle => Type.getObjectType("java/lang/invoke/MethodHandle")
        }
      case JSR => BasicInterpreter.NULL_TYPE
      case GETSTATIC => Type.getType(insn.asInstanceOf[FieldInsnNode].desc)
      case NEW => Type.getType(insn.asInstanceOf[TypeInsnNode].desc)
    }
    Box(tpe)
  }

  def copyOperation(insn: AbstractInsnNode, value: Box) = value

  def unaryOperation(insn: AbstractInsnNode, value: Box) = {
    val tpe: Type = insn.getOpcode match {
      case INEG | IINC | L2I | F2I | D2I | I2B | I2C | I2S => Type.INT_TYPE
      case FNEG| I2F| L2F| D2F => Type.FLOAT_TYPE
      case LNEG | I2L | F2L | D2L => Type.LONG_TYPE
      case DNEG | I2D | L2D | F2D => Type.DOUBLE_TYPE
      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | TABLESWITCH | LOOKUPSWITCH |
           IRETURN | LRETURN | FRETURN | DRETURN | ARETURN | PUTSTATIC =>
        BasicInterpreter.NULL_TYPE
      case GETFIELD => Type.getType(insn.asInstanceOf[FieldInsnNode].desc)
      case NEWARRAY =>
        insn.asInstanceOf[IntInsnNode].operand match {
          case T_BOOLEAN => Type.getType("[Z")
          case T_CHAR => Type.getType("[C")
          case T_BYTE => Type.getType("[B")
          case T_SHORT => Type.getType("[S")
          case T_INT => Type.getType("[I")
          case T_FLOAT => Type.getType("[F")
          case T_DOUBLE => Type.getType("[D")
          case T_LONG => Type.getType("[J")
        }
      case ANEWARRAY => Type.getType("[" + Type.getType(insn.asInstanceOf[TypeInsnNode].desc))
      case ARRAYLENGTH => Type.INT_TYPE
      case ATHROW => null
      case CHECKCAST => Type.getType(insn.asInstanceOf[TypeInsnNode].desc)
      case INSTANCEOF => Type.BOOLEAN_TYPE
      case MONITORENTER | MONITOREXIT | IFNULL | IFNONNULL => BasicInterpreter.NULL_TYPE
    }
    Box(tpe)
  }
  def binaryOperation(insn: AbstractInsnNode, v1: Box, v2: Box) = {
    val tpe: Type = insn.getOpcode match {
      case IALOAD | BALOAD | CALOAD | SALOAD | IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR =>
        Type.INT_TYPE
      case FALOAD | FADD | FSUB | FMUL | FDIV | FREM => Type.FLOAT_TYPE
      case LALOAD | LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR =>
        Type.LONG_TYPE
      case DALOAD | DADD | DSUB | DMUL | DDIV | DREM =>
        Type.DOUBLE_TYPE
      case AALOAD =>
        // Dereference the array type
        ???
      case LCMP | FCMPL | FCMPG | DCMPL | DCMPG => Type.INT_TYPE
      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE | PUTFIELD =>
        BasicInterpreter.NULL_TYPE
    }
    Box(tpe)
  }
  def ternaryOperation(insn: AbstractInsnNode, v1: Box, v2: Box, v3: Box) = {
    Box(BasicInterpreter.NULL_TYPE)
  }
  def naryOperation(insn: AbstractInsnNode, vs: java.util.List[_ <: Box]) = {
    val tpe: Type = insn.getOpcode match{
      case MULTIANEWARRAY => Type.getType(insn.asInstanceOf[MultiANewArrayInsnNode].desc)
      case INVOKEDYNAMIC => Type.getReturnType(insn.asInstanceOf[InvokeDynamicInsnNode].desc)
      case _ => Type.getReturnType(insn.asInstanceOf[MethodInsnNode].desc)
    }
    Box(tpe)
  }
  def returnOperation(insn: AbstractInsnNode, value: Box, expected: Box) = {}
  def merge(v1: Box, v2: Box) = v1
}
