package joptimize
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.{Handle}
import org.objectweb.asm.tree._

/**
  * The inferred type at any point in the program.
  *
  * Currently a dumb wrapper of the ASM/JVM runtime types, but should grow more
  * detailed in future to include things such as:
  *
  * - Literal types: for integers, strings, ...
  * - Union types: A | B, A | B | C. More specific than widening to the LUB
  * - Arrays of known size
  * - Bottom: a type representing unreachable code, to help in narrowing after
  *   dead code elimination
  */
case class Inferred(value: org.objectweb.asm.Type) extends org.objectweb.asm.tree.analysis.Value{
  def getSize: Int = value.getSize
}
object Inferred{
  implicit def fromAsmType(value: org.objectweb.asm.Type): Inferred = Inferred(value)
}

object Dataflow extends Interpreter[Inferred](ASM4){
  def newValue(tpe: org.objectweb.asm.Type) = tpe

  def newOperation(insn: AbstractInsnNode) = {
    insn.getOpcode match{
      case ACONST_NULL => BasicInterpreter.NULL_TYPE
      case ICONST_M1 | ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 =>
        INT_TYPE
      case LCONST_0 | LCONST_1 => LONG_TYPE
      case FCONST_0 | FCONST_1 | FCONST_2 => FLOAT_TYPE
      case DCONST_0 | DCONST_1 => DOUBLE_TYPE
      case BIPUSH | SIPUSH => INT_TYPE
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match{
          case _: java.lang.Integer => INT_TYPE
          case _: java.lang.Float => FLOAT_TYPE
          case _: java.lang.Double => DOUBLE_TYPE
          case _: java.lang.String => getObjectType("java/lang/String")
          case value: org.objectweb.asm.Type =>
            value.getSort match{
              case OBJECT | ARRAY => getObjectType("java/lang/Class")
              case METHOD => getObjectType("java/lang/invoke/MethodType")
            }
          case _: Handle => getObjectType("java/lang/invoke/MethodHandle")
        }
      case JSR => BasicInterpreter.NULL_TYPE
      case GETSTATIC => getType(insn.asInstanceOf[FieldInsnNode].desc)
      case NEW => getType(insn.asInstanceOf[TypeInsnNode].desc)
    }
  }

  def copyOperation(insn: AbstractInsnNode, value: Inferred) = value

  def unaryOperation(insn: AbstractInsnNode, value: Inferred) = {
    insn.getOpcode match {
      case INEG | IINC | L2I | F2I | D2I | I2B | I2C | I2S => INT_TYPE
      case FNEG | I2F| L2F| D2F => FLOAT_TYPE
      case LNEG | I2L | F2L | D2L => LONG_TYPE
      case DNEG | I2D | L2D | F2D => DOUBLE_TYPE
      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | TABLESWITCH | LOOKUPSWITCH |
           IRETURN | LRETURN | FRETURN | DRETURN | ARETURN | PUTSTATIC =>
        BasicInterpreter.NULL_TYPE
      case GETFIELD => getType(insn.asInstanceOf[FieldInsnNode].desc)
      case NEWARRAY =>
        insn.asInstanceOf[IntInsnNode].operand match {
          case T_BOOLEAN => getType("[Z")
          case T_CHAR => getType("[C")
          case T_BYTE => getType("[B")
          case T_SHORT => getType("[S")
          case T_INT => getType("[I")
          case T_FLOAT => getType("[F")
          case T_DOUBLE => getType("[D")
          case T_LONG => getType("[J")
        }
      case ANEWARRAY => getType("[" + getType(insn.asInstanceOf[TypeInsnNode].desc))
      case ARRAYLENGTH => INT_TYPE
      case ATHROW => BasicInterpreter.NULL_TYPE
      case CHECKCAST => getType(insn.asInstanceOf[TypeInsnNode].desc)
      case INSTANCEOF => BOOLEAN_TYPE
      case MONITORENTER | MONITOREXIT | IFNULL | IFNONNULL => BasicInterpreter.NULL_TYPE
    }
  }
  def binaryOperation(insn: AbstractInsnNode, v1: Inferred, v2: Inferred) = {
    insn.getOpcode match {
      case IALOAD | BALOAD | CALOAD | SALOAD | IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR =>
        INT_TYPE
      case FALOAD | FADD | FSUB | FMUL | FDIV | FREM => FLOAT_TYPE
      case LALOAD | LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR =>
        LONG_TYPE
      case DALOAD | DADD | DSUB | DMUL | DDIV | DREM =>
        DOUBLE_TYPE
      case AALOAD => v1.value.getElementType
      case LCMP | FCMPL | FCMPG | DCMPL | DCMPG => INT_TYPE
      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE | PUTFIELD =>
        BasicInterpreter.NULL_TYPE
    }
  }
  def ternaryOperation(insn: AbstractInsnNode, v1: Inferred, v2: Inferred, v3: Inferred) = {
    BasicInterpreter.NULL_TYPE
  }
  def naryOperation(insn: AbstractInsnNode, vs: java.util.List[_ <: Inferred]) = {
    insn.getOpcode match{
      case MULTIANEWARRAY => getType(insn.asInstanceOf[MultiANewArrayInsnNode].desc)
      case INVOKEDYNAMIC => getReturnType(insn.asInstanceOf[InvokeDynamicInsnNode].desc)
      case _ => getReturnType(insn.asInstanceOf[MethodInsnNode].desc)
    }
  }
  def returnOperation(insn: AbstractInsnNode, value: Inferred, expected: Inferred) = {}
  def merge(v1: Inferred, v2: Inferred) = v1
}
