package joptimize
import org.objectweb.asm.Handle
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis._

class LValue(val size: Int, val insn: Option[AbstractInsnNode], val upstream: Seq[LValue]) extends Value{
  def getSize = size
}
object Liveness extends Interpreter[LValue](ASM4){
  def newValue(tpe: org.objectweb.asm.Type) = {
    if (tpe == null) new LValue(1, None, Nil)
    else new LValue(tpe.getSize, None, Nil)
  }

  def newOperation(insn: AbstractInsnNode) = {
    insn.getOpcode match{
      case ACONST_NULL => new LValue(1, Some(insn), Nil)
      case ICONST_M1 => new LValue(1, Some(insn), Nil)
      case ICONST_0 => new LValue(1, Some(insn), Nil)
      case ICONST_1 => new LValue(1, Some(insn), Nil)
      case ICONST_2 => new LValue(1, Some(insn), Nil)
      case ICONST_3 => new LValue(1, Some(insn), Nil)
      case ICONST_4 => new LValue(1, Some(insn), Nil)
      case ICONST_5 => new LValue(1, Some(insn), Nil)
      case LCONST_0 => new LValue(2, Some(insn), Nil)
      case LCONST_1 => new LValue(2, Some(insn), Nil)
      case FCONST_0 => new LValue(1, Some(insn), Nil)
      case FCONST_1 => new LValue(1, Some(insn), Nil)
      case FCONST_2 => new LValue(1, Some(insn), Nil)
      case DCONST_0 => new LValue(1, Some(insn), Nil)
      case DCONST_1 => new LValue(1, Some(insn), Nil)
      case BIPUSH | SIPUSH => new LValue(1, Some(insn), Nil)
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match{
          case i: java.lang.Integer => new LValue(1, Some(insn), Nil)
          case f: java.lang.Float => new LValue(1, Some(insn), Nil)
          case d: java.lang.Double => new LValue(2, Some(insn), Nil)
          case _: java.lang.String => new LValue(1, Some(insn), Nil)
          case value: org.objectweb.asm.Type =>
            value.getSort match{
              case OBJECT | ARRAY => new LValue(1, Some(insn), Nil)
              case METHOD => new LValue(1, Some(insn), Nil)
            }
          case _: Handle => new LValue(1, Some(insn), Nil)
        }
      case JSR => new LValue(1, Some(insn), Nil)
      case GETSTATIC => new LValue(JType.read(insn.asInstanceOf[FieldInsnNode].desc).size, Some(insn), Nil)
      case NEW => new LValue(1, Some(insn), Nil)
    }
  }

  def copyOperation(insn: AbstractInsnNode, value: LValue) = new LValue(value.size, Some(insn), Seq(value))

  def unaryOperation(insn: AbstractInsnNode, value: LValue) = {

    insn.getOpcode match {
      case INEG => new LValue(1, Some(insn), Seq(value))
      case IINC => new LValue(1, Some(insn), Seq(value))
      case L2I => new LValue(1, Some(insn), Seq(value))
      case F2I => new LValue(1, Some(insn), Seq(value))
      case D2I => new LValue(1, Some(insn), Seq(value))
      case I2B => new LValue(1, Some(insn), Seq(value))
      case I2C => new LValue(1, Some(insn), Seq(value))
      case I2S => new LValue(1, Some(insn), Seq(value))

      case FNEG => new LValue(1, Some(insn), Seq(value))
      case I2F => new LValue(1, Some(insn), Seq(value))
      case L2F => new LValue(1, Some(insn), Seq(value))
      case D2F => new LValue(1, Some(insn), Seq(value))

      case LNEG => new LValue(2, Some(insn), Seq(value))
      case I2L => new LValue(2, Some(insn), Seq(value))
      case F2L => new LValue(2, Some(insn), Seq(value))
      case D2L => new LValue(2, Some(insn), Seq(value))

      case DNEG => new LValue(2, Some(insn), Seq(value))
      case I2D => new LValue(2, Some(insn), Seq(value))
      case L2D => new LValue(2, Some(insn), Seq(value))
      case F2D => new LValue(2, Some(insn), Seq(value))
      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | TABLESWITCH | LOOKUPSWITCH |
           IRETURN | LRETURN | FRETURN | DRETURN | ARETURN | PUTSTATIC =>
        new LValue(1, Some(insn), Seq(value))
      case GETFIELD => new LValue(JType.read(insn.asInstanceOf[FieldInsnNode].desc).size, Some(insn), Seq(value))
      case NEWARRAY => new LValue(1, Some(insn), Seq(value))
      case ANEWARRAY => new LValue(1, Some(insn), Seq(value))
      case ARRAYLENGTH => new LValue(1, Some(insn), Seq(value))
      case ATHROW => new LValue(1, Some(insn), Seq(value))
      case CHECKCAST => new LValue(1, Some(insn), Seq(value))
      case INSTANCEOF => new LValue(1, Some(insn), Seq(value))
      case MONITORENTER | MONITOREXIT | IFNULL | IFNONNULL => new LValue(1, Some(insn), Seq(value))
    }
  }
  def binaryOperation(insn: AbstractInsnNode, v1: LValue, v2: LValue) = {
    insn.getOpcode match {
      case IALOAD | BALOAD | CALOAD | SALOAD => new LValue(1, Some(insn), Seq(v1, v2))
      case IADD => new LValue(1, Some(insn), Seq(v1, v2))
      case ISUB => new LValue(1, Some(insn), Seq(v1, v2))
      case IMUL => new LValue(1, Some(insn), Seq(v1, v2))
      case IDIV => new LValue(1, Some(insn), Seq(v1, v2))
      case IREM => new LValue(1, Some(insn), Seq(v1, v2))
      case ISHL => new LValue(1, Some(insn), Seq(v1, v2))
      case ISHR => new LValue(1, Some(insn), Seq(v1, v2))
      case IUSHR => new LValue(1, Some(insn), Seq(v1, v2))
      case IAND => new LValue(1, Some(insn), Seq(v1, v2))
      case IOR => new LValue(1, Some(insn), Seq(v1, v2))
      case IXOR => new LValue(1, Some(insn), Seq(v1, v2))

      case FALOAD => new LValue(1, Some(insn), Seq(v1, v2))
      case FADD => new LValue(1, Some(insn), Seq(v1, v2))
      case FSUB => new LValue(1, Some(insn), Seq(v1, v2))
      case FMUL => new LValue(1, Some(insn), Seq(v1, v2))
      case FDIV => new LValue(1, Some(insn), Seq(v1, v2))
      case FREM =>  new LValue(1, Some(insn), Seq(v1, v2))

      case LALOAD => new LValue(2, Some(insn), Seq(v1, v2))
      case LADD => new LValue(2, Some(insn), Seq(v1, v2))
      case LSUB => new LValue(2, Some(insn), Seq(v1, v2))
      case LMUL => new LValue(2, Some(insn), Seq(v1, v2))
      case LDIV => new LValue(2, Some(insn), Seq(v1, v2))
      case LREM => new LValue(2, Some(insn), Seq(v1, v2))
      case LSHL => new LValue(2, Some(insn), Seq(v1, v2))
      case LSHR => new LValue(2, Some(insn), Seq(v1, v2))
      case LUSHR => new LValue(2, Some(insn), Seq(v1, v2))
      case LAND => new LValue(2, Some(insn), Seq(v1, v2))
      case LOR => new LValue(2, Some(insn), Seq(v1, v2))
      case LXOR => new LValue(2, Some(insn), Seq(v1, v2))

      case DALOAD => new LValue(2, Some(insn), Seq(v1, v2))
      case DADD => new LValue(2, Some(insn), Seq(v1, v2))
      case DSUB => new LValue(2, Some(insn), Seq(v1, v2))
      case DMUL => new LValue(2, Some(insn), Seq(v1, v2))
      case DDIV => new LValue(2, Some(insn), Seq(v1, v2))
      case DREM => new LValue(2, Some(insn), Seq(v1, v2))

      case AALOAD => new LValue(1, Some(insn), Seq(v1, v2))
      case LCMP | FCMPL | FCMPG | DCMPL | DCMPG => new LValue(1, Some(insn), Seq(v1, v2))
      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE | PUTFIELD =>
        new LValue(1, Some(insn), Seq(v1, v2))
    }
  }
  def ternaryOperation(insn: AbstractInsnNode, v1: LValue, v2: LValue, v3: LValue): LValue = {
    new LValue(1, Some(insn), Seq(v1, v2, v3))
  }
  def naryOperation(insn: AbstractInsnNode, vs: java.util.List[_ <: LValue]) = {
    import collection.JavaConverters._

    insn.getOpcode match{
      case MULTIANEWARRAY => new LValue(1, Some(insn), vs.asScala)
      case INVOKEDYNAMIC => ???
      case _ => new LValue( Desc.read(insn.asInstanceOf[MethodInsnNode].desc).ret.size, Some(insn), vs.asScala)
    }
  }
  def returnOperation(insn: AbstractInsnNode, value: LValue, expected: LValue) = {}

  // We do not use this, since we do the walking
  // and merging manually in AbstractInterpreter
  def merge(v1: LValue, v2: LValue) = ???
}
