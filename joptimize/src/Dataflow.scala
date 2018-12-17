package joptimize
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.{Handle}
import org.objectweb.asm.tree._

object Dataflow extends Interpreter[IType](ASM4){
  def newValue(tpe: org.objectweb.asm.Type) = {
    if (tpe == null) JType.Null
    else JType.read(tpe.getInternalName)
  }

  def newOperation(insn: AbstractInsnNode) = {
    insn.getOpcode match{
      case ACONST_NULL => JType.Null
      case ICONST_M1 => IType.I(-1)
      case ICONST_0 => IType.I(0)
      case ICONST_1 => IType.I(1)
      case ICONST_2 => IType.I(2)
      case ICONST_3 => IType.I(3)
      case ICONST_4 => IType.I(4)
      case ICONST_5 => IType.I(5)
      case LCONST_0 => IType.J(0)
      case LCONST_1 => IType.J(1)
      case FCONST_0 => IType.F(0)
      case FCONST_1 => IType.F(1)
      case FCONST_2 => IType.F(2)
      case DCONST_0 => IType.D(0)
      case DCONST_1 => IType.D(1)
      case BIPUSH | SIPUSH => IType.I(insn.asInstanceOf[IntInsnNode].operand)
      case LDC =>
        insn.asInstanceOf[LdcInsnNode].cst match{
          case i: java.lang.Integer => IType.I(i)
          case f: java.lang.Float => IType.F(f)
          case d: java.lang.Double => IType.D(d)
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

  def copyOperation(insn: AbstractInsnNode, value: IType) = value

  def unaryOperation(insn: AbstractInsnNode, value: IType) = {
    insn.getOpcode match {
      case INEG => value match{ case IType.I(i) => IType.I(-i) case _ => JType.Prim.I}
      case IINC => value match{ case IType.I(i) => IType.I(i + 1) case _ => JType.Prim.I}
      case L2I => value match{ case IType.J(i) => IType.I(i.toInt) case _ => JType.Prim.I}
      case F2I => value match{ case IType.F(i) => IType.I(i.toInt) case _ => JType.Prim.I}
      case D2I => value match{ case IType.D(i) => IType.I(i.toInt) case _ => JType.Prim.I}
      case I2B => value match{ case IType.I(i) => IType.I(i.toByte) case _ => JType.Prim.I}
      case I2C => value match{ case IType.I(i) => IType.I(i.toChar) case _ => JType.Prim.I}
      case I2S => value match{ case IType.I(i) => IType.I(i.toShort) case _ => JType.Prim.I}

      case FNEG => value match{ case IType.F(i) => IType.F(-i) case _ => JType.Prim.F}
      case I2F => value match{ case IType.I(i) => IType.F(i) case _ => JType.Prim.F}
      case L2F => value match{ case IType.J(i) => IType.F(i) case _ => JType.Prim.F}
      case D2F => value match{ case IType.D(i) => IType.F(i.toFloat) case _ => JType.Prim.F}

      case LNEG => value match{ case IType.J(i) => IType.J(-i) case _ => JType.Prim.J}
      case I2L => value match{ case IType.I(i) => IType.J(i) case _ => JType.Prim.J}
      case F2L => value match{ case IType.F(i) => IType.J(i.toLong) case _ => JType.Prim.J}
      case D2L => value match{ case IType.D(i) => IType.J(i.toLong) case _ => JType.Prim.J}

      case DNEG => value match{ case IType.D(i) => IType.D(-i) case _ => JType.Prim.D}
      case I2D => value match{ case IType.I(i) => IType.D(i) case _ => JType.Prim.D}
      case L2D => value match{ case IType.J(i) => IType.D(-i) case _ => JType.Prim.D}
      case F2D => value match{ case IType.F(i) => IType.D(-i) case _ => JType.Prim.D}
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
  def binaryOperation(insn: AbstractInsnNode, v1: IType, v2: IType) = {
    insn.getOpcode match {
      case IALOAD | BALOAD | CALOAD | SALOAD => JType.Prim.I
      case IADD => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 + i2) case _ => JType.Prim.I}
      case ISUB => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 - i2) case _ => JType.Prim.I}
      case IMUL => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 * i2) case _ => JType.Prim.I}
      case IDIV => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 / i2) case _ => JType.Prim.I}
      case IREM => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 % i2) case _ => JType.Prim.I}
      case ISHL => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 << i2) case _ => JType.Prim.I}
      case ISHR => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 >> i2) case _ => JType.Prim.I}
      case IUSHR => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 >>> i2) case _ => JType.Prim.I}
      case IAND => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 & i2) case _ => JType.Prim.I}
      case IOR => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 | i2) case _ => JType.Prim.I}
      case IXOR => (v1, v2) match{ case (IType.I(i1), IType.I(i2)) => IType.I(i1 ^ i2) case _ => JType.Prim.I}

      case FALOAD => JType.Prim.F
      case FADD => (v1, v2) match{ case (IType.F(i1), IType.F(i2)) => IType.F(i1 + i2) case _ => JType.Prim.F}
      case FSUB => (v1, v2) match{ case (IType.F(i1), IType.F(i2)) => IType.F(i1 - i2) case _ => JType.Prim.F}
      case FMUL => (v1, v2) match{ case (IType.F(i1), IType.F(i2)) => IType.F(i1 * i2) case _ => JType.Prim.F}
      case FDIV => (v1, v2) match{ case (IType.F(i1), IType.F(i2)) => IType.F(i1 / i2) case _ => JType.Prim.F}
      case FREM =>  (v1, v2) match{ case (IType.F(i1), IType.F(i2)) => IType.F(i1 % i2) case _ => JType.Prim.F}

      case LALOAD => JType.Prim.J
      case LADD => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 + i2) case _ => JType.Prim.J}
      case LSUB => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 - i2) case _ => JType.Prim.J}
      case LMUL => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 * i2) case _ => JType.Prim.J}
      case LDIV => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 / i2) case _ => JType.Prim.J}
      case LREM => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 % i2) case _ => JType.Prim.J}
      case LSHL => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 << i2) case _ => JType.Prim.J}
      case LSHR => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 >> i2) case _ => JType.Prim.J}
      case LUSHR => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 >>> i2) case _ => JType.Prim.J}
      case LAND => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 & i2) case _ => JType.Prim.J}
      case LOR => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 | i2) case _ => JType.Prim.J}
      case LXOR => (v1, v2) match{ case (IType.J(i1), IType.J(i2)) => IType.J(i1 ^ i2) case _ => JType.Prim.J}

      case DALOAD => JType.Prim.D
      case DADD => (v1, v2) match{ case (IType.D(i1), IType.D(i2)) => IType.D(i1 + i2) case _ => JType.Prim.D}
      case DSUB => (v1, v2) match{ case (IType.D(i1), IType.D(i2)) => IType.D(i1 - i2) case _ => JType.Prim.D}
      case DMUL => (v1, v2) match{ case (IType.D(i1), IType.D(i2)) => IType.D(i1 * i2) case _ => JType.Prim.D}
      case DDIV => (v1, v2) match{ case (IType.D(i1), IType.D(i2)) => IType.D(i1 / i2) case _ => JType.Prim.D}
      case DREM => (v1, v2) match{ case (IType.D(i1), IType.D(i2)) => IType.D(i1 % i2) case _ => JType.Prim.D}

      case AALOAD => v1.asInstanceOf[JType.Arr].innerType
      case LCMP | FCMPL | FCMPG | DCMPL | DCMPG => JType.Prim.I
      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE | PUTFIELD =>
        JType.Null
    }
  }
  def ternaryOperation(insn: AbstractInsnNode, v1: IType, v2: IType, v3: IType) = {
    JType.Null
  }
  def naryOperation(insn: AbstractInsnNode, vs: java.util.List[_ <: IType]) = {
    insn.getOpcode match{
      case MULTIANEWARRAY => JType.read(insn.asInstanceOf[MultiANewArrayInsnNode].desc)
      case INVOKEDYNAMIC => Desc.read(insn.asInstanceOf[InvokeDynamicInsnNode].desc).ret
      case _ => Desc.read(insn.asInstanceOf[MethodInsnNode].desc).ret
    }
  }
  def returnOperation(insn: AbstractInsnNode, value: IType, expected: IType) = {}

  // We do not use this, since we do the walking
  // and merging manually in AbstractInterpreter
  def merge(v1: IType, v2: IType) = ???
}
