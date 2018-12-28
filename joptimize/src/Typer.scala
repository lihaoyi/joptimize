package joptimize
import java.util

import org.objectweb.asm.Opcodes._

/**
  * Performs type inference on a single SSA node, given the types of its upstream
  * dependencies.
  */
class Typer(merge0: Seq[IType] => IType){
  def visitSSA(ssa: SSA, inferred: util.IdentityHashMap[SSA, IType]): IType = ssa match{
    case SSA.Arg(index, typeSize) => inferred.get(ssa)
    case SSA.BinOp(a, b, opcode, typeSize) =>
      val v1 = inferred.get(a)
      val v2 = inferred.get(b)
      opcode match{
        case IADD => fold(v1, v2, IType.I)(_ + _)
        case ISUB => fold(v1, v2, IType.I)(_ - _)
        case IMUL => fold(v1, v2, IType.I)(_ * _)
        case IDIV => fold(v1, v2, IType.I)(_ / _)
        case IREM => fold(v1, v2, IType.I)(_ % _)
        case ISHL => fold(v1, v2, IType.I)(_ << _)
        case ISHR => fold(v1, v2, IType.I)(_ >> _)
        case IUSHR => fold(v1, v2, IType.I)(_ >>> _)
        case IAND => fold(v1, v2, IType.I)(_ & _)
        case IOR => fold(v1, v2, IType.I)(_ | _)
        case IXOR => fold(v1, v2, IType.I)(_ ^ _)

        case FADD => fold(v1, v2, IType.F)(_ + _)
        case FSUB => fold(v1, v2, IType.F)(_ - _)
        case FMUL => fold(v1, v2, IType.F)(_ * _)
        case FDIV => fold(v1, v2, IType.F)(_ / _)
        case FREM => fold(v1, v2, IType.F)(_ % _)

        case LADD => fold(v1, v2, IType.J)(_ + _)
        case LSUB => fold(v1, v2, IType.J)(_ - _)
        case LMUL => fold(v1, v2, IType.J)(_ * _)
        case LDIV => fold(v1, v2, IType.J)(_ / _)
        case LREM => fold(v1, v2, IType.J)(_ % _)
        case LSHL => fold(v1, v2, IType.J)(_ << _)
        case LSHR => fold(v1, v2, IType.J)(_ >> _)
        case LUSHR => fold(v1, v2, IType.J)(_ >>> _)
        case LAND => fold(v1, v2, IType.J)(_ & _)
        case LOR => fold(v1, v2, IType.J)(_ | _)
        case LXOR => fold(v1, v2, IType.J)(_ ^ _)

        case DADD => fold(v1, v2, IType.D)(_ + _)
        case DSUB => fold(v1, v2, IType.D)(_ - _)
        case DMUL => fold(v1, v2, IType.D)(_ * _)
        case DDIV => fold(v1, v2, IType.D)(_ / _)
        case DREM => fold(v1, v2, IType.D)(_ % _)
      }
    case SSA.UnaryOp(a, opcode, typeSize) =>
      val value = inferred.get(a)
      opcode match{
        case INEG => fold1(value, IType.I, IType.I)(-_)
        case L2I => fold1(value, IType.J, IType.I)(_.toInt)
        case F2I => fold1(value, IType.F, IType.I)(_.toInt)
        case D2I => fold1(value, IType.D, IType.I)(_.toInt)
        case I2B => fold1(value, IType.I, IType.I)(_.toByte)
        case I2C => fold1(value, IType.I, IType.I)(_.toChar)
        case I2S => fold1(value, IType.J, IType.I)(_.toShort)

        case FNEG => fold1(value, IType.F, IType.F)(-_)
        case I2F => fold1(value, IType.I, IType.F)(_.toFloat)
        case L2F => fold1(value, IType.J, IType.F)(_.toFloat)
        case D2F => fold1(value, IType.D, IType.F)(_.toFloat)

        case LNEG => fold1(value, IType.J, IType.J)(-_)
        case I2L => fold1(value, IType.I, IType.J)(_.toLong)
        case F2L => fold1(value, IType.F, IType.J)(_.toLong)
        case D2L => fold1(value, IType.D, IType.J)(_.toLong)

        case DNEG => fold1(value, IType.D, IType.D)(-_)
        case I2D => fold1(value, IType.I, IType.D)(_.toDouble)
        case L2D => fold1(value, IType.J, IType.D)(_.toDouble)
        case F2D => fold1(value, IType.F, IType.D)(_.toDouble)
      }
    case SSA.Inc(a, increment) => JType.Null
    case SSA.UnaryBranch(a, target, opcode) => JType.Null
    case SSA.BinBranch(a, b, target, opcode) => JType.Null
    case SSA.ReturnVal(a) => JType.Null
    case SSA.AThrow(src) => JType.Null
    case SSA.TableSwitch(src, min, max, default, targets) => JType.Null
    case SSA.LookupSwitch(src, default, keys, tartargets) => JType.Null
    case SSA.Goto(target) => JType.Null
    case SSA.CheckCast(src, desc) => desc
    case SSA.ArrayLength(src) => JType.Prim.I
    case SSA.InstanceOf(src, desc) =>
      val cls = inferred.get(src)
      val merged = merge0(Seq(desc, cls))
      if (merged == desc) IType.I(1)
      else if (merged == cls) JType.Prim.Z
      else IType.I(0)

    case SSA.PushI(value) => IType.I(value)
    case SSA.PushJ(value) => IType.J(value)
    case SSA.PushF(value) => IType.F(value)
    case SSA.PushD(value) => IType.D(value)
    case SSA.PushS(value) => JType.Cls("java/lang/String")
    case SSA.PushNull() => JType.Null
    case SSA.PushCls(value) => JType.Cls("java/lang/Class")
    case SSA.New(cls) => inferred.get(ssa)
    case SSA.NewArray(src, typeRef) => typeRef

    case SSA.MultiANewArray(desc, dims) => desc
    case SSA.PutStatic(state, src, cls, name, desc) => JType.Null
    case SSA.GetStatic(state, cls, name, desc) => desc
    case SSA.PutField(state, src, obj, owner, name, desc) => JType.Null
    case SSA.GetField(state, obj, owner, name, desc) => desc
    case SSA.PutArray(state, src, indexSrc, array) => JType.Null
    case SSA.GetArray(state, indexSrc, array, typeSize) => inferred.get(array).asInstanceOf[JType.Arr].innerType
    case SSA.MonitorEnter(indexSrc) => JType.Null
    case SSA.MonitorExit(indexSrc) => JType.Null
  }

  def fold[T](v1: IType, v2: IType, const: IType.ConstantCompanion[T])
             (f: (T, T) => T): IType = {

    (v1, v2) match {
      case (const(i1), const(i2)) => const(f(i1, i2))
      case _ => v1.widen
    }
  }

  def fold1[T, V](v1: IType,
                  const1: IType.ConstantCompanion[T],
                  const2: IType.ConstantCompanion[V])
                 (f: T => V): IType = {

    v1 match {
      case const1(i1) => const2(f(i1))
      case _ => const2.wide
    }
  }
}
