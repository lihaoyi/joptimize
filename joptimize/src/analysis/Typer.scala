package joptimize.analysis
import java.util

import joptimize.model.{CType, IType, JType, SSA}

/**
  * Performs type inference on a single SSA node, given the types of its upstream
  * dependencies.
  */
class Typer(merge0: Seq[IType] => IType){
  def visitSSA(ssa: SSA.Val, inferred: util.IdentityHashMap[SSA.Val, IType]): IType = ssa match{
    case SSA.Arg(index, typeSize) => inferred.get(ssa)
    case SSA.BinOp(a, b, opcode) =>
      val v1 = inferred.get(a)
      val v2 = inferred.get(b)
      opcode match{
        case SSA.BinOp.IADD => fold(v1, v2, CType.I)(_ + _)
        case SSA.BinOp.ISUB => fold(v1, v2, CType.I)(_ - _)
        case SSA.BinOp.IMUL => fold(v1, v2, CType.I)(_ * _)
        case SSA.BinOp.IDIV => fold(v1, v2, CType.I)(_ / _)
        case SSA.BinOp.IREM => fold(v1, v2, CType.I)(_ % _)
        case SSA.BinOp.ISHL => fold(v1, v2, CType.I)(_ << _)
        case SSA.BinOp.ISHR => fold(v1, v2, CType.I)(_ >> _)
        case SSA.BinOp.IUSHR => fold(v1, v2, CType.I)(_ >>> _)
        case SSA.BinOp.IAND => fold(v1, v2, CType.I)(_ & _)
        case SSA.BinOp.IOR => fold(v1, v2, CType.I)(_ | _)
        case SSA.BinOp.IXOR => fold(v1, v2, CType.I)(_ ^ _)

        case SSA.BinOp.FADD => fold(v1, v2, CType.F)(_ + _)
        case SSA.BinOp.FSUB => fold(v1, v2, CType.F)(_ - _)
        case SSA.BinOp.FMUL => fold(v1, v2, CType.F)(_ * _)
        case SSA.BinOp.FDIV => fold(v1, v2, CType.F)(_ / _)
        case SSA.BinOp.FREM => fold(v1, v2, CType.F)(_ % _)

        case SSA.BinOp.LADD => fold(v1, v2, CType.J)(_ + _)
        case SSA.BinOp.LSUB => fold(v1, v2, CType.J)(_ - _)
        case SSA.BinOp.LMUL => fold(v1, v2, CType.J)(_ * _)
        case SSA.BinOp.LDIV => fold(v1, v2, CType.J)(_ / _)
        case SSA.BinOp.LREM => fold(v1, v2, CType.J)(_ % _)
        case SSA.BinOp.LSHL => fold(v1, v2, CType.J)(_ << _)
        case SSA.BinOp.LSHR => fold(v1, v2, CType.J)(_ >> _)
        case SSA.BinOp.LUSHR => fold(v1, v2, CType.J)(_ >>> _)
        case SSA.BinOp.LAND => fold(v1, v2, CType.J)(_ & _)
        case SSA.BinOp.LOR => fold(v1, v2, CType.J)(_ | _)
        case SSA.BinOp.LXOR => fold(v1, v2, CType.J)(_ ^ _)

        case SSA.BinOp.DADD => fold(v1, v2, CType.D)(_ + _)
        case SSA.BinOp.DSUB => fold(v1, v2, CType.D)(_ - _)
        case SSA.BinOp.DMUL => fold(v1, v2, CType.D)(_ * _)
        case SSA.BinOp.DDIV => fold(v1, v2, CType.D)(_ / _)
        case SSA.BinOp.DREM => fold(v1, v2, CType.D)(_ % _)

        case SSA.BinOp.LCMP => fold(v1, v2, CType.J)(java.lang.Long.compare)
        case SSA.BinOp.FCMPL => fold(v1, v2, CType.F)((f1, f2) =>
          if (java.lang.Float.isNaN(f1) || java.lang.Float.isNaN(f2)) -1
          else java.lang.Float.compare(f1, f2)
        )
        case SSA.BinOp.FCMPG => fold(v1, v2, CType.F)((f1, f2) =>
          if (java.lang.Float.isNaN(f1) || java.lang.Float.isNaN(f2)) 1
          else java.lang.Float.compare(f1, f2)
        )
        case SSA.BinOp.DCMPL => fold(v1, v2, CType.D)((f1, f2) =>
          if (java.lang.Double.isNaN(f1) || java.lang.Double.isNaN(f2)) -1
          else java.lang.Double.compare(f1, f2)
        )
        case SSA.BinOp.DCMPG => fold(v1, v2, CType.D)((f1, f2) =>
          if (java.lang.Double.isNaN(f1) || java.lang.Double.isNaN(f2)) 1
          else java.lang.Double.compare(f1, f2)
        )
      }
    case SSA.UnaOp(a, opcode) =>
      val value = inferred.get(a)
      opcode match{
        case SSA.UnaOp.INEG => fold1(value, CType.I, CType.I)(-_)
        case SSA.UnaOp.L2I => fold1(value, CType.J, CType.I)(_.toInt)
        case SSA.UnaOp.F2I => fold1(value, CType.F, CType.I)(_.toInt)
        case SSA.UnaOp.D2I => fold1(value, CType.D, CType.I)(_.toInt)
        case SSA.UnaOp.I2B => fold1(value, CType.I, CType.I)(_.toByte)
        case SSA.UnaOp.I2C => fold1(value, CType.I, CType.I)(_.toChar)
        case SSA.UnaOp.I2S => fold1(value, CType.J, CType.I)(_.toShort)

        case SSA.UnaOp.FNEG => fold1(value, CType.F, CType.F)(-_)
        case SSA.UnaOp.I2F => fold1(value, CType.I, CType.F)(_.toFloat)
        case SSA.UnaOp.L2F => fold1(value, CType.J, CType.F)(_.toFloat)
        case SSA.UnaOp.D2F => fold1(value, CType.D, CType.F)(_.toFloat)

        case SSA.UnaOp.LNEG => fold1(value, CType.J, CType.J)(-_)
        case SSA.UnaOp.I2L => fold1(value, CType.I, CType.J)(_.toLong)
        case SSA.UnaOp.F2L => fold1(value, CType.F, CType.J)(_.toLong)
        case SSA.UnaOp.D2L => fold1(value, CType.D, CType.J)(_.toLong)

        case SSA.UnaOp.DNEG => fold1(value, CType.D, CType.D)(-_)
        case SSA.UnaOp.I2D => fold1(value, CType.I, CType.D)(_.toDouble)
        case SSA.UnaOp.L2D => fold1(value, CType.J, CType.D)(_.toDouble)
        case SSA.UnaOp.F2D => fold1(value, CType.F, CType.D)(_.toDouble)
      }
    case SSA.UnaBranch(control, a, opcode) => JType.Null
    case SSA.BinBranch(control, a, b, opcode) => JType.Null
    case SSA.ReturnVal(control, a) => JType.Null
    case SSA.AThrow(src) => JType.Null
    case SSA.TableSwitch(src, min, max) => JType.Null
    case SSA.LookupSwitch(src, keys) => JType.Null
    case SSA.CheckCast(src, desc) => desc
    case SSA.ArrayLength(src) => JType.Prim.I
    case SSA.InstanceOf(src, desc) =>
      val cls = inferred.get(src)
      val merged = merge0(Seq(desc, cls))
      if (merged == desc) CType.I(1)
      else if (merged == cls) JType.Prim.Z
      else CType.I(0)

    case SSA.PushI(value) => CType.I(value)
    case SSA.PushJ(value) => CType.J(value)
    case SSA.PushF(value) => CType.F(value)
    case SSA.PushD(value) => CType.D(value)
    case SSA.PushS(value) => JType.Cls("java/lang/String")
    case SSA.PushNull() => JType.Null
    case SSA.PushCls(value) => JType.Cls("java/lang/Class")
    case SSA.New(cls) => inferred.get(ssa)
    case SSA.NewArray(src, typeRef) => typeRef

    case SSA.MultiANewArray(desc, dims) => desc
    case SSA.PutStatic(src, cls, name, desc) => JType.Null
    case SSA.GetStatic(cls, name, desc) => desc
    case SSA.PutField(src, obj, owner, name, desc) => JType.Null
    case SSA.GetField(obj, owner, name, desc) => desc
    case SSA.PutArray(src, indexSrc, array) => JType.Null
    case SSA.GetArray(indexSrc, array, typeSize) => inferred.get(array).asInstanceOf[JType.Arr].innerType
    case SSA.MonitorEnter(indexSrc) => JType.Null
    case SSA.MonitorExit(indexSrc) => JType.Null
  }

  def fold[T](v1: IType, v2: IType, const: CType.ConstantCompanion[T])
             (f: (T, T) => T): IType = {

    (v1, v2) match {
      case (const(i1), const(i2)) => const(f(i1, i2))
      case _ => v1.widen
    }
  }

  def fold1[T, V](v1: IType,
                  const1: CType.ConstantCompanion[T],
                  const2: CType.ConstantCompanion[V])
                 (f: T => V): IType = {

    v1 match {
      case const1(i1) => const2(f(i1))
      case _ => const2.wide
    }
  }
}
