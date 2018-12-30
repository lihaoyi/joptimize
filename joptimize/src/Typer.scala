package joptimize
import java.util

/**
  * Performs type inference on a single SSA node, given the types of its upstream
  * dependencies.
  */
class Typer(merge0: Seq[IType] => IType){
  def visitSSA(ssa: SSA, inferred: util.IdentityHashMap[SSA, IType]): IType = ssa match{
    case SSA.Arg(index, typeSize) => inferred.get(ssa)
    case SSA.BinOp(a, b, opcode) =>
      val v1 = inferred.get(a)
      val v2 = inferred.get(b)
      opcode match{
        case SSA.BinOp.IADD => fold(v1, v2, IType.I)(_ + _)
        case SSA.BinOp.ISUB => fold(v1, v2, IType.I)(_ - _)
        case SSA.BinOp.IMUL => fold(v1, v2, IType.I)(_ * _)
        case SSA.BinOp.IDIV => fold(v1, v2, IType.I)(_ / _)
        case SSA.BinOp.IREM => fold(v1, v2, IType.I)(_ % _)
        case SSA.BinOp.ISHL => fold(v1, v2, IType.I)(_ << _)
        case SSA.BinOp.ISHR => fold(v1, v2, IType.I)(_ >> _)
        case SSA.BinOp.IUSHR => fold(v1, v2, IType.I)(_ >>> _)
        case SSA.BinOp.IAND => fold(v1, v2, IType.I)(_ & _)
        case SSA.BinOp.IOR => fold(v1, v2, IType.I)(_ | _)
        case SSA.BinOp.IXOR => fold(v1, v2, IType.I)(_ ^ _)

        case SSA.BinOp.FADD => fold(v1, v2, IType.F)(_ + _)
        case SSA.BinOp.FSUB => fold(v1, v2, IType.F)(_ - _)
        case SSA.BinOp.FMUL => fold(v1, v2, IType.F)(_ * _)
        case SSA.BinOp.FDIV => fold(v1, v2, IType.F)(_ / _)
        case SSA.BinOp.FREM => fold(v1, v2, IType.F)(_ % _)

        case SSA.BinOp.LADD => fold(v1, v2, IType.J)(_ + _)
        case SSA.BinOp.LSUB => fold(v1, v2, IType.J)(_ - _)
        case SSA.BinOp.LMUL => fold(v1, v2, IType.J)(_ * _)
        case SSA.BinOp.LDIV => fold(v1, v2, IType.J)(_ / _)
        case SSA.BinOp.LREM => fold(v1, v2, IType.J)(_ % _)
        case SSA.BinOp.LSHL => fold(v1, v2, IType.J)(_ << _)
        case SSA.BinOp.LSHR => fold(v1, v2, IType.J)(_ >> _)
        case SSA.BinOp.LUSHR => fold(v1, v2, IType.J)(_ >>> _)
        case SSA.BinOp.LAND => fold(v1, v2, IType.J)(_ & _)
        case SSA.BinOp.LOR => fold(v1, v2, IType.J)(_ | _)
        case SSA.BinOp.LXOR => fold(v1, v2, IType.J)(_ ^ _)

        case SSA.BinOp.DADD => fold(v1, v2, IType.D)(_ + _)
        case SSA.BinOp.DSUB => fold(v1, v2, IType.D)(_ - _)
        case SSA.BinOp.DMUL => fold(v1, v2, IType.D)(_ * _)
        case SSA.BinOp.DDIV => fold(v1, v2, IType.D)(_ / _)
        case SSA.BinOp.DREM => fold(v1, v2, IType.D)(_ % _)

        case SSA.BinOp.LCMP => fold(v1, v2, IType.J)(java.lang.Long.compare)
        case SSA.BinOp.FCMPL => fold(v1, v2, IType.F)((f1, f2) =>
          if (java.lang.Float.isNaN(f1) || java.lang.Float.isNaN(f2)) -1
          else java.lang.Float.compare(f1, f2)
        )
        case SSA.BinOp.FCMPG => fold(v1, v2, IType.F)((f1, f2) =>
          if (java.lang.Float.isNaN(f1) || java.lang.Float.isNaN(f2)) 1
          else java.lang.Float.compare(f1, f2)
        )
        case SSA.BinOp.DCMPL => fold(v1, v2, IType.D)((f1, f2) =>
          if (java.lang.Double.isNaN(f1) || java.lang.Double.isNaN(f2)) -1
          else java.lang.Double.compare(f1, f2)
        )
        case SSA.BinOp.DCMPG => fold(v1, v2, IType.D)((f1, f2) =>
          if (java.lang.Double.isNaN(f1) || java.lang.Double.isNaN(f2)) 1
          else java.lang.Double.compare(f1, f2)
        )
      }
    case SSA.UnaryOp(a, opcode) =>
      val value = inferred.get(a)
      opcode match{
        case SSA.UnaryOp.INEG => fold1(value, IType.I, IType.I)(-_)
        case SSA.UnaryOp.L2I => fold1(value, IType.J, IType.I)(_.toInt)
        case SSA.UnaryOp.F2I => fold1(value, IType.F, IType.I)(_.toInt)
        case SSA.UnaryOp.D2I => fold1(value, IType.D, IType.I)(_.toInt)
        case SSA.UnaryOp.I2B => fold1(value, IType.I, IType.I)(_.toByte)
        case SSA.UnaryOp.I2C => fold1(value, IType.I, IType.I)(_.toChar)
        case SSA.UnaryOp.I2S => fold1(value, IType.J, IType.I)(_.toShort)

        case SSA.UnaryOp.FNEG => fold1(value, IType.F, IType.F)(-_)
        case SSA.UnaryOp.I2F => fold1(value, IType.I, IType.F)(_.toFloat)
        case SSA.UnaryOp.L2F => fold1(value, IType.J, IType.F)(_.toFloat)
        case SSA.UnaryOp.D2F => fold1(value, IType.D, IType.F)(_.toFloat)

        case SSA.UnaryOp.LNEG => fold1(value, IType.J, IType.J)(-_)
        case SSA.UnaryOp.I2L => fold1(value, IType.I, IType.J)(_.toLong)
        case SSA.UnaryOp.F2L => fold1(value, IType.F, IType.J)(_.toLong)
        case SSA.UnaryOp.D2L => fold1(value, IType.D, IType.J)(_.toLong)

        case SSA.UnaryOp.DNEG => fold1(value, IType.D, IType.D)(-_)
        case SSA.UnaryOp.I2D => fold1(value, IType.I, IType.D)(_.toDouble)
        case SSA.UnaryOp.L2D => fold1(value, IType.J, IType.D)(_.toDouble)
        case SSA.UnaryOp.F2D => fold1(value, IType.F, IType.D)(_.toDouble)
      }
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
