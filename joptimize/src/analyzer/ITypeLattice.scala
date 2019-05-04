package joptimize.analyzer

import joptimize.model._

class ITypeLattice(merge: (IType, IType) => IType,
                   inferredArgs: Seq[IType]) extends Lattice[IType]{
  def transferValue(node: SSA.Val, inferences: SSA.Val => IType) = {
    node match{
      case n: SSA.New => n.cls
      case n: SSA.CheckCast => n.desc
      case n: SSA.InstanceOf =>
        val inferredSrc = inferences(n.src)
        val merged = merge(inferredSrc, n.desc)
        if (merged == n.desc) CType.I(1)
        else if (merged == inferredSrc) JType.Prim.Z
        else CType.I(0)

      case n: SSA.ChangedState => JType.Prim.V
      case n: SSA.Arg => inferredArgs(n.index)

      case n: SSA.ConstI => CType.I(n.value)
      case n: SSA.ConstJ => CType.J(n.value)
      case n: SSA.ConstF => CType.F(n.value)
      case n: SSA.ConstD => CType.D(n.value)
      case n: SSA.ConstStr => JType.Cls("java/lang/String")
      case n: SSA.ConstNull => JType.Null
      case n: SSA.ConstCls => JType.Cls("java/lang/Class")

      case n: SSA.ArrayLength => JType.Prim.I

      case n: SSA.GetField => n.desc
      case n: SSA.PutField => JType.Prim.V

      case n: SSA.GetStatic => n.desc
      case n: SSA.PutStatic => JType.Prim.V

      case n: SSA.GetArray => n.tpe
      case n: SSA.PutArray => JType.Prim.V

      case n: SSA.NewArray => JType.Arr(n.typeRef)
      case n: SSA.MultiANewArray => n.desc

      case n: SSA.BinOp =>
        (inferences(n.a), inferences(n.b)) match {
          case (a: CType.I, b: CType.I) =>
            CType.I(
              n.opcode match {
                case SSA.BinOp.IADD => a.value + b.value
                case SSA.BinOp.ISUB => a.value - b.value
                case SSA.BinOp.IMUL => a.value * b.value
                case SSA.BinOp.IDIV => a.value / b.value
                case SSA.BinOp.IREM => a.value % b.value

                case SSA.BinOp.ISHL => a.value << b.value
                case SSA.BinOp.ISHR => a.value >> b.value
                case SSA.BinOp.IUSHR => a.value >> b.value

                case SSA.BinOp.IAND => a.value & b.value
                case SSA.BinOp.IOR => a.value | b.value
                case SSA.BinOp.IXOR => a.value ^ b.value
              }
            )
          case (a: CType.J, b: CType.J) =>

            n.opcode match {
              case SSA.BinOp.LADD => CType.J(a.value + b.value)
              case SSA.BinOp.LSUB => CType.J(a.value - b.value)
              case SSA.BinOp.LMUL => CType.J(a.value * b.value)
              case SSA.BinOp.LDIV => CType.J(a.value / b.value)
              case SSA.BinOp.LREM => CType.J(a.value % b.value)

              case SSA.BinOp.LAND => CType.J(a.value & b.value)
              case SSA.BinOp.LOR => CType.J(a.value | b.value)
              case SSA.BinOp.LXOR => CType.J(a.value ^ b.value)

              case SSA.BinOp.LCMP => CType.I(java.lang.Long.compare(a.value, b.value))
            }
          case (a: CType.J, b: CType.I) =>
            CType.J(
              n.opcode match {
                case SSA.BinOp.LSHL => a.value << b.value
                case SSA.BinOp.LSHR => a.value >> b.value
                case SSA.BinOp.LUSHR => a.value >> b.value
              }
            )
          case (a: CType.F, b: CType.F) =>

            n.opcode match {
              case SSA.BinOp.FADD => CType.F(a.value + b.value)
              case SSA.BinOp.FSUB => CType.F(a.value - b.value)
              case SSA.BinOp.FMUL => CType.F(a.value * b.value)
              case SSA.BinOp.FDIV => CType.F(a.value / b.value)
              case SSA.BinOp.FREM => CType.F(a.value % b.value)

              case SSA.BinOp.FCMPL => CType.I(
                if (java.lang.Float.isNaN(a.value) || java.lang.Float.isNaN(b.value)) -1
                else java.lang.Float.compare(a.value, b.value)
              )
              case SSA.BinOp.FCMPG => CType.I(
                if (java.lang.Float.isNaN(a.value) || java.lang.Float.isNaN(b.value)) 1
                else java.lang.Float.compare(a.value, b.value)
              )
            }

          case (a: CType.D, b: CType.D) =>

            n.opcode match {
              case SSA.BinOp.DADD => CType.D(a.value + b.value)
              case SSA.BinOp.DSUB => CType.D(a.value - b.value)
              case SSA.BinOp.DMUL => CType.D(a.value * b.value)
              case SSA.BinOp.DDIV => CType.D(a.value / b.value)
              case SSA.BinOp.DREM => CType.D(a.value % b.value)

              case SSA.BinOp.DCMPL => CType.I(
                if (java.lang.Double.isNaN(a.value) || java.lang.Double.isNaN(b.value)) -1
                else java.lang.Double.compare(a.value, b.value)
              )
              case SSA.BinOp.DCMPG => CType.I(
                if (java.lang.Double.isNaN(a.value) || java.lang.Double.isNaN(b.value)) 1
                else java.lang.Double.compare(a.value, b.value)
              )
            }
          case _ => n.opcode.tpe
        }
      case n: SSA.UnaOp =>

        inferences(n.a) match {
          case a: CType.I =>
            n.opcode match {
              case SSA.UnaOp.INEG => CType.I(-a.value)
              case SSA.UnaOp.I2B => CType.I(a.value.toByte)
              case SSA.UnaOp.I2C => CType.I(a.value.toChar)
              case SSA.UnaOp.I2S => CType.I(a.value.toShort)
              case SSA.UnaOp.I2L => CType.J(a.value)
              case SSA.UnaOp.I2F => CType.F(a.value.toFloat)
              case SSA.UnaOp.I2D => CType.D(a.value.toDouble)
            }
          case a: CType.J =>
            n.opcode match {
              case SSA.UnaOp.LNEG => CType.J(-a.value)
              case SSA.UnaOp.L2I => CType.I(a.value.toInt)
              case SSA.UnaOp.L2F => CType.F(a.value.toFloat)
              case SSA.UnaOp.L2D => CType.D(a.value.toDouble)
            }
          case a: CType.F =>
            n.opcode match {
              case SSA.UnaOp.FNEG => CType.F(-a.value)
              case SSA.UnaOp.F2I => CType.I(a.value.toInt)
              case SSA.UnaOp.F2L => CType.J(a.value.toLong)
              case SSA.UnaOp.F2D => CType.D(a.value)
            }
          case a: CType.D =>
            n.opcode match {
              case SSA.UnaOp.DNEG => CType.D(-a.value)
              case SSA.UnaOp.D2I => CType.I(a.value.toInt)
              case SSA.UnaOp.D2L => CType.J(a.value.toLong)
              case SSA.UnaOp.D2F => CType.F(a.value.toFloat)
            }
          case _ => n.opcode.tpe
        }

      //    case n: SSA.InvokeDynamic => computeMethodSig(n, n.srcs.map(inferences))
    }
  }

  override def join(lhs: IType, rhs: IType) = {
    val res = merge(lhs, rhs)
    res
  }
}

