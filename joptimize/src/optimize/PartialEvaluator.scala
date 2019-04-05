package joptimize.optimize

import joptimize.Util
import joptimize.model.SSA

object PartialEvaluator {

  def replaceJump(current: SSA.Jump,
                  directNext: SSA.Block,
                  liveBlocks: SSA.Block => Boolean) = {
    //                     c
    //                    /
    //       a        TRUE -- d
    //        \      /    \     \
    // block - branch      ---- phi
    //        /      \          / |
    //       b        false ---  |
    //                     \     /
    //                      e----
    Util.replace(directNext, current.block)
    //       --------------------
    //      /      a        TRUE \--- d
    //     /        \      /      \    \
    // block ------- branch        --- phi
    //     \        /      \          / |
    //      c      b        false ----  |
    //                           \      /
    //                            e ----
    current.block.downstreamRemove(current)
    //       --------------------
    //      /      a        TRUE \--- d
    //     /        \      /      \    \
    // block         branch        -- phi
    //     \        /      \          / |
    //      c      b        false ----  |
    //                           \      /
    //                            e ----
    val branchBlocks = current.downstreamList.toSet

    branchBlocks.flatMap(_.downstreamList).collect {
      case phi: SSA.Phi =>
        phi.incoming = phi.incoming.flatMap(x =>
          if (x._1 == directNext) Some(current.block -> x._2)
          else if (branchBlocks(x._1)) {
            x._1.downstreamRemove(phi)
            x._2.downstreamRemove(phi)
            None
          }
          else {
            Some(x)
          }
        )
        phi

      case r: SSA.Merge =>
        r.incoming = r.incoming.flatMap { x =>
          if (x == directNext) Some(current.block)
          else if (branchBlocks(x)) {
            x.downstreamRemove(r)
            None
          }
          else Some(x)
        }
        r
    }
    //       ---------------------
    //      /      a        TRUE  \-- d
    //     /        \      /       \    \
    // block         branch         --- phi
    //     \        /      \
    //      c      b        false - e
    //
    //      d --- phi
    //     /     /
    // block -----
    //     \
    //      c
  }

  def evaluateJump(current: SSA.Jump): Option[SSA.SimpleBlock] = {
    def findDownstreamBlock(boolean: Boolean) = {
      if (boolean) current.downstreamList.collectFirst { case n: SSA.True => n }
      else current.downstreamList.collectFirst { case n: SSA.False => n }
    }

    current match {
      case current: SSA.LookupSwitch =>
        current.src match{
          case const: SSA.ConstI =>
            current.downstreamList.collectFirst{case d: SSA.Case if d.n == const.value => d}.orElse(
              current.downstreamList.collectFirst{case d: SSA.Default => d}
            )
          case _ => None
        }
      case current: SSA.TableSwitch =>
        current.src match{
          case const: SSA.ConstI =>
            current.downstreamList.collectFirst{case d: SSA.Case if d.n == const.value => d}.orElse(
              current.downstreamList.collectFirst{case d: SSA.Default => d}
            )
          case _ => None
        }
      case current: SSA.UnaBranch =>
        current.a match {
          case const: SSA.ConstI =>
            current.opcode match {
              case SSA.UnaBranch.IFEQ => findDownstreamBlock(const.value == 0)
              case SSA.UnaBranch.IFGE => findDownstreamBlock(const.value >= 0)
              case SSA.UnaBranch.IFGT => findDownstreamBlock(const.value > 0)
              case SSA.UnaBranch.IFLE => findDownstreamBlock(const.value <= 0)
              case SSA.UnaBranch.IFLT => findDownstreamBlock(const.value < 0)
              case SSA.UnaBranch.IFNE => findDownstreamBlock(const.value != 0)
              case _ => None
            }
          case _ => None
        }
      case current: SSA.BinBranch =>
        (current.a, current.b) match {
          case (a: SSA.ConstI, b: SSA.ConstI) =>
            current.opcode match {
              case SSA.BinBranch.IF_ICMPEQ => findDownstreamBlock(a.value == b.value)
              case SSA.BinBranch.IF_ICMPGE => findDownstreamBlock(a.value >= b.value)
              case SSA.BinBranch.IF_ICMPGT => findDownstreamBlock(a.value > b.value)
              case SSA.BinBranch.IF_ICMPLE => findDownstreamBlock(a.value <= b.value)
              case SSA.BinBranch.IF_ICMPLT => findDownstreamBlock(a.value < b.value)
              case SSA.BinBranch.IF_ICMPNE => findDownstreamBlock(a.value != b.value)
              case _ => None
            }
          case _ => None
        }
      case current: SSA.TableSwitch => None
      case current: SSA.LookupSwitch => None
      case current: SSA.Return => None
      case current: SSA.ReturnVal => None
    }
  }

  def evaluateVal(s: SSA.Val): Option[SSA.Val] = s match {

    case n: SSA.BinOp =>
      (n.a, n.b) match {
        case (a: SSA.ConstI, b: SSA.ConstI) =>
          Some(new SSA.ConstI(
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
          ))
        case (a: SSA.ConstJ, b: SSA.ConstJ) =>

          Some(n.opcode match {
            case SSA.BinOp.LADD => new SSA.ConstJ(a.value + b.value)
            case SSA.BinOp.LSUB => new SSA.ConstJ(a.value - b.value)
            case SSA.BinOp.LMUL => new SSA.ConstJ(a.value * b.value)
            case SSA.BinOp.LDIV => new SSA.ConstJ(a.value / b.value)
            case SSA.BinOp.LREM => new SSA.ConstJ(a.value % b.value)

            case SSA.BinOp.LAND => new SSA.ConstJ(a.value & b.value)
            case SSA.BinOp.LOR => new SSA.ConstJ(a.value | b.value)
            case SSA.BinOp.LXOR => new SSA.ConstJ(a.value ^ b.value)

            case SSA.BinOp.LCMP => new SSA.ConstI(java.lang.Long.compare(a.value, b.value))
          })
        case (a: SSA.ConstJ, b: SSA.ConstI) =>
          Some(new SSA.ConstJ(
            n.opcode match {
              case SSA.BinOp.LSHL => a.value << b.value
              case SSA.BinOp.LSHR => a.value >> b.value
              case SSA.BinOp.LUSHR => a.value >> b.value
            }
          ))
        case (a: SSA.ConstF, b: SSA.ConstF) =>

          Some(n.opcode match {
            case SSA.BinOp.FADD => new SSA.ConstF(a.value + b.value)
            case SSA.BinOp.FSUB => new SSA.ConstF(a.value - b.value)
            case SSA.BinOp.FMUL => new SSA.ConstF(a.value * b.value)
            case SSA.BinOp.FDIV => new SSA.ConstF(a.value / b.value)
            case SSA.BinOp.FREM => new SSA.ConstF(a.value % b.value)

            case SSA.BinOp.FCMPL => new SSA.ConstI(
              if (java.lang.Float.isNaN(a.value) || java.lang.Float.isNaN(b.value)) -1
              else java.lang.Float.compare(a.value, b.value)
            )
            case SSA.BinOp.FCMPG => new SSA.ConstI(
              if (java.lang.Float.isNaN(a.value) || java.lang.Float.isNaN(b.value)) 1
              else java.lang.Float.compare(a.value, b.value)
            )
          })

        case (a: SSA.ConstD, b: SSA.ConstD) =>

          Some(n.opcode match {
            case SSA.BinOp.DADD => new SSA.ConstD(a.value + b.value)
            case SSA.BinOp.DSUB => new SSA.ConstD(a.value - b.value)
            case SSA.BinOp.DMUL => new SSA.ConstD(a.value * b.value)
            case SSA.BinOp.DDIV => new SSA.ConstD(a.value / b.value)
            case SSA.BinOp.DREM => new SSA.ConstD(a.value % b.value)

            case SSA.BinOp.DCMPL => new SSA.ConstI(
              if (java.lang.Double.isNaN(a.value) || java.lang.Double.isNaN(b.value)) -1
              else java.lang.Double.compare(a.value, b.value)
            )
            case SSA.BinOp.DCMPG => new SSA.ConstI(
              if (java.lang.Double.isNaN(a.value) || java.lang.Double.isNaN(b.value)) 1
              else java.lang.Double.compare(a.value, b.value)
            )
          })
        case _ => None
      }
    case n: SSA.UnaOp =>

      n.a match {
        case a: SSA.ConstI =>
          Some(n.opcode match {
            case SSA.UnaOp.INEG => new SSA.ConstI(-a.value)
            case SSA.UnaOp.I2B => new SSA.ConstI(a.value.toByte)
            case SSA.UnaOp.I2C => new SSA.ConstI(a.value.toChar)
            case SSA.UnaOp.I2S => new SSA.ConstI(a.value.toShort)
            case SSA.UnaOp.I2L => new SSA.ConstJ(a.value)
            case SSA.UnaOp.I2F => new SSA.ConstF(a.value.toFloat)
            case SSA.UnaOp.I2D => new SSA.ConstD(a.value.toDouble)
          })
        case a: SSA.ConstJ =>
          Some(n.opcode match {
            case SSA.UnaOp.LNEG => new SSA.ConstJ(-a.value)
            case SSA.UnaOp.L2I => new SSA.ConstI(a.value.toInt)
            case SSA.UnaOp.L2F => new SSA.ConstF(a.value.toFloat)
            case SSA.UnaOp.L2D => new SSA.ConstD(a.value.toDouble)
          })
        case a: SSA.ConstF =>
          Some(n.opcode match {
            case SSA.UnaOp.FNEG => new SSA.ConstF(-a.value)
            case SSA.UnaOp.F2I => new SSA.ConstI(a.value.toInt)
            case SSA.UnaOp.F2L => new SSA.ConstJ(a.value.toLong)
            case SSA.UnaOp.F2D => new SSA.ConstD(a.value)
          })
        case a: SSA.ConstD =>
          Some(n.opcode match {
            case SSA.UnaOp.DNEG => new SSA.ConstD(-a.value)
            case SSA.UnaOp.D2I => new SSA.ConstI(a.value.toInt)
            case SSA.UnaOp.D2L => new SSA.ConstJ(a.value.toLong)
            case SSA.UnaOp.D2F => new SSA.ConstF(a.value.toFloat)
          })
        case _ => None
      }
    case _ => None
  }

}
