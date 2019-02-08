package joptimize.analysis

import joptimize.model.{Program, SSA}

import scala.collection.mutable

object PartialEvaluator {
  def apply(program: Program) = {
    println("PartialEvaluator.apply")
    val queue = program.getAllVertices()
      .to[mutable.LinkedHashSet]

    while (queue.nonEmpty) {
      val current = queue.head
      queue.remove(current)
      current match{

        case phi: SSA.Phi if phi.getSize != 0 =>
          //          b
          //         /
          // a -- phi -- c
          //         \
          //          d
          //    b
          //   /
          // a -- c
          //   \
          //    d
          val filteredValues = phi.incoming.filter(_._2 != phi)

          if (filteredValues.map(_._2).size == 1) {
            val replacement = filteredValues.head._2
            for (v <- current.upstream) v.downstreamRemoveAll(current)
            val deltaDownstream = current.downstreamList.filter(_ != current)
            deltaDownstream.foreach(replacement.downstreamAdd)

            for (down <- deltaDownstream) SSA.update(down, current, replacement)
            queue.add(replacement)
            replacement.downstreamList.foreach(queue.add)
          }

        case current: SSA.Val =>
          val replacement = evaluateVal(current)
          if (replacement ne current){
            for (v <- current.upstream) v.downstreamRemoveAll(current)
            val deltaDownstream = current.downstreamList.filter(_ != current)
            deltaDownstream.foreach(replacement.downstreamAdd)

            for (down <- deltaDownstream) {
              SSA.update(down, current, replacement)
              queue.add(down)
            }
          }
        case current: SSA.Jump =>
          val directNextOpt = evaluateJump(current)

          for(directNext <- directNextOpt){
            //                     c
            //                    /
            //       a        TRUE - d -
            //        \      /    \      \
            // block - branch      ----- phi
            //        /      \          / |
            //       b        false ----  |
            //                     \      /
            //       c              e ----
            //      /
            // block - d
            //      \   \
            //       --- phi
            for(down <- directNext.downstreamList){
              SSA.update(down, directNext, current.block)
              current.block.downstreamAdd(down)
            }

            for(up <- current.upstream){
              up.downstreamRemove(current)
            }

            val branchBlocks = current.downstreamList.toSet

            branchBlocks.flatMap(_.downstreamList).foreach{
              case phi: SSA.Phi =>
                phi.incoming = phi.incoming.flatMap(x =>
                  if (x._1 == directNext) Some(current.block -> x._2)
                  else if (branchBlocks(x._1)) None
                  else Some(x)
                )
                queue.add(phi)

              case r: SSA.Merge =>
                r.incoming = r.incoming.flatMap{ x =>
                  if (x == directNext) Some(current.block)
                  else if (branchBlocks(x)) None
                  else Some(x)
                }
                queue.add(r)
            }
          }

        case reg: SSA.Merge =>
          if (reg.incoming.size == 1) {
            val replacement = reg.incoming.head
            for (v <- current.upstream) v.downstreamRemoveAll(current)
            val deltaDownstream = current.downstreamList.filter(_ != current)
            deltaDownstream.foreach(replacement.downstreamAdd)

            for (down <- deltaDownstream) SSA.update(down, current, replacement)
            queue.add(replacement)
          }

        case _ =>
      }
    }
  }

  def evaluateJump(current: SSA.Jump): Option[SSA.SimpleBlock] = {
    def findDownstreamBlock(boolean: Boolean) = {
      if (boolean) current.downstreamList.collectFirst { case n: SSA.True => n }
      else current.downstreamList.collectFirst { case n: SSA.False => n }
    }

    current match {
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

  def evaluateVal(s: SSA.Val): SSA.Val = s match {

    case n: SSA.BinOp =>
      (n.a, n.b) match {
        case (a: SSA.ConstI, b: SSA.ConstI) =>
          SSA.ConstI(
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
        case (a: SSA.ConstJ, b: SSA.ConstJ) =>

          n.opcode match {
            case SSA.BinOp.LADD => SSA.ConstJ(a.value + b.value)
            case SSA.BinOp.LSUB => SSA.ConstJ(a.value - b.value)
            case SSA.BinOp.LMUL => SSA.ConstJ(a.value * b.value)
            case SSA.BinOp.LDIV => SSA.ConstJ(a.value / b.value)
            case SSA.BinOp.LREM => SSA.ConstJ(a.value % b.value)

            case SSA.BinOp.LAND => SSA.ConstJ(a.value & b.value)
            case SSA.BinOp.LOR => SSA.ConstJ(a.value | b.value)
            case SSA.BinOp.LXOR => SSA.ConstJ(a.value ^ b.value)

            case SSA.BinOp.LCMP => SSA.ConstI(java.lang.Long.compare(a.value, b.value))
          }
        case (a: SSA.ConstJ, b: SSA.ConstI) =>
          SSA.ConstJ(
            n.opcode match {
              case SSA.BinOp.LSHL => a.value << b.value
              case SSA.BinOp.LSHR => a.value >> b.value
              case SSA.BinOp.LUSHR => a.value >> b.value
            }
          )
        case (a: SSA.ConstF, b: SSA.ConstF) =>

          n.opcode match {
            case SSA.BinOp.FADD => SSA.ConstF(a.value + b.value)
            case SSA.BinOp.FSUB => SSA.ConstF(a.value - b.value)
            case SSA.BinOp.FMUL => SSA.ConstF(a.value * b.value)
            case SSA.BinOp.FDIV => SSA.ConstF(a.value / b.value)
            case SSA.BinOp.FREM => SSA.ConstF(a.value % b.value)

            case SSA.BinOp.FCMPL => SSA.ConstI(
              if (java.lang.Float.isNaN(a.value) || java.lang.Float.isNaN(b.value)) -1
              else java.lang.Float.compare(a.value, b.value)
            )
            case SSA.BinOp.FCMPG => SSA.ConstI(
              if (java.lang.Float.isNaN(a.value) || java.lang.Float.isNaN(b.value)) 1
              else java.lang.Float.compare(a.value, b.value)
            )
          }

        case (a: SSA.ConstD, b: SSA.ConstD) =>

          n.opcode match {
            case SSA.BinOp.DADD => SSA.ConstD(a.value + b.value)
            case SSA.BinOp.DSUB => SSA.ConstD(a.value - b.value)
            case SSA.BinOp.DMUL => SSA.ConstD(a.value * b.value)
            case SSA.BinOp.DDIV => SSA.ConstD(a.value / b.value)
            case SSA.BinOp.DREM => SSA.ConstD(a.value % b.value)

            case SSA.BinOp.DCMPL => SSA.ConstI(
              if (java.lang.Double.isNaN(a.value) || java.lang.Double.isNaN(b.value)) -1
              else java.lang.Double.compare(a.value, b.value)
            )
            case SSA.BinOp.DCMPG => SSA.ConstI(
              if (java.lang.Double.isNaN(a.value) || java.lang.Double.isNaN(b.value)) 1
              else java.lang.Double.compare(a.value, b.value)
            )
          }
        case _ => s
      }
    case n: SSA.UnaOp =>

      n.a match {
        case a: SSA.ConstI =>
          n.opcode match {
            case SSA.UnaOp.INEG => SSA.ConstI(-a.value)
            case SSA.UnaOp.I2B => SSA.ConstI(a.value.toByte)
            case SSA.UnaOp.I2C => SSA.ConstI(a.value.toChar)
            case SSA.UnaOp.I2S => SSA.ConstI(a.value.toShort)
            case SSA.UnaOp.I2L => SSA.ConstJ(a.value)
            case SSA.UnaOp.I2F => SSA.ConstF(a.value.toFloat)
            case SSA.UnaOp.I2D => SSA.ConstD(a.value.toDouble)
          }
        case a: SSA.ConstJ =>
          n.opcode match {
            case SSA.UnaOp.LNEG => SSA.ConstJ(-a.value)
            case SSA.UnaOp.L2I => SSA.ConstI(a.value.toInt)
            case SSA.UnaOp.L2F => SSA.ConstF(a.value.toFloat)
            case SSA.UnaOp.L2D => SSA.ConstD(a.value.toDouble)
          }
        case a: SSA.ConstF =>
          n.opcode match {
            case SSA.UnaOp.FNEG => SSA.ConstF(-a.value)
            case SSA.UnaOp.F2I => SSA.ConstI(a.value.toInt)
            case SSA.UnaOp.F2L => SSA.ConstJ(a.value.toLong)
            case SSA.UnaOp.F2D => SSA.ConstD(a.value)
          }
        case a: SSA.ConstD =>
          n.opcode match {
            case SSA.UnaOp.DNEG => SSA.ConstD(-a.value)
            case SSA.UnaOp.D2I => SSA.ConstI(a.value.toInt)
            case SSA.UnaOp.D2L => SSA.ConstJ(a.value.toLong)
            case SSA.UnaOp.D2F => SSA.ConstF(a.value.toFloat)
          }
        case _ => s
      }
    case _ => s
  }

}
