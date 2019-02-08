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
      val replacement = evaluateNode(current)
      if (replacement ne current){
        for (v <- current.upstream) v.downstreamRemoveAll(current)
        val deltaDownstream = current.downstreamList.filter(_ != current)
        deltaDownstream.foreach(replacement.downstreamAdd)

        for (down <- deltaDownstream) {
          SSA.update(down, current, replacement)
          queue.add(down)
        }
      }
    }
  }
  def evaluateNode(s: SSA.Node): SSA.Node = s match {
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
            }
          )
        case (a: SSA.ConstJ, b: SSA.ConstJ) =>
          SSA.ConstJ(
            n.opcode match {
              case SSA.BinOp.LADD => a.value + b.value
              case SSA.BinOp.LSUB => a.value - b.value
              case SSA.BinOp.LMUL => a.value * b.value
              case SSA.BinOp.LDIV => a.value / b.value
              case SSA.BinOp.LREM => a.value % b.value
            }
          )
        case (a: SSA.ConstJ, b: SSA.ConstI) =>
          SSA.ConstJ(
            n.opcode match {
              case SSA.BinOp.LSHL => a.value << b.value
              case SSA.BinOp.LSHR => a.value >> b.value
              case SSA.BinOp.LUSHR => a.value >> b.value
            }
          )
        case (a: SSA.ConstF, b: SSA.ConstF) =>
          SSA.ConstF(
            n.opcode match {
              case SSA.BinOp.FADD => a.value + b.value
              case SSA.BinOp.FSUB => a.value - b.value
              case SSA.BinOp.FMUL => a.value * b.value
              case SSA.BinOp.FDIV => a.value / b.value
              case SSA.BinOp.FREM => a.value % b.value
            }
          )
        case (a: SSA.ConstD, b: SSA.ConstD) =>
          SSA.ConstD(
            n.opcode match {
              case SSA.BinOp.DADD => a.value + b.value
              case SSA.BinOp.DSUB => a.value - b.value
              case SSA.BinOp.DMUL => a.value * b.value
              case SSA.BinOp.DDIV => a.value / b.value
              case SSA.BinOp.DREM => a.value % b.value
            }
          )
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
