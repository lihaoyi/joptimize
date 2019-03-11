package joptimize.analysis

import joptimize.model._

import scala.collection.mutable
trait Lattice[T]{
  def transferValue(node: SSA.Val, inferences: SSA.Val => T): T
  def join(lhs: T, rhs: T): T
}


case class ITypeLattice(merge: (IType, IType) => IType) extends Lattice[IType]{
  def transferValue(node: SSA.Val, inferences: SSA.Val => IType) = node match{
    case n: SSA.Arg => n.tpe
    case n: SSA.ConstI => CType.I(n.value)
    case n: SSA.ConstJ => CType.J(n.value)
    case n: SSA.ConstF => CType.F(n.value)
    case n: SSA.ConstD => CType.D(n.value)
    case n: SSA.ConstStr => JType.Cls("java/lang/String")
    case n: SSA.ConstNull => JType.Null
    case n: SSA.ConstCls => JType.Cls("java/lang/Class")
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
        case _ => ???
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
        case _ => ???
      }

  }

  override def join(lhs: IType, rhs: IType) = merge(lhs, rhs)
}
object OptimisticAnalyze {
  def apply[T](program: Program,
               initialValues: Map[SSA.Val, T],
               initialBlock: SSA.Block,
               lattice: Lattice[T],
               naming: Namer.Result): (Map[SSA.Val, T], Map[SSA.Block, Boolean]) = {
    val inferredBlocks = mutable.Map(initialBlock -> Map.empty[SSA.Phi, T])
    val inferredValues = mutable.Map(initialValues.toSeq:_*)
    val workList = mutable.LinkedHashSet(initialBlock -> Map.empty[SSA.Phi, T])

    val evaluated = mutable.Map.empty[(Map[SSA.Phi, T], SSA.Val), T]

    def evaluate(env: Map[SSA.Phi, T], v: SSA.Val): T = {
      evaluated.get((env, v)) match{
        case Some(res) => res
        case None => v match{
          case phi: SSA.Phi =>
            if (!env.contains(phi)) pprint.log(phi.incoming)
            env(phi)
          case _ => lattice.transferValue(v, evaluate(env, _))
        }
      }
    }

    while(workList.nonEmpty){
      val current = workList.head
      val (currentBlock, currentEnv) = current
      pprint.log(naming.savedLocals(currentBlock)._2)
      workList.remove(current)

      val Array(nextControl) = currentBlock.downstreamList.collect{case n: SSA.Control => n}
      pprint.log(naming.savedLocals(nextControl)._2)

      def queueNextBlock(nextBlock: SSA.Block) = {

        val phis = nextBlock.downstreamList.collect{case p: SSA.Phi => p}
        val phiMapping = phis
          .flatMap{phi =>
            val exprs = phi
              .incoming
              .collect{case (k, v) if k == currentBlock && !v.isInstanceOf[SSA.State] => v}
              .toSeq
            exprs match{
              case Nil => None
              case Seq(expr) =>
                Some((phi, evaluate(currentEnv, expr)))
            }
          }
          .toMap

        pprint.log(phiMapping)

        inferredBlocks.get(nextBlock) match{
          case Some(existingMapping) =>
            if (phiMapping != existingMapping){
              assert(phiMapping.keySet == existingMapping.keySet)

              val widenedMapping =
                for((k, v) <- existingMapping)
                  yield (k, lattice.join(v, phiMapping(k)))

              inferredBlocks(nextBlock) = widenedMapping
              workList.add(nextBlock -> widenedMapping)
            }
          case None =>
            inferredBlocks(nextBlock) = phiMapping
            workList.add(nextBlock -> phiMapping)
        }
      }
      nextControl match{
        case nextBlock: SSA.Block => queueNextBlock(nextBlock)

        case n: SSA.Jump =>
          n match{
            case r: SSA.Return =>

            case r: SSA.ReturnVal =>

            case n: SSA.UnaBranch =>
              val valueA = evaluate(currentEnv, n.a)
              val doBranch = (valueA, n.opcode) match{
                case (CType.I(v), SSA.UnaBranch.IFNE) => Some(v != 0)
                case (CType.I(v), SSA.UnaBranch.IFNE) => Some(v == 0)
                case (CType.I(v), SSA.UnaBranch.IFLE) => Some(v <= 0)
                case (CType.I(v), SSA.UnaBranch.IFLT) => Some(v < 0)
                case (CType.I(v), SSA.UnaBranch.IFGE) => Some(v >= 0)
                case (CType.I(v), SSA.UnaBranch.IFGT) => Some(v > 0)
                case (JType.Null, SSA.UnaBranch.IFNULL) => Some(true)
                case (JType.Null, SSA.UnaBranch.IFNONNULL) => Some(false)
                case _ => None
              }
              doBranch match{
                case None =>
                  queueNextBlock(n.downstreamList.collect{ case t: SSA.True => t}.head)
                  queueNextBlock(n.downstreamList.collect{ case t: SSA.False => t}.head)

                case Some(bool) =>
                  if (bool) queueNextBlock(n.downstreamList.collect{ case t: SSA.True => t}.head)
                  else queueNextBlock(n.downstreamList.collect{ case t: SSA.False => t}.head)
              }
            case n: SSA.BinBranch =>
              val valueA = evaluate(currentEnv, n.a)
              val valueB = evaluate(currentEnv, n.b)

            case n: SSA.TableSwitch =>
            case n: SSA.LookupSwitch =>
          }
      }
//      val newInference = lattice.transferValue(current, inferredValues)
//      if (!inferredValues.get(current).contains(newInference)){
//        current.downstreamList.foreach(workList.add)
//      }
    }

    pprint.log(inferredValues)


    ???
  }
}
