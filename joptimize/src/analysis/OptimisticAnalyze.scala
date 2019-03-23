package joptimize.analysis

import joptimize.Util
import joptimize.model._

import scala.collection.mutable
trait Lattice[T]{
  def transferValue(node: SSA.Val, inferences: SSA.Val => T): T
  def join(lhs: T, rhs: T): T
}


class ITypeLattice(merge: (IType, IType) => IType,
                   computeMethodSig: (MethodSig, Boolean, Seq[IType]) => IType,
                   inferredArgs: Seq[IType]) extends Lattice[IType]{
  def transferValue(node: SSA.Val, inferences: SSA.Val => IType) = {
    node.upstream.collect{case v: SSA.Val => inferences(v)}
    node match{
      case n: SSA.New => n.cls
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

      case n: SSA.InvokeStatic => computeMethodSig(n.sig, false, n.srcs.map(inferences))
      case n: SSA.InvokeSpecial => computeMethodSig(n.sig, true, n.srcs.map(inferences))
      case n: SSA.InvokeVirtual => computeMethodSig(n.sig, false, n.srcs.map(inferences))
      case n: SSA.InvokeInterface => computeMethodSig(n.sig, false, n.srcs.map(inferences))
      //    case n: SSA.InvokeDynamic => computeMethodSig(n, n.srcs.map(inferences))
    }
  }

  override def join(lhs: IType, rhs: IType) = {
    val res = merge(lhs, rhs)
    res
  }
}
object OptimisticAnalyze {
  /**
    * Performs an optimistic analysis on the given method body.
    *
    * Walks the method body block-by-block, evaluating all phi nodes each time
    * a block transition is made. The current evaluation environment is kept as
    * a global mapping of phi nodes to [[T]], which is updated as evaluation
    * occurs. Evaluation of non-phi nodes are cached, unless invalidated by a
    * change in the value assigned to an upstream phi node.
    *
    * The phi-inference mapping can be shared by all blocks being evaluated, as
    * the inferences monotonically widen as time goes on, and we never have two
    * blocks which treat a single phi node as having multiple, incompatible
    * inferences. If a block is re-visited with a set of phi-inferences
    * different from what it was assigned earlier, the conflicting inferences
    * are merged via [[Lattice.join]] and the block re-visited using the new
    * inferences.
    *
    * As long as new blocks are being discovered, or phi node inferences are
    * modified, the respective blocks are added to the worklist for processing.
    * When the worklist is empty, inference is complete and the algorithm exits
    */
  def apply[T](program: Program,
               initialValues: Map[SSA.Val, T],
               initialBlock: SSA.Block,
               lattice: Lattice[T],
               naming: Namer.Result): (mutable.LinkedHashMap[SSA.Val, T], Set[SSA.Block]) = {

    val inferredBlocks = mutable.Set(initialBlock)
    val workList = mutable.LinkedHashSet(initialBlock)

    val evaluated = mutable.LinkedHashMap.empty[SSA.Val, T]

    def evaluate(v: SSA.Val): T = {
      evaluated.getOrElseUpdate(
        v,
        v match {
          case phi: SSA.Phi => evaluated(phi)
          case _ => lattice.transferValue(v, evaluate)
        }
      )
    }

    while(workList.nonEmpty){

      val currentBlock = workList.head
      workList.remove(currentBlock)
      val Array(nextControl) = currentBlock.downstreamList.collect{case n: SSA.Control => n}
//      println()
//      pprint.log(currentBlock)
//      pprint.log(nextControl)
      def queueNextBlock(nextBlock: SSA.Block) = {
        val nextPhis = nextBlock
          .downstreamList
          .collect{case p: SSA.Phi => p}
          .filter(phi => phi.block == nextBlock)

        val newPhiMapping = nextPhis
          .map{phi =>
            val Seq(expr) = phi
              .incoming
              .collect{case (k, v) if k == currentBlock => v}
              .toSeq
            val res = evaluate(expr)
            (phi, res)
          }
          .toMap

        var continueNextBlock = !inferredBlocks(nextBlock)

        val invalidatedPhis = mutable.Set.empty[SSA.Phi]

        for((k, v) <- newPhiMapping) {
          evaluated.get(k) match{
            case None =>
              continueNextBlock = true
              evaluated(k) = v
            case Some(old) =>
              if (old != v){

                val merged = lattice.join(old, v)
                if (merged != old){
                  continueNextBlock = true
                  invalidatedPhis.add(k)
                  evaluated(k) = merged
                }
              }
          }
        }

        if (continueNextBlock) {
          inferredBlocks.add(nextBlock)
          workList.add(nextBlock)
          val invalidated = Util.breadthFirstSeen[SSA.Node](invalidatedPhis.toSet)(_.downstreamList)
            .filter(!_.isInstanceOf[SSA.Phi])

          invalidated.foreach{
            case b: SSA.Block =>
              inferredBlocks.remove(b)
            case p: SSA.Jump =>
              inferredBlocks.remove(p.block)
              workList.add(p.block)
            case n: SSA.Val =>
              evaluated.remove(n)
            case _ => // do nothing
          }
        }

      }


      nextControl.upstream.collect{case v: SSA.Val => evaluate(v)}
      nextControl match{
        case nextBlock: SSA.Block => queueNextBlock(nextBlock)

        case n: SSA.Jump =>
          n match{
            case r: SSA.Return =>

            case r: SSA.ReturnVal =>
            case n: SSA.UnaBranch =>
              val valueA = evaluate(n.a)
              val doBranch = (valueA, n.opcode) match{
                case (CType.I(v), SSA.UnaBranch.IFNE) => Some(v != 0)
                case (CType.I(v), SSA.UnaBranch.IFEQ) => Some(v == 0)
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
              val valueA = evaluate(n.a)
              val valueB = evaluate(n.b)
              val doBranch = (valueA, valueB, n.opcode) match{
                case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPEQ) => Some(v1 == v2)
                case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPNE) => Some(v1 != v2)
                case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPLT) => Some(v1 < v2)
                case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPGE) => Some(v1 >= v2)
                case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPGT) => Some(v1 > v2)
                case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPLE) => Some(v1 <= v2)
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
            case n: SSA.TableSwitch => ???
            case n: SSA.LookupSwitch => ???
          }
      }
    }

    pprint.log(evaluated)

    (evaluated, inferredBlocks.toSet)
  }
}
