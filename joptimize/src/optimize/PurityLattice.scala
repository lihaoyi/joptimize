package joptimize.optimize

import joptimize.model._


class PurityLattice(checkMethodPurity: (MethodSig, Boolean, Seq[IType]) => Boolean) {
  def transferValue(node: SSA.Val, inferences: SSA.Val => (IType, Boolean)) = {
    val upstream = node.upstream.collect{case v: SSA.Val => inferences(v)}.map(_._2).forall(identity)
    val current = node match{
      case n: SSA.New => false
      case n: SSA.CheckCast => false
      case n: SSA.InstanceOf => true

      case n: SSA.ChangedState => true
      case n: SSA.Arg => true

      case n: SSA.ConstI => true
      case n: SSA.ConstJ => true
      case n: SSA.ConstF => true
      case n: SSA.ConstD => true
      case n: SSA.ConstStr => true
      case n: SSA.ConstNull => true
      case n: SSA.ConstCls => true

      case n: SSA.ArrayLength => false

      case n: SSA.GetField => false
      case n: SSA.PutField => false

      case n: SSA.GetStatic => false
      case n: SSA.PutStatic => false

      case n: SSA.GetArray => false
      case n: SSA.PutArray => false

      case n: SSA.NewArray => false
      case n: SSA.MultiANewArray => false

      case n: SSA.BinOp =>
        n.opcode match {
          case SSA.BinOp.IDIV | SSA.BinOp.IREM | SSA.BinOp.LDIV | SSA.BinOp.LREM => false
          case _ => true
        }
      case n: SSA.UnaOp => true

      case n: SSA.InvokeStatic => checkMethodPurity(n.sig, false, n.srcs.map(inferences(_)._1))
      case n: SSA.InvokeSpecial => checkMethodPurity(n.sig, true, n.srcs.map(inferences(_)._1))
      case n: SSA.InvokeVirtual => checkMethodPurity(n.sig, false, n.srcs.map(inferences(_)._1))
      case n: SSA.InvokeInterface => checkMethodPurity(n.sig, false, n.srcs.map(inferences(_)._1))
      //    case n: SSA.InvokeDynamic => computeMethodSig(n, n.srcs.map(inferences))
    }
    upstream && current
  }

  def join(lhs: Boolean, rhs: Boolean) = {
    lhs && rhs
  }
}
