package joptimize.analyzer

import joptimize.model.{CType, IType, JType, SSA}

trait Brancher[T] {
  def evaluateUnaBranch(t: T, op: SSA.UnaBranch.Code): Option[Boolean]
  def evaluateBinBranch(t1: T, t2: T, op: SSA.BinBranch.Code): Option[Boolean]
  def evaluateSwitch(t: T): Option[Int]
}

object ITypeBrancher extends Brancher[IType]{
  def evaluateUnaBranch(t: IType, op: SSA.UnaBranch.Code) = (t, op) match {
    case (CType.I(v), SSA.UnaBranch.IFNE) => Some(v != 0)
    case (CType.I(v), SSA.UnaBranch.IFEQ) => Some(v == 0)
    case (CType.I(v), SSA.UnaBranch.IFLE) => Some(v <= 0)
    case (CType.I(v), SSA.UnaBranch.IFLT) => Some(v < 0)
    case (CType.I(v), SSA.UnaBranch.IFGE) => Some(v >= 0)
    case (CType.I(v), SSA.UnaBranch.IFGT) => Some(v > 0)
//    case (JType.Null, SSA.UnaBranch.IFNULL) => Some(true)
//    case (JType.Null, SSA.UnaBranch.IFNONNULL) => Some(false)
    case _ => None
  }
  def evaluateBinBranch(t1: IType, t2: IType, op: SSA.BinBranch.Code) = (t1, t2, op) match {
    case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPEQ) => Some(v1 == v2)
    case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPNE) => Some(v1 != v2)
    case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPLT) => Some(v1 < v2)
    case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPGE) => Some(v1 >= v2)
    case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPGT) => Some(v1 > v2)
    case (CType.I(v1), CType.I(v2), SSA.BinBranch.IF_ICMPLE) => Some(v1 <= v2)
    case _ => None
  }
  def evaluateSwitch(t: IType) = t match {
    case CType.I(v) => Some(v)
    case _ => None
  }
}