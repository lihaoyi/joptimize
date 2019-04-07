package joptimize.optimize

import joptimize.model.{IType, SSA}

class CombinedLattice(typeLattice: ITypeLattice, purityLattice: PurityLattice) extends Lattice[(IType, Boolean)]{
  def transferValue(node: SSA.Val, inferences: SSA.Val => (IType, Boolean)) = {
    (typeLattice.transferValue(node, inferences(_)._1), purityLattice.transferValue(node, inferences(_)))
  }

  def join(lhs: (IType, Boolean), rhs: (IType, Boolean)) = {
    val (lhsType, lhsPurity) = lhs
    val (rhsType, rhsPurity) = rhs
    (typeLattice.join(lhsType, rhsType), purityLattice.join(lhsPurity, rhsPurity))
  }
}
