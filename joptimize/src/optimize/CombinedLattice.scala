package joptimize.optimize

import joptimize.model.{IType, SSA}
import optimize.LivenessLattice

class CombinedLattice(typeLattice: ITypeLattice,
                      purityLattice: PurityLattice,
                      livenessLattice: LivenessLattice) extends Lattice[(IType, Boolean, Set[Int])]{
  def transferValue(node: SSA.Val, inferences: SSA.Val => (IType, Boolean, Set[Int])) = {
    val inferredType = typeLattice.transferValue(node, inferences(_)._1)
    val inferredPurity = purityLattice.transferValue(node, inferences(_) match{case (a, b, c) => (a, b)})
    val inferredLiveness = livenessLattice.transferValue(node, inferredType, inferredPurity, inferences(_))
    (inferredType, inferredPurity, inferredLiveness)
  }

  def join(lhs: (IType, Boolean, Set[Int]), rhs: (IType, Boolean, Set[Int])) = {
    val (lhsType, lhsPurity, lhsLiveArgs) = lhs
    val (rhsType, rhsPurity, rhsLiveArgs) = rhs
    Tuple3(
      typeLattice.join(lhsType, rhsType),
      purityLattice.join(lhsPurity, rhsPurity),
      livenessLattice.join(lhsLiveArgs, rhsLiveArgs)
    )
  }
}
