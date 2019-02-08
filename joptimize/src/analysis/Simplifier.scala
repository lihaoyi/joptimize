package joptimize.analysis

import joptimize.Util
import joptimize.model.SSA

object Simplifier {
  val transform: PartialFunction[SSA.Node, Seq[SSA.Node]] = {
    case phi: SSA.Phi if phi.getSize != 0 =>
      //          b             _   b
      //         /             | |/
      // a -- phi -- c    a -- phi -- c
      //         \                \
      //          d                d
      //    b
      //   /
      // a -- c
      //   \
      //    d
      val filteredValues = phi.incoming.filter(_._2 != phi)

      if (filteredValues.map(_._2).size == 1) Util.replace(phi, filteredValues.head._2)
      else Nil

    case reg: SSA.Merge =>
      //            b
      //           /
      // a -- merge -- c
      //           \
      //            d
      //    b
      //   /
      // a -- c
      //   \
      //    d
      if (reg.incoming.size == 1) Util.replace(reg, reg.incoming.head)
      else Nil
  }
}
