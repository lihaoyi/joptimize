package optimize

import joptimize.model._


class LivenessLattice(fetchMethodInfo: (MethodSig, Boolean, Seq[IType]) => Set[Int]) {
  def transferValue(node: SSA.Val,
                    inferredType: IType,
                    inferredPurity: Boolean,
                    inferences: SSA.Val => (IType, Boolean, Set[Int])): Set[Int] = {
    node match{
      case n: SSA.Arg => Set(n.index)
      case n: SSA.Invoke =>
        if (inferredType.isConstant && inferredPurity) Set()
        else {
          val inferredArgs = n.srcs.map(inferences(_)._1)
          val liveArgs = fetchMethodInfo(n.sig, n.isInstanceOf[SSA.InvokeSpecial], inferredArgs)

          n.srcs
            .iterator
            .zipWithIndex
            .collect{case (v, i) if i == 0 || liveArgs(i) => inferences(v)._3}
            .flatten
            .toSet
        }

      case _ =>
        node.upstream.collect{case v: SSA.Val => inferences(v)}.flatMap(_._3).toSet
    }

  }

  def join(lhs: Set[Int], rhs: Set[Int]) = {
    lhs | rhs
  }
}
