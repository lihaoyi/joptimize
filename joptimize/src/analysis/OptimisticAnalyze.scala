package joptimize.analysis
//
//import joptimize.model._
//
//import scala.collection.mutable
//trait Lattice[T]{
//  def transfer(node: SSA.Node, inferences: SSA.Node => T): T
//}
//
//
//object Lattice extends Lattice[IType]{
//  def transfer(node: SSA.Node, inferences: SSA.Node => IType) = node match{
//    case n: SSA.ConstI => CType.I(n.value)
//    case n: SSA.ConstJ => CType.J(n.value)
//    case n: SSA.ConstF => CType.F(n.value)
//    case n: SSA.ConstD => CType.D(n.value)
//    case n: SSA.ConstStr => JType.Cls("java/lang/String")
//    case n: SSA.ConstNull => JType.Null
//    case n: SSA.ConstCls => JType.Cls("java/lang/Class")
//
//    case n: SSA.Phi =>
//
//  }
//}
//object OptimisticAnalyze {
//  def apply[T](program: Program,
//               initial: Map[SSA.Node, T],
//               lattice: Lattice[T]): Map[SSA.Node, T] = {
//    val inferences = mutable.Map(initial.toSeq:_*)
//    val workList = mutable.LinkedHashSet(initial.keys.toSeq:_*)
//    while(workList.nonEmpty){
//      val current = workList.head
//      workList.remove(current)
//
//      val newInference = lattice.transfer(current, inferences)
//      if (!inferences.get(current).contains(newInference)){
//        current.downstreamList.foreach(workList.add)
//      }
//    }
//  }
//}
