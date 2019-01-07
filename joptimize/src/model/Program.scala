package joptimize.model

import java.util

import joptimize.Util
import joptimize.graph.TarjansStronglyConnectedComponents

import scala.collection.mutable


/**
  * Represents the combined control/data-flow graph of a SSA program.
  */
case class Program(allTerminals: Seq[SSA.Ctrl]){

  /**
    * Transforms this program by trying to apply a callback on every node.
    *
    * If the callback is successfully applied, it is applied repeatedly until
    * it no longer matches the node
    */
  def transform(onValue: PartialFunction[SSA.Val, SSA.Val] = PartialFunction.empty,
                onControl: PartialFunction[SSA.Ctrl, SSA.Ctrl] = PartialFunction.empty): Program = {


    val (allNodes0, _, _) = Util.breadthFirstAggregation[SSA.Node](allTerminals.toSet)(_.upstream)
    val allNodes = allNodes0.toSeq
    val nodeToIndex = allNodes.zipWithIndex.toMap
    val indexToNode = nodeToIndex.map(_.swap)
    val groupedIndices = TarjansStronglyConnectedComponents(allNodes.map(_.upstream.map(nodeToIndex)))
    val groupedNodes = groupedIndices.map(_.map(indexToNode))
    val seenVals = new util.IdentityHashMap[SSA.Node, SSA.Node]()
    val seenCtrls = new util.IdentityHashMap[SSA.Ctrl, SSA.Ctrl]()
    pprint.log(groupedNodes, height=99999)
    for(group <- groupedNodes){
      pprint.log(group)
      val queue = group.to[mutable.Queue]
      while(queue.nonEmpty){
        val current = queue.dequeue()
        current match{
          case n: SSA.Val =>
            onValue.lift(n) match{
              case None => seenVals.put(n, n)
              case Some(replaced) =>
                pprint.log(current)
                pprint.log(replaced)
                current.replaceWith(replaced)
                queue.enqueue(replaced)
            }
          case n: SSA.Ctrl =>
            onControl.lift(n) match{
              case None => seenCtrls.put(n, n)
              case Some(replaced) =>
                current.replaceWith(replaced)
                queue.enqueue(replaced)
            }
        }
      }
    }

    Program(allTerminals.map(seenCtrls.get))
  }
}