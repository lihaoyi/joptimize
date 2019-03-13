package joptimize.model

import guru.nidi.graphviz.attribute.RankDir
import joptimize.Util
import joptimize.analysis.Namer

import scala.collection.mutable


/**
  * Represents the combined control/data-flow graph of a SSA program.
  */
case class Program(args: Seq[SSA.Arg], allTerminals: Seq[SSA.Control]){
  def getAllVertices() = Util.breadthFirstSeen[SSA.Node](allTerminals.toSet)(_.upstream)

  def checkLinks(checkDead: Boolean = true) = {
    val allVertices = getAllVertices()
    allVertices.foreach(_.checkLinks())

    if (checkDead) {
      val missing = for {
        v <- allVertices
        down <- v.downstreamList
        if !allVertices.contains(down)
      } yield (v, down)

      assert(missing.isEmpty, "Downstream nodes not part of allVertices: " + missing.mkString(", "))
    }
  }

  def transform(visit: PartialFunction[SSA.Node, Seq[SSA.Node]]) = {
    val queue = getAllVertices().to[mutable.LinkedHashSet]
    while (queue.nonEmpty) {
      val current = queue.head
      queue.remove(current)
      for(next <- visit.lift(current)) {
        for(n <- next) queue.add(n)
      }
    }
  }

  def dump(fileName: String, naming: Namer.Result = null) = {
    def name(x: SSA.Node) = if (naming == null) "" else naming(x).getOrElse("")
    import guru.nidi.graphviz.model.Factory._
    import guru.nidi.graphviz.attribute._
    import guru.nidi.graphviz.engine._
    import guru.nidi.graphviz.model._

    val live = Util.breadthFirstSeen[SSA.Node](allTerminals.toSet)(_.upstream)

    val (seen, terminals, allEdges) = Util.breadthFirstAggregation0[SSA.Node](allTerminals.toSet)(
      x => x.upstream ++ x.downstreamList
    )

    val allGraphvizNodes = seen.keys.map(x => x -> node(x.toString + " " + name(x))).toMap
    val (liveNodes, deadNodes) = allGraphvizNodes.toSeq.partition(t => live(t._1))
    val directedEdges: Map[SSA.Node, Seq[(SSA.Node, String)]] = allEdges
      .flatMap{case (a, b) =>
        (a.downstreamList.contains(b), b.upstream.contains(a)) match{
          case (true, true) => Seq((a, b, "bidi"))
          case (false, true) => Seq((a, b, "up"))
          case (true, false) => Seq((a, b, "down"))
          case (false, false) => Nil
        }
      }
      .groupBy(_._1)
      .map{case (k, vs) => (k, vs.map(v => (v._2, v._3)))}


    def colorNodes(nodes: Seq[(SSA.Node, Node)]) = {
      nodes.map { case (x, n) =>
        n.`with`(
          x match {
            case n: SSA.Val => if (n.getSize == 0) Color.RED else Color.CYAN
            case c: SSA.Control => Color.MAGENTA
          },
          Style.FILLED
        )
      }
    }

    val g = graph("Program")
      .directed()
      .graphAttr()
      .`with`(RankDir.TOP_TO_BOTTOM)
      .`with`(
        directedEdges.map{ case (x, ys) =>
          allGraphvizNodes(x).link(
            ys.map { case (y, kind) =>
              to(allGraphvizNodes(y)).`with`(
                kind match{
                  case "bidi" => Style.SOLID
                  case "up" => Style.DOTTED
                  case "down" => Style.DOTTED
                }
              )
            }
            :_*
         )
        }.toSeq:_*
      )
      .`with`(colorNodes(deadNodes):_*)
      .`with`(graph.cluster().named("Live").`with`(colorNodes(liveNodes):_*))
    Graphviz.fromGraph(g).render(Format.SVG).toFile(new java.io.File("out/" + fileName))
  }
}