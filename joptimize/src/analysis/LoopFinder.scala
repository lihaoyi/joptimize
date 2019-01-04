package joptimize.analysis

import scala.collection.mutable


object LoopFinder {
  case class SimpleLoop[T](
    header: T,
    allHeaders: Set[T],
    isReducible: Boolean,
    basicBlocks: Set[T],
    children: Set[SimpleLoop[T]]
  )
  private[this] class MutableSimpleLoop[T](val header: T,
                                           val isReducible: Boolean,
                                           val isRoot: Boolean) {
    var basicBlocks  = Set[T](header)
    var allHeaders = Set[T](header)

    var children = Set[MutableSimpleLoop[T]]()

    var parent: MutableSimpleLoop[T] = null

    def setParent(parent: MutableSimpleLoop[T]) = {
      this.parent = parent
      this.parent.children += this
    }

    def freeze(): SimpleLoop[T] = SimpleLoop(
      header,
      allHeaders,
      isReducible,
      basicBlocks,
      children.map(_.freeze())
    )
  }

  object BasicBlockClass extends Enumeration {
    val BB_NONHEADER,    // a regular BB
        BB_REDUCIBLE,    // reducible loop
        BB_SELF,         // single BB loop
        BB_IRREDUCIBLE = Value  // Sentinel
  }

  /**
    * class UnionFindNode
    *
    * The algorithm uses the Union/Find algorithm to collapse
    * complete loops into a single node. These nodes and the
    * corresponding functionality are implemented with this class
    */
  class UnionFindNode[T] {

    var parent   : UnionFindNode[T] = null
    var bb       : T    = null.asInstanceOf[T]

    // Initialize this node.
    //
    def initNode(bb: T) = {
      this.parent     = this
      this.bb         = bb
    }

    // Union/Find Algorithm - The find routine.
    //
    // Implemented with Path Compression (inner loops are only
    // visited and collapsed once, however, deep nests would still
    // result in significant traversals).
    //
    def findSet: UnionFindNode[T] = {
      var nodeList = List[UnionFindNode[T]]()

      var node = this
      while (node != node.parent) {
        if (node.parent != node.parent.parent) {
          nodeList = node :: nodeList
        }
        node = node.parent
      }

      // Path Compression, all nodes' parents point to the 1st level parent.
      for (iter <- nodeList)
        iter.parent_=(node.parent)
      node
    }

    // Union/Find Algorithm - The union routine.
    //
    // Trivial. Assigning parent pointer is enough,
    // we rely on path compression.
    //
    def union(basicBlock: UnionFindNode[T]) = parent = basicBlock
  }

  //
  // Constants
  //
  // Marker for uninitialized nodes.
  val UNVISITED: Int = -1


  //
  // IsAncestor
  //
  // As described in the paper, determine whether a node 'w' is a
  // "true" ancestor for node 'v'.
  //
  // Dominance can be tested quickly using a pre-order trick
  // for depth-first spanning trees. This is why DFS is the first
  // thing we run below.
  //
  def isAncestor(w: Int, v: Int, last: Array[Int])= (w <= v) && (v <= last(w))

  //
  // DFS - Depth-First-Search
  //
  // DESCRIPTION:
  // Simple depth first traversal along out edges with node numbering.
  //
  def DFS[T](currentNode: T,
             number: scala.collection.mutable.Map[T, Int],
             last: Array[Int],
             current: Int,
             outEdges: Map[T, Seq[T]]): Int = {

    number(currentNode) = current

    var lastid = current
    for (target <- outEdges.getOrElse(currentNode, Nil)){
      if (!number.contains(target)){
        lastid = DFS(target, number, last, lastid + 1, outEdges)
      }
    }
    last(number(currentNode)) = lastid
    lastid
  }

  // Find loops and build loop forest using Havlak's algorithm, which
  // is derived from Tarjan. Variable names and step numbering has
  // been chosen to be identical to the nomenclature in Havlak's
  // paper (which, in turn, is similar to the one used by Tarjan).
  //
  def analyzeLoops[T](edgeList: Seq[(T, T)]): SimpleLoop[T] = {

    val allNodes = edgeList.flatMap{case (k, v) => Seq(k, v)}.distinct
    val size = allNodes.size

    val nonBackPreds    = new Array[Set[Int]](size)
    val backPreds       = new Array[List[Int]](size)

    val number          = scala.collection.mutable.Map[T, Int]()

    val header          = new Array[Int](size)
    val types           = new Array[BasicBlockClass.Value](size)
    val last            = new Array[Int](size)
    val nodes           = new Array[UnionFindNode[T]](size)

    for (i <- 0 until size) {
      nonBackPreds(i) = Set[Int]()
      backPreds(i)    = List[Int]()
      nodes(i)        = new UnionFindNode()
    }

    val outEdges = edgeList.groupBy(_._1).map{case (k, v) => (k, v.map(_._2))}
    val inEdges = edgeList.groupBy(_._2).map{case (k, v) => (k, v.map(_._1))}

    val startNode = allNodes.find(!inEdges.contains(_)).get

    DFS(startNode, number, last, 0, outEdges)

    for((currentNode, current) <- number) nodes(current).initNode(currentNode)

    for (w <- Range(0, size)) {
      header(w) = 0
      types(w)  = BasicBlockClass.BB_NONHEADER

      val nodeW = nodes(w).bb

      val in = inEdges.getOrElse(nodeW, Nil)
      for (nodeV <- in) {
        for(v <- number.get(nodeV)){
          if (isAncestor(w, v, last)) backPreds(w) ::= v
          else nonBackPreds(w) += v
        }
      }
    }

    header(0) = 0

    val loopMap = mutable.Map.empty[T, MutableSimpleLoop[T]]
    for (w <- Range.inclusive(size - 1, 0, -1)) {
      // this is 'P' in Havlak's paper
      var nodePool = List.empty[UnionFindNode[T]]

      val nodeW = nodes(w).bb

      for (v <- backPreds(w)) {
        if (v != w) nodePool ::= nodes(v).findSet
        else types(w) = BasicBlockClass.BB_SELF
      }

      var workList = List.empty[UnionFindNode[T]]
      workList = nodePool

      if (nodePool.nonEmpty) {
        types(w) = BasicBlockClass.BB_REDUCIBLE
      }

      while (workList.nonEmpty) {
        val x = workList.head
        workList = workList.tail


        for (iter <- nonBackPreds(number(x.bb))) {
          val ydash = nodes(iter).findSet

          if (!isAncestor(w, number(ydash.bb), last)) {
            types(w) = BasicBlockClass.BB_IRREDUCIBLE
            types(number(x.bb)) = BasicBlockClass.BB_IRREDUCIBLE
            nonBackPreds(w) += number(ydash.bb)
          } else {
            if (number(ydash.bb) != w) {
              if (!nodePool.contains(ydash)) {
                workList = ydash :: workList
                nodePool = ydash :: nodePool
              }
            }
          }
        }
      }


      if (nodePool.nonEmpty || types(w) == BasicBlockClass.BB_SELF) {
        val loop = new MutableSimpleLoop(
          nodeW,
          types(w) != BasicBlockClass.BB_IRREDUCIBLE,
          false
        )

        loopMap(nodes(w).bb) = loop

        for (node <- nodePool) {
          header(number(node.bb)) = w
          node.union(nodes(w))

          if (loopMap.contains(node.bb)) loopMap(node.bb).setParent(loop)
          else {
            if (types(number(node.bb)) == BasicBlockClass.BB_REDUCIBLE || types(number(node.bb)) == BasicBlockClass.BB_IRREDUCIBLE){
              loop.allHeaders += node.bb
            }
            loop.basicBlocks += node.bb
          }
        }
      }
    }

    val root = new MutableSimpleLoop(startNode, true, true)

    for (liter <- loopMap.values) {
      if (!liter.isRoot && liter.parent == null) liter.setParent(root)
    }

    root.freeze()
  }  // findLoops
}
