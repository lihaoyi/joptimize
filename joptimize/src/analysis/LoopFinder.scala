package joptimize.analysis

import scala.collection.mutable


object LoopFinder {
  case class Loop[T](
    headers: Set[T],
    isReducible: Boolean,
    basicBlocks: Set[T],
    children: Set[Loop[T]]
  )
  private[this] class LoopBuilder[T](val primaryHeader: T,
                                     val isReducible: Boolean,
                                     val isRoot: Boolean) {
    var basicBlocks  = Set[T](primaryHeader)
    var allHeaders = Set[T](primaryHeader)

    var children = Set[LoopBuilder[T]]()

    var parent: LoopBuilder[T] = null

    def setParent(parent: LoopBuilder[T]) = {
      this.parent = parent
      this.parent.children += this
    }

    def build(): Loop[T] = Loop(
      allHeaders,
      isReducible,
      basicBlocks,
      children.map(_.build())
    )
  }

  sealed class BlockType private ()(implicit name: sourcecode.Name){
    override def toString = name.value
  }
  object BlockType {
    val NonHeader = new BlockType()
    val Reducible = new BlockType()
    val Self = new BlockType()
    val Irreducible = new BlockType()
  }

  /**
    * class UnionFindNode
    *
    * The algorithm uses the Union/Find algorithm to collapse
    * complete loops into a single node. These nodes and the
    * corresponding functionality are implemented with this class
    */
  class UnionFindNode[T](var value: T) {
    var parent   : UnionFindNode[T] = this

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
      for (iter <- nodeList) iter.parent = node.parent
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
  def analyzeLoops[T](edgeList: Seq[(T, T)]): Loop[T] = {

    val outEdges = edgeList.groupBy(_._1).map{case (k, v) => (k, v.map(_._2))}
    val inEdges = edgeList.groupBy(_._2).map{case (k, v) => (k, v.map(_._1))}

    val (size, startNode) = {
      // Avoid using allNodes throughout the body of this function, because
      // we should be using `number` and `numberToNode` instead.
      val allNodes = edgeList.flatMap{case (k, v) => Seq(k, v)}.distinct
      val startNode = allNodes.find(!inEdges.contains(_)).get
      (allNodes.size, startNode)
    }

    val nonBackPreds    = new Array[Set[Int]](size)
    val backPreds       = new Array[Set[Int]](size)

    val number          = scala.collection.mutable.Map[T, Int]()

    val header          = new Array[Int](size)
    val types           = new Array[BlockType](size)
    val last            = new Array[Int](size)
    val nodes           = new Array[UnionFindNode[T]](size)

    DFS(startNode, number, last, 0, outEdges)
    val numberToNode = number.map(_.swap)

    for (w <- Range(0, size)) {

      nonBackPreds(w) = Set[Int]()
      backPreds(w) = Set[Int]()
      nodes(w) = new UnionFindNode(numberToNode(w))
      header(w) = 0
      types(w)  = BlockType.NonHeader

      val in = inEdges.getOrElse(numberToNode(w), Nil)
      for (nodeV <- in) {
        val v = number(nodeV)
        if (isAncestor(w, v, last)) backPreds(w) += v
        else nonBackPreds(w) += v
      }
    }

    header(0) = 0

    val loopMap = mutable.Map.empty[T, LoopBuilder[T]]
    for (w <- Range.inclusive(size - 1, 0, -1)) {
      // this is 'P' in Havlak's paper
      var nodePool = List.empty[UnionFindNode[T]]

      for (v <- backPreds(w)) {
        if (v != w) nodePool ::= nodes(v).findSet
        else types(w) = BlockType.Self
      }

      var workList = nodePool

      if (nodePool.nonEmpty) {
        types(w) = BlockType.Reducible
      }

      while (workList.nonEmpty) {
        val x = workList.head
        workList = workList.tail

        for (y <- nonBackPreds(number(x.value))) {
          val ydash = nodes(y).findSet

          if (!isAncestor(w, number(ydash.value), last)) {
            types(w) = BlockType.Irreducible

            nonBackPreds(w) += number(ydash.value)
          } else if (!nodePool.contains(ydash) && number(ydash.value) != w) {
            workList = ydash :: workList
            nodePool = ydash :: nodePool
          }
        }
      }


      if (nodePool.nonEmpty || types(w) == BlockType.Self) {
        val loop = new LoopBuilder(
          numberToNode(w),
          types(w) != BlockType.Irreducible,
          false
        )

        loopMap(nodes(w).value) = loop

        for (x <- nodePool) {
          header(number(x.value)) = w
          x.union(nodes(w))
          if (loopMap.contains(x.value)) loopMap(x.value).setParent(loop)
          else {
            types(number(x.value)) match{
              case BlockType.Reducible | BlockType.Irreducible =>
                loop.allHeaders += x.value
              case _ => // do nothing
            }

            loop.basicBlocks += x.value
          }
        }
      }
    }

    val root = new LoopBuilder(startNode, true, true)

    for(i <- Range(0, numberToNode.size)){
      if (types(i) == BlockType.NonHeader && header(i) == 0){
        root.basicBlocks += numberToNode(i)
      }
    }

    for (liter <- loopMap.values) {
      if (!liter.isRoot && liter.parent == null) liter.setParent(root)
    }

    root.build()
  }
}
