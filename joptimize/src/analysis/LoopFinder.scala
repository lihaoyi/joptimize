package joptimize.analysis

//
// class SimpleLoop
//
// Basic representation of loops, a loop has an entry point,
// one or more exit edges, a set of basic blocks, and potentially
// an outer loop - a "parent" loop.
//
// Furthermore, it can have any set of properties, e.g.,
// it can be an irreducible loop, have control flow, be
// a candidate for transformations, and what not.
//
class SimpleLoop(val header: Int = -1){
  var basicBlocks  = Set[Int]()
  if (header != -1) basicBlocks += header
  var children     = Set[SimpleLoop]()
  var parent       : SimpleLoop = null

  var isRoot       : Boolean = false
  var isReducible  : Boolean = true
  var counter      : Int = 0
  var nestingLevel : Int = 0
  var depthLevel   : Int = 0

  def addNode(bb : Int) = basicBlocks += bb
  def addChildLoop(loop : SimpleLoop) = children += loop

  def dump(indent : Int) {
    for (i <- 0 until indent)
      System.out.format("  ")

    System.out.format("loop-%d nest: %d depth %d %s\n",
      counter.asInstanceOf[AnyRef],
      nestingLevel.asInstanceOf[AnyRef],
      depthLevel.asInstanceOf[AnyRef],
      if (isReducible) ""
      else "(Irreducible) " )

  }

  def setParent(parent : SimpleLoop) = {
    this.parent = parent
    this.parent.addChildLoop(this)
  }

  def setNestingLevel(level : Int) = {
    nestingLevel = level
    if (level == 0) isRoot_=(true)
  }
}


//
// LoopStructureGraph
//
// Maintain loop structure for a given CFG.
//
// Two values are maintained for this loop graph, depth, and nesting level.
// For example:
//
// loop        nesting level    depth
//----------------------------------------
// loop-0      2                0
//   loop-1    1                1
//   loop-3    1                1
//     loop-2  0                2
//
class LSG {
  var loops = List[SimpleLoop]()
  var root  = new SimpleLoop()
  root.setNestingLevel(0)
  addLoop(root)


  def addLoop(loop : SimpleLoop) = loops = loop :: loops

  def dump = dumpRec(root,0)

  // Interesting - needs return type
  def dumpRec(loop : SimpleLoop, indent : Int) : Unit = {
    loop.dump(indent)

    for (liter <- loop.children)
      dumpRec(liter, indent + 1)
  }

  def calculateNestingLevel = {
    for (liter <- loops) {
      if (!liter.isRoot)
        if (liter.parent == null)
          liter.setParent(root)
    }

    calculateNestingLevelRec(root, 0)
  }

  def max(a : Int, b : Int) = if (a > b) a else b

  def calculateNestingLevelRec(loop : SimpleLoop, depth : Int) {
    loop.depthLevel_=(depth)
    for (liter <- loop.children) {
      calculateNestingLevelRec(liter, depth+1)

      loop.setNestingLevel(max(loop.nestingLevel,
        1+liter.nestingLevel))
    }
  }

  def getNumLoops : Int = loops.size
}

//======================================================
// Main Algorithm
//======================================================

object HavlakLoopFinder {
  object BasicBlockClass extends Enumeration {
    val BB_TOP,      // uninitialized
    BB_NONHEADER,    // a regular BB
    BB_REDUCIBLE,    // reducible loop
    BB_SELF,         // single BB loop
    BB_IRREDUCIBLE,  // irreducible loop
    BB_LAST = Value  // Sentinel
  }

  /**
    * class UnionFindNode
    *
    * The algorithm uses the Union/Find algorithm to collapse
    * complete loops into a single node. These nodes and the
    * corresponding functionality are implemented with this class
    */
  class UnionFindNode {

    var parent    : UnionFindNode = null
    var bb        : Int    = -1
    var loop      : SimpleLoop    = null
    var dfsNumber : Int           = 0

    // Initialize this node.
    //
    def initNode(bb : Int, dfsNumber : Int) = {
      this.parent     = this
      this.bb         = bb
      this.dfsNumber  = dfsNumber
      this.loop       = null
    }

    // Union/Find Algorithm - The find routine.
    //
    // Implemented with Path Compression (inner loops are only
    // visited and collapsed once, however, deep nests would still
    // result in significant traversals).
    //
    def findSet : UnionFindNode = {
      var nodeList = List[UnionFindNode]()

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
    def union(basicBlock : UnionFindNode) = {
      parent_=(basicBlock)
    }
  }

  //
  // Constants
  //
  // Marker for uninitialized nodes.
  val UNVISITED : Int = -1


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
  def isAncestor(w : Int, v : Int, last : Array[Int]) : Boolean = {
    (w <= v) && (v <= last(w))
  }

  //
  // DFS - Depth-First-Search
  //
  // DESCRIPTION:
  // Simple depth first traversal along out edges with node numbering.
  //
  def DFS(currentNode: Int,
          nodes: Array[UnionFindNode],
          number: scala.collection.mutable.Map[Int, Int],
          last: Array[Int],
          current: Int,
          outEdges: Map[Int, Seq[Int]]) : Int = {
    nodes(current).initNode(currentNode, current)
    number +=(currentNode -> current)

    var lastid = current
    for {
      target <- outEdges.getOrElse(currentNode, Nil)
      if number(target) == UNVISITED
    } {
      lastid = DFS(target, nodes, number, last, lastid + 1, outEdges)
    }
    last(number(currentNode)) = lastid
    lastid
  }

  //
  // findLoops
  //
  // Find loops and build loop forest using Havlak's algorithm, which
  // is derived from Tarjan. Variable names and step numbering has
  // been chosen to be identical to the nomenclature in Havlak's
  // paper (which, in turn, is similar to the one used by Tarjan).
  //
  def findLoops(edgeList: Seq[(Int, Int)],
                lsg : LSG): Int = {
    val allNodes = edgeList.flatMap{case (k, v) => Seq(k, v)}.distinct
    val size = allNodes.size

    val nonBackPreds    = new Array[Set[Int]](size)
    val backPreds       = new Array[List[Int]](size)

    val number          = scala.collection.mutable.Map[Int, Int]()

    val header          = new Array[Int](size)
    val types           = new Array[BasicBlockClass.Value](size)
    val last            = new Array[Int](size)
    val nodes           = new Array[UnionFindNode](size)

    for (i <- 0 until size) {
      nonBackPreds(i) = Set[Int]()
      backPreds(i)    = List[Int]()
      nodes(i)        = new UnionFindNode()
    }

    val outEdges = edgeList.groupBy(_._1).map{case (k, v) => (k, v.map(_._2))}
    val inEdges = edgeList.groupBy(_._2).map{case (k, v) => (k, v.map(_._1))}

    val startNode = allNodes.find(!inEdges.contains(_)).get
    // Step a:
    //   - initialize all nodes as unvisited.
    //   - depth-first traversal and numbering.
    //   - unreached BB's are marked as dead.
    //
    for (value <- allNodes) {
      number(value) = UNVISITED
    }

    DFS(startNode, nodes, number, last, 0, outEdges)

    // Step b:
    //   - iterate over all nodes.
    //
    //   A backedge comes from a descendant in the DFS tree, and non-backedges
    //   from non-descendants (following Tarjan).
    //
    //   - check incoming edges 'v' and add them to either
    //     - the list of backedges (backPreds) or
    //     - the list of non-backedges (nonBackPreds)
    //
    for (w <- Range(0, size)) {
      header(w) = 0
      types(w)  = BasicBlockClass.BB_NONHEADER

      val nodeW = nodes(w).bb

      val in = inEdges.getOrElse(nodeW, Nil)
      for (nodeV <- in) {
        val v = number(nodeV)
        if (v != UNVISITED) {
          if (isAncestor(w, v, last)) {
            backPreds(w) = v :: backPreds(w)
          } else {
            nonBackPreds(w) += v
          }
        }
      }
    }

    // Start node is root of all other loops.
    header(0) = 0

    // Step c:
    //
    // The outer loop, unchanged from Tarjan. It does nothing except
    // for those nodes which are the destinations of backedges.
    // For a header node w, we chase backward from the sources of the
    // backedges adding nodes to the set P, representing the body of
    // the loop headed by w.
    //
    // By running through the nodes in reverse of the DFST preorder,
    // we ensure that inner loop headers will be processed before the
    // headers for surrounding loops.
    //
    for (w <- Range.inclusive(size - 1, 0, -1)) {
      // this is 'P' in Havlak's paper
      var nodePool = List[UnionFindNode]()

      val nodeW = nodes(w).bb

      // Step d:
      for (v <- backPreds(w)) {
        if (v != w) {
          nodePool = nodes(v).findSet :: nodePool
        } else {
          types(w) = BasicBlockClass.BB_SELF
        }
      }

      // Copy nodePool to workList.
      //
      var workList = List[UnionFindNode]()
      workList = nodePool filter (p => true)

      if (nodePool.nonEmpty) {
        types(w) = BasicBlockClass.BB_REDUCIBLE
      }

      // work the list...
      //
      while (workList.nonEmpty) {
        val x = workList.head
        workList = workList.tail

        // Step e:
        //
        // Step e represents the main difference from Tarjan's method.
        // Chasing upwards from the sources of a node w's backedges. If
        // there is a node y' that is not a descendant of w, w is marked
        // the header of an irreducible loop, there is another entry
        // into this loop that avoids w.
        //

        for (iter <- nonBackPreds(x.dfsNumber)) {
          val y = nodes(iter)
          val ydash = y.findSet

          if (!isAncestor(w, ydash.dfsNumber, last)) {
            types(w) = BasicBlockClass.BB_IRREDUCIBLE
            nonBackPreds(w) += ydash.dfsNumber
          } else {
            if (ydash.dfsNumber != w) {
              if (!nodePool.contains(ydash)) {
                workList = ydash :: workList
                nodePool = ydash :: nodePool
              }
            }
          }
        }
      }

      // Collapse/Unionize nodes in a SCC to a single node
      // For every SCC found, create a loop descriptor and link it in.
      //
      if (nodePool.nonEmpty || types(w) == BasicBlockClass.BB_SELF) {
        val loop = new SimpleLoop(nodeW)

        loop.isReducible = types(w) != BasicBlockClass.BB_IRREDUCIBLE

        // At this point, one can set attributes to the loop, such as:
        //
        // the bottom node:
        //    iter  = backPreds(w).begin();
        //    loop bottom is: nodes(iter).node;
        //
        // the number of backedges:
        //    backPreds(w).size()
        //
        // whether this loop is reducible:
        //    types(w) != BB_IRREDUCIBLE
        //
        nodes(w).loop = loop

        for (node <- nodePool) {
          // Add nodes to loop descriptor.
          header(node.dfsNumber) = w
          node.union(nodes(w))

          // Nested loops are not added, but linked together.
          if (node.loop != null) {
            node.loop.setParent(loop)
          } else {
            loop.addNode(node.bb)
          }
        }

        lsg.addLoop(loop)
      }  // nodePool.size
    }  // Step c

    lsg.getNumLoops
  }  // findLoops
}
