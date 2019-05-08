package test
import joptimize.algorithms.Dominator
import joptimize.graph.HavlakLoopTree.Loop
import joptimize.graph.HavlakLoopTree
import utest._

object LoopFinderTests extends TestSuite{
  val tests = Tests{
    def check[T](args: Seq[(T, T)],
                 expectedLoopTree: Loop[T],
                 expectedImmediateDominators: Map[T, T] = null,
                 expectedDominatorDepths: Map[T, Int] = null) = {
      val allNodes = args.flatMap{case (k, v) => Seq(k, v)}
      val analyzed = HavlakLoopTree.analyzeLoops(args, allNodes)
      assert(analyzed == expectedLoopTree)
      if (expectedImmediateDominators!= null){
        val Dominator.Result(immediateDominators, dominatorDepths) =
          Dominator.findDominators(args, allNodes)

        assert(immediateDominators == expectedImmediateDominators)
        assert(dominatorDepths == expectedDominatorDepths)
      }
    }

    'reducible - {
      // 0 -> 1
      'twoNode - check[Int](
        args = Seq(0 -> 1),
        expectedLoopTree = Loop(0, true, Set(0, 1), Set()),
        expectedImmediateDominators = Map(1 -> 0),
        expectedDominatorDepths = Map(0 -> 0, 1 -> 1)
      )
      // 0 -> 1 -> 2
      'threeNode - check[Int](
        args = Seq(0 -> 1, 1 -> 2),
        expectedLoopTree = Loop(0, true, Set(0, 1, 2), Set()),
        expectedImmediateDominators = Map(1 -> 0, 2 -> 1),
        expectedDominatorDepths = Map(0 -> 0, 1 -> 1, 2 -> 2)
      )
      //         2
      //        ^ \
      //       /   v
      // 0 -> 1     4
      //       \   ^
      //        v /
      //         3
      'diamond - check[Int](
        args = Seq(0 -> 1, 1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4),
        expectedLoopTree = Loop(0, true, Set(0, 1, 2, 3, 4), Set()),
        expectedImmediateDominators = Map(1 -> 0, 2 -> 1, 3 -> 1, 4 -> 1),
        expectedDominatorDepths = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 2, 4 -> 2)
      )

      // 0 -> 1 <-> 2
      'loop - check[Int](
        args = Seq(0 -> 1, 1 -> 2, 2 -> 1),
        expectedLoopTree = Loop(0, true, Set(0), Set(
          Loop(1, true, Set(1, 2), Set())
        )),
        expectedImmediateDominators = Map(1 -> 0, 2 -> 1),
        expectedDominatorDepths = Map(0 -> 0, 1 -> 1, 2 -> 2)
      )
      // 0 -> 1 <-> 2 -> 3
      'loopEnd - check[Int](
        args = Seq(0 -> 1, 1 -> 2, 2 -> 1, 2 -> 3),
        expectedLoopTree = Loop(0, true, Set(0, 3), Set(
          Loop(1, true, Set(1, 2), Set())
        )),
        expectedImmediateDominators = Map(1 -> 0, 2 -> 1, 3 -> 2),
        expectedDominatorDepths = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)
      )

      // 0 -> 1 <-> 2 <-> 3
      'nestedLoop - check[Int](
        args = Seq(0 -> 1, 1 -> 2, 2 -> 1, 2 -> 3, 3 -> 2),
        expectedLoopTree = Loop(0, true, Set(0), Set(
          Loop(1, true, Set(1), Set(
            Loop(2, true, Set(2, 3), Set())
          ))
        )),
        expectedImmediateDominators = Map(1 -> 0, 2 -> 1, 3 -> 2),
        expectedDominatorDepths = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)
      )

      //           4
      //           ^
      //           |
      //           v
      // 0 -> 1 -> 2 -> 3 <-> 5
      //      ^        /
      //       \      /
      //        ------
      'twoNestedLoops - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          2 -> 3,
          3 -> 1,
          2 -> 4,
          4 -> 2,
          3 -> 5,
          5 -> 3
        ),
        expectedLoopTree = Loop(0, true, Set(0), Set(
          Loop(1, true, Set(1), Set(
            Loop(2, true, Set(2, 4), Set()),
            Loop(3, true, Set(3, 5), Set())
          ))
        )),
        expectedImmediateDominators = Map(1 -> 0, 2 -> 1, 3 -> 2, 4 -> 2, 5 -> 3),
        expectedDominatorDepths = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 3, 5 -> 4)
      )

      //           4 -> 6
      //           ^
      //           |
      //           v
      // 0 -> 1 -> 2 -> 3 <-> 5 -> 7 <-> 8 -> 9
      //      ^        /
      //       \      /
      //        ------
      'twoNestedLoopsEnds - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          2 -> 3,
          3 -> 1,
          2 -> 4,
          4 -> 2,
          3 -> 5,
          5 -> 3,
          4 -> 6,
          5 -> 7,
          7 -> 8,
          8 -> 7,
          8 -> 9
        ),
        expectedLoopTree = Loop(0, true, Set(0, 9, 6), Set(
          Loop(7, true, Set(7, 8), Set()),
          Loop(1, true, Set(1), Set(
            Loop(2, true, Set(2, 4), Set()),
            Loop(3, true, Set(3, 5), Set())
          ))
        )),
        expectedImmediateDominators = Map(
          1 -> 0, 2 -> 1, 3 -> 2, 4 -> 2, 6 -> 4, 5 -> 3, 7 -> 5, 8 -> 7, 9 -> 8
        ),
        expectedDominatorDepths = Map(
          0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 3, 6 -> 4, 5 -> 4, 7 -> 5, 8 -> 6, 9 -> 7
        )
      )

      //           4
      //           ^             6
      //           |            ^ \
      //           v           /   v
      // 0 -> 1 -> 2 -> 3 <-> 5     7
      //      ^        /       ^   /
      //       \      /         \ v
      //        ------           8
      'twoDoublyNestedLoops - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          2 -> 3,
          3 -> 1,
          2 -> 4,
          4 -> 2,
          3 -> 5,
          5 -> 3,
          5 -> 6,
          6 -> 7,
          7 -> 8,
          8 -> 5
        ),
        expectedLoopTree = Loop(0, true, Set(0), Set(
          Loop(1, true, Set(1), Set(
            Loop(2, true, Set(2, 4), Set()),
            Loop(3, true, Set(3), Set(
              Loop(5, true, Set(5, 6, 7, 8), Set())
            ))
          ))
        )),
        expectedImmediateDominators = Map(
          1 -> 0, 2 -> 1, 3 -> 2, 4 -> 2, 5 -> 3, 6 -> 5, 7 -> 6, 8 -> 7
        ),
        expectedDominatorDepths = Map(
          0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 3, 5 -> 4, 6 -> 5, 7 -> 6, 8 -> 7
        )
      )
    }
    'irreducible - {
      //         -----> 3
      //        /       ^
      //  0 -> 1        |
      //        \       v
      //         -----> 2
      'simple - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          1 -> 3,
          2 -> 3,
          3 -> 2
        ),
        expectedLoopTree = Loop(0, true, Set(0, 1), Set(
          Loop(2, false, Set(2, 3), Set())
        )),
        expectedImmediateDominators = Map(
          1 -> 0,
          2 -> 1,
          3 -> 1
        ),
        expectedDominatorDepths = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 2
        )
      )

      //          ----> 3
      //         /     ^ \
      //        /     /   v
      //  0 -> 1     4 <-> 5
      //        \     ^   /
      //         \     \ v
      //          ----> 2
      'nested - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          1 -> 3,
          2 -> 4,
          4 -> 3,
          3 -> 5,
          5 -> 2,
          4 -> 5,
          5 -> 4

        ),
        expectedLoopTree = Loop(0, true, Set(0, 1), Set(
          Loop(2, false, Set(2), Set(
            Loop(4, false, Set(4, 3, 5), Set())
          ))
        )),
        expectedImmediateDominators = Map(
          1 -> 0,
          2 -> 1,
          3 -> 1,
          4 -> 1,
          5 -> 1
        ),
        expectedDominatorDepths = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 2,
          4 -> 2,
          5 -> 2
        )
      )
      'paper - check[String](
        args = Seq(
          "START" -> "END",
          "START" -> "a",
          "a" -> "b",
          "a" -> "d",
          "b" -> "y",
          "y" -> "d",
          "y" -> "k",
          "d" -> "y",
          "d" -> "t",
          "t" -> "d",
          "k" -> "k",
          "k" -> "b",
          "k" -> "l",
          "l" -> "a",
          "l" -> "END"
        ),
        expectedLoopTree = Loop("START", true, Set("START", "END"), Set(
          Loop("a", true, Set("a", "l"), Set(
            Loop("b", false, Set("b"), Set(
              Loop("y", false, Set("y"), Set(
                Loop("d", true, Set("d", "t"), Set())
              )),
              Loop("k", true, Set("k"), Set())
            ))
          ))
        ))
      )
    }

    'chains - {
      //         -----> 3
      //        /       ^
      //  0 -> 1        |
      //        \       v
      //         -----> 2 -> 4 <-> 5
      'reducibleIrreducible - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          1 -> 3,
          2 -> 3,
          3 -> 2,
          2 -> 4,
          4 -> 5,
          5 -> 4
        ),
        expectedLoopTree = Loop(0, true, Set(0, 1), Set(
          Loop(2, false, Set(2, 3), Set()),
          Loop(4, true, Set(4, 5), Set())
        )),
        expectedImmediateDominators = Map(
          1 -> 0,
          2 -> 1,
          3 -> 1,
          4 -> 2,
          5 -> 4
        ),
        expectedDominatorDepths = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 2,
          4 -> 3,
          5 -> 4
        )
      )

      //         -----> 3
      //        /       ^
      //  0 -> 1        |
      //        \       v
      //         -----> 2 <-> 4
      'reducibleIrreducible2 - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          1 -> 3,
          2 -> 3,
          3 -> 2,
          2 -> 4,
          4 -> 2
        ),
        expectedLoopTree = Loop(0, true, Set(0, 1), Set(
          Loop(2, false, Set(2, 4, 3), Set())
        )),
        expectedImmediateDominators = Map(
          1 -> 0,
          2 -> 1,
          3 -> 1,
          4 -> 2
        ),
        expectedDominatorDepths = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 2,
          4 -> 3
        )
      )

      //          -----> 4
      //         /       ^
      //        /        v
      //  0 -> 1         3 -> 5 <-> 6
      //        \        ^
      //         \       v
      //          -----> 2
      'reducibleIrreducible3 - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          1 -> 4,
          2 -> 3,
          3 -> 2,
          3 -> 4,
          4 -> 3,
          3 -> 5,
          5 -> 6,
          6 -> 5
        ),
        expectedLoopTree = Loop(0, true, Set(0, 1), Set(
          Loop(2, false, Set(2), Set(
            Loop(3, false, Set(3, 4), Set())
          )),
          Loop(5, true, Set(5, 6), Set())
        )),
        expectedImmediateDominators = Map(
          1 -> 0,
          2 -> 1,
          3 -> 1,
          4 -> 1,
          5 -> 3,
          6 -> 5
        ),
        expectedDominatorDepths = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 2,
          4 -> 2,
          5 -> 3,
          6 -> 4
        )
      )
      //          -----> 4
      //         /       ^
      //        /        v
      //  0 -> 1         3 <-> 5
      //        \        ^
      //         \       v
      //          -----> 2
      'reducibleIrreducible4 - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          1 -> 4,
          2 -> 3,
          3 -> 2,
          3 -> 4,
          4 -> 3,
          3 -> 5,
          5 -> 3
        ),
        expectedLoopTree = Loop(0, true, Set(0, 1), Set(
          Loop(2, false, Set(2), Set(
            Loop(3, false, Set(3, 5, 4), Set())
          ))
        )),
        expectedImmediateDominators = Map(
          1 -> 0,
          2 -> 1,
          3 -> 1,
          4 -> 1,
          5 -> 3
        ),
        expectedDominatorDepths = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 2,
          4 -> 2,
          5 -> 3
        )
      )

      //               -----> 3
      //              /       ^
      //  0 -> 1 <-> 2        |
      //              \       v
      //               -----> 4
      'irreducibleReducible - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          2 -> 1,
          2 -> 3,
          2 -> 4,
          3 -> 4,
          4 -> 3
        ),
        expectedLoopTree = Loop(0, true, Set(0), Set(
          Loop(1, true, Set(1, 2), Set()),
          Loop(3, false, Set(3, 4), Set())
        )),
        expectedImmediateDominators = Map(
          1 -> 0,
          2 -> 1,
          3 -> 2,
          4 -> 2
        ),
        expectedDominatorDepths = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 3,
          4 -> 3
        )
      )

      //               ------ 3
      //              v       ^
      //  0 -> 1 <-> 2        |
      //              \       v
      //               -----> 4
      'irreducibleReducible2 - check[Int](
        args = Seq(
          0 -> 1,
          1 -> 2,
          2 -> 1,
          3 -> 2,
          2 -> 4,
          3 -> 4,
          4 -> 3
        ),
        expectedLoopTree = Loop(0, true, Set(0), Set(
          Loop(1, true, Set(1), Set(
            Loop(2, true, Set(2), Set(
              Loop(4,true,Set(4, 3),Set())
            ))
          ))
        )),
        expectedImmediateDominators = Map(
          1 -> 0,
          2 -> 1,
          3 -> 4,
          4 -> 2
        ),
        expectedDominatorDepths = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 4,
          4 -> 3
        )
      )
    }
  }
}
