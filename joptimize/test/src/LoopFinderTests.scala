package joptimize
import joptimize.analysis.LoopFinder
import joptimize.analysis.LoopFinder.Loop
import utest._

object LoopFinderTests extends TestSuite{
  val tests = Tests{
    def check[T](args: Seq[(T, T)], expected: Loop[T]) = {
      val analyzed = LoopFinder.analyzeLoops(args)
      assert(analyzed == expected)
    }

    'reducible - {
      // 0 -> 1
      'twoNode - check[Int](
        args = Seq(0 -> 1),
        expected = Loop(Set(0), true, Set(0, 1), Set())
      )
      // 0 -> 1 -> 2
      'threeNode - check[Int](
        args = Seq(0 -> 1, 1 -> 2),
        expected = Loop(Set(0), true, Set(0, 1, 2), Set())
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
        expected = Loop(Set(0), true, Set(0, 1, 2, 3, 4), Set())
      )

      // 0 -> 1 <-> 2
      'loop - check[Int](
        args = Seq(0 -> 1, 1 -> 2, 2 -> 1),
        expected = Loop(Set(0), true, Set(0), Set(
          Loop(Set(1), true, Set(1, 2), Set())
        ))
      )
      // 0 -> 1 <-> 2 -> 3
      'loopEnd - check[Int](
        args = Seq(0 -> 1, 1 -> 2, 2 -> 1, 2 -> 3),
        expected = Loop(Set(0), true, Set(0, 3), Set(
          Loop(Set(1), true, Set(1, 2), Set())
        ))
      )

      // 0 -> 1 <-> 2 <-> 3
      'nestedLoop - check[Int](
        args = Seq(0 -> 1, 1 -> 2, 2 -> 1, 2 -> 3, 3 -> 2),
        expected = Loop(Set(0), true, Set(0), Set(
          Loop(Set(1), true, Set(1), Set(
            Loop(Set(2), true, Set(2, 3), Set())
          ))
        ))
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
        expected = Loop(Set(0), true, Set(0), Set(
          Loop(Set(1), true, Set(1), Set(
            Loop(Set(2), true, Set(2, 4), Set()),
            Loop(Set(3), true, Set(3, 5), Set())
          ))
        ))
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
        expected = Loop(Set(0), true, Set(0, 9, 6), Set(
          Loop(Set(7), true, Set(7, 8), Set()),
          Loop(Set(1), true, Set(1), Set(
            Loop(Set(2), true, Set(2, 4), Set()),
            Loop(Set(3), true, Set(3, 5), Set())
          ))
        ))
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
        expected = Loop(Set(0), true, Set(0), Set(
          Loop(Set(1), true, Set(1), Set(
            Loop(Set(2), true, Set(2, 4), Set()),
            Loop(Set(3), true, Set(3), Set(
              Loop(Set(5), true, Set(5, 6, 7, 8), Set())
            ))
          ))
        ))
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
        expected = Loop(Set(0), true, Set(0, 1), Set(
          Loop(Set(2), false, Set(2, 3), Set())
        ))
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
        expected = Loop(Set(0), true, Set(0, 1), Set(
          Loop(Set(2), false, Set(2), Set(
            Loop(Set(4), false, Set(4, 3, 5), Set())
          ))
        ))
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
        expected = Loop(Set("START"), true, Set("START", "END"), Set(
          Loop(Set("a"), true, Set("a", "l"), Set(
            Loop(Set("b"), false, Set("b"), Set(
              Loop(Set("y"), false, Set("y"), Set(
                Loop(Set("d"), true, Set("d", "t"), Set())
              )),
              Loop(Set("k"), true, Set("k"), Set())
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
        expected = Loop(Set(0), true, Set(0, 1), Set(
          Loop(Set(2), false, Set(2, 3), Set()),
          Loop(Set(4), true, Set(4, 5), Set())
        ))
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
        expected = Loop(Set(0), true, Set(0, 1), Set(
          Loop(Set(2), false, Set(2, 4, 3), Set())
        ))
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
        expected = Loop(Set(0), true, Set(0, 1), Set(
          Loop(Set(2), false, Set(2), Set(
            Loop(Set(3), false, Set(3, 4), Set())
          )),
          Loop(Set(5), true, Set(5, 6), Set())
        ))
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
        expected = Loop(Set(0), true, Set(0, 1), Set(
          Loop(Set(2), false, Set(2), Set(
            Loop(Set(3), false, Set(3, 5, 4), Set())
          ))
        ))
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
        expected = Loop(Set(0), true, Set(0), Set(
          Loop(Set(1), true, Set(1, 2), Set()),
          Loop(Set(3), false, Set(3, 4), Set())
        ))
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
        expected = Loop(Set(0), true, Set(0), Set(
          Loop(Set(1), true, Set(1), Set(
            Loop(Set(2), true, Set(2), Set(
              Loop(Set(4),true,Set(4, 3),Set())
            ))
          ))
        ))
      )
    }
  }
}
