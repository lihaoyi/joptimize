package joptimize
import joptimize.analysis.LoopFinder
import joptimize.analysis.LoopFinder.SimpleLoop
import utest._
object UnitTests extends TestSuite{
  val tests = Tests{
    'leastUpperBound - {
      Util.leastUpperBound(Set(0))(_ => Nil) ==> Set(0)
      Util.leastUpperBound(Set(0, 1))(Array(List(2), List(2), Nil)) ==> Set(2)
      Util.leastUpperBound(Set(0, 1))(Array(List(2, 3), List(2, 3), Nil, Nil)) ==> Set(2, 3)
      Util.leastUpperBound(Set(0, 1))(Array(List(2, 3), List(2, 3), List(3), Nil)) ==> Set(2)
    }
    'loopfinder - {
      def check(args: Seq[(Int, Int)], expected: SimpleLoop[Int]) = {
        val analyzed = LoopFinder.analyzeLoops(args)
        assert(analyzed == expected)
      }

      * - check(
        args = Seq(0 -> 1),
        expected = SimpleLoop(0, Set(0), true, Set(0), Set())
      )
      * - check(
        args = Seq(0 -> 1, 1 -> 2),
        expected = SimpleLoop(0, Set(0), true, Set(0), Set())
      )
      * - check(
        args = Seq(0 -> 1, 1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4),
        expected = SimpleLoop(0, Set(0), true, Set(0), Set())
      )
      * - check(
        args = Seq(0 -> 1, 1 -> 2, 2 -> 1),
        expected = SimpleLoop(0, Set(0), true, Set(0), Set(
          SimpleLoop(1, Set(1), true, Set(1, 2), Set())
        ))
      )
      * - check(
        args = Seq(0 -> 1, 1 -> 2, 2 -> 1, 2 -> 3, 3 -> 2),
        expected = SimpleLoop(0, Set(0), true, Set(0), Set(
          SimpleLoop(1, Set(1), true, Set(1), Set(
            SimpleLoop(2, Set(2), true, Set(2, 3), Set())
          ))
        ))
      )
      * - check(
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
        expected = SimpleLoop(0, Set(0), true, Set(0), Set(
          SimpleLoop(1, Set(1), true, Set(1), Set(
            SimpleLoop(2, Set(2), true, Set(2, 4), Set()),
            SimpleLoop(3, Set(3), true, Set(3, 5), Set())
          ))
        ))
      )
      * - check(
        args = Seq(
          0 -> 1,
          1 -> 2,
          1 -> 3,
          2 -> 3,
          3 -> 2
        ),
        expected = SimpleLoop(0, Set(0), true, Set(0), Set(
          SimpleLoop(2, Set(2, 3), false, Set(2, 3), Set())
        ))
      )

    }
  }
}
