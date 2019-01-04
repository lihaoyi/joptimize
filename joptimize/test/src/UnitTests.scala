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
      def check[T](args: Seq[(T, T)], expected: SimpleLoop[T]) = {
        val analyzed = LoopFinder.analyzeLoops(args)
        assert(analyzed == expected)
      }
      'reducible - {

        'twoNode - check[Int](
          args = Seq(0 -> 1),
          expected = SimpleLoop(0, Set(0), true, Set(0), Set())
        )
        'threeNode - check[Int](
          args = Seq(0 -> 1, 1 -> 2),
          expected = SimpleLoop(0, Set(0), true, Set(0), Set())
        )
        'diamong - check[Int](
          args = Seq(0 -> 1, 1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4),
          expected = SimpleLoop(0, Set(0), true, Set(0), Set())
        )
        'loop - check[Int](
          args = Seq(0 -> 1, 1 -> 2, 2 -> 1),
          expected = SimpleLoop(0, Set(0), true, Set(0), Set(
            SimpleLoop(1, Set(1), true, Set(1, 2), Set())
          ))
        )
        'nestedLoop - check[Int](
          args = Seq(0 -> 1, 1 -> 2, 2 -> 1, 2 -> 3, 3 -> 2),
          expected = SimpleLoop(0, Set(0), true, Set(0), Set(
            SimpleLoop(1, Set(1), true, Set(1), Set(
              SimpleLoop(2, Set(2), true, Set(2, 3), Set())
            ))
          ))
        )
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
          expected = SimpleLoop(0, Set(0), true, Set(0), Set(
            SimpleLoop(1, Set(1), true, Set(1), Set(
              SimpleLoop(2, Set(2), true, Set(2, 4), Set()),
              SimpleLoop(3, Set(3), true, Set(3, 5), Set())
            ))
          ))
        )
      }
      'irreducible - {
        'simple - check[Int](
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
          expected = SimpleLoop("START",Set("START"),true,Set("START"),Set(
            SimpleLoop("a",Set("a"),true,Set("a", "l"),Set(
              SimpleLoop("b",Set("b"),false,Set("b"),Set(
                SimpleLoop("y",Set("y"),false,Set("y"),Set(
                  SimpleLoop("d",Set("d"),true,Set("d", "t"),Set())
                )),
                SimpleLoop("k",Set("k"),true,Set("k"),Set())
              ))
            ))
          ))
        )
      }
    }
  }
}
