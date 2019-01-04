package joptimize
import utest._
object UnitTests extends TestSuite{
  val tests = Tests{
    'leastUpperBound - {
      Util.leastUpperBound(Set(0))(_ => Nil) ==> Set(0)
      Util.leastUpperBound(Set(0, 1))(Array(List(2), List(2), Nil)) ==> Set(2)
      Util.leastUpperBound(Set(0, 1))(Array(List(2, 3), List(2, 3), Nil, Nil)) ==> Set(2, 3)
      Util.leastUpperBound(Set(0, 1))(Array(List(2, 3), List(2, 3), List(3), Nil)) ==> Set(2)
    }
  }
}
