package joptimize
import utest._
object MainTests extends TestSuite{
  val classesRoot = os.Path("out/joptimize/test/compile/dest/classes", os.pwd)
  val testRoot = classesRoot / "joptimize" / "examples"
  def check()(implicit tp: utest.framework.TestPath) = {
    JOptimize.run(
      os.walk(testRoot / tp.value.last)
        .map(p => (p.relativeTo(classesRoot).toString, os.read.bytes(p)))
        .toMap,
      Seq(s"joptimize/examples/${tp.value.last}/Main.class" -> "main")
    )
  }
  val tests = Tests{
    'supertype - check()
    'supertypeindirect - check()
    'parametric - check()
    'parametricindirect - check()
    'lambda - check()
    'lambdaindirect - check()
  }
}
