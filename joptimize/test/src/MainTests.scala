package joptimize
import utest._
object MainTests extends TestSuite{
  val classesRoot = os.Path("out/joptimize/test/compile/dest/classes", os.pwd)
  val outRoot = os.Path("out/scratch", os.pwd)
  val testRoot = classesRoot / "joptimize" / "examples"
  def check()(implicit tp: utest.framework.TestPath) = {
    val outputFileMap = JOptimize.run(
      os.walk(testRoot / tp.value.last)
        .map(p => (p.relativeTo(classesRoot).toString, os.read.bytes(p)))
        .toMap,
      Seq(MethodSig(s"joptimize/examples/${tp.value.last}/Main$$", "main", "(II)I"))
    )

    os.remove.all(outRoot / tp.value.last)
    for((k, bytes) <- outputFileMap){
      os.write(outRoot / tp.value.last / os.RelPath(k), bytes, createFolders = true)
    }
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
