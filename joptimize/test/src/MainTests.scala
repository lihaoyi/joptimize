package joptimize
import java.net.URLClassLoader

import utest._
object MainTests extends TestSuite{
  val classesRoot = os.Path("out/joptimize/test/compile/dest/classes", os.pwd)
  val outRoot = os.Path("out/scratch", os.pwd)
  val testRoot = classesRoot / "joptimize" / "examples"
  def check(x: Int, y: Int, expected: Int)(implicit tp: utest.framework.TestPath) = {
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

    val cl = new URLClassLoader(Array((outRoot / tp.value.last).toNIO.toUri.toURL))
    try{
      val cls = cl.loadClass(s"joptimize.examples.${tp.value.last}.Main")
      val method = cls.getMethod("main", classOf[Int], classOf[Int])
      val res = method.invoke(null, Integer.valueOf(x), Integer.valueOf(y))
      assert(res == expected)
    }finally{
      cl.close()
    }
  }
  val tests = Tests{

    'supertype - check(1, 2, 6)
    'supertypeindirect - check(1, 2, 6)
//    'parametric - check()
//    'parametricindirect - check()
//    'lambda - check()
//    'lambdaindirect - check()
  }
}
