package joptimize
import java.net.URLClassLoader

import utest._
object MainTests extends TestSuite{
  val classesRoot = os.Path("out/joptimize/test/compile/dest/classes", os.pwd)
  val outRoot = os.Path("out/scratch", os.pwd)
  val testRoot = classesRoot / "joptimize" / "examples"
  def check(args: Any,
            expected: Any,
            clsName: String = "Main",
            methodName: String = "main",
            methodDesc: String = "(II)I",
            methodSigClasses: Seq[Class[_]] = Seq(classOf[Int], classOf[Int]),
            checkMissing: Boolean = false)
           (implicit tp: utest.framework.TestPath,
            line: sourcecode.Line) = {

    val outputFileMap = JOptimize.run(
      os.walk(testRoot / tp.value)
        .map(p => (p.relativeTo(classesRoot).toString, os.read.bytes(p)))
        .toMap,
      Seq(MethodSig(s"joptimize/examples/${tp.value.mkString("/")}/$clsName", methodName, methodDesc, static = true)),
      eliminateOldMethods = true
    )

    os.remove.all(outRoot / tp.value.last)
    for((k, bytes) <- outputFileMap){
      os.write(outRoot / tp.value.last / os.RelPath(k), bytes, createFolders = true)
    }

    val cl = new URLClassLoader(Array((outRoot / tp.value.last).toNIO.toUri.toURL))
    try{
      val cls = cl.loadClass(s"joptimize.examples.${tp.value.mkString(".")}.$clsName")
      val method = cls.getMethod(methodName, methodSigClasses:_*)
      val argsList = args match{
        case () => Nil
        case p: Product => p.productIterator.toSeq
        case x => Seq(x)
      }
      val res = method.invoke(null, argsList.asInstanceOf[Seq[AnyRef]]:_*)

      // Make sure the correct value is computed
      (res, expected) match{
        case (a: Array[AnyRef], b: Array[AnyRef]) =>
          assert{
            line
            java.util.Arrays.deepEquals(a, b)
          }
        case _ => assert{
          line
          res == expected
        }
      }


      if (checkMissing){
        val cls2 = cl.loadClass(s"joptimize.examples.${tp.value.mkString(".")}.$clsName$$")
        assert(
          // That the previously-existing method has been removed
          !cls2.getMethods.exists(_.getName == "call"),
          // And the n>=2 duplicate methods are in place (presumably being used)
          cls2.getMethods.count(_.getName.startsWith("call")) >= 2
        )
      }
    }finally{
      cl.close()
    }
  }
  def tests = Tests{
    'simple - {
      'arrays - {
        check((1, 2), Array.fill(1, 2)(0), "MultiDimArrays", "make2D", "(II)[[I", Seq(classOf[Int], classOf[Int]))
        check((1, 2, 3), Array.fill(1, 2, 3)(0), "MultiDimArrays", "make3D", "(III)[[[I", Seq(classOf[Int], classOf[Int], classOf[Int]))
        check((), 900, "MultiDimArrays", "getAndSet", "()I", Nil)
      }
      'controlflow - {
        check((10, 11), 10, "IfElse", "basicIf", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((10, 9), -10, "IfElse", "basicIf", "(II)I", Seq(classOf[Int], classOf[Int]))

        check(10, 10, "IfElse", "ifNonIntZero", "(I)I", Seq(classOf[Int]))
        check(-9, 9, "IfElse", "ifNonIntZero", "(I)I", Seq(classOf[Int]))

        check((10, 11), -10, "IfElse", "ifNonIntBinary", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((10, 9), 10, "IfElse", "ifNonIntBinary", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((200, 200), -200, "IfElse", "ifNonIntBinary", "(II)I", Seq(classOf[Int], classOf[Int]))

        check((3, 2), 3, "IfElse", "ifElseIf", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((2, 2), -2, "IfElse", "ifElseIf", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((1, 2), 2, "IfElse", "ifElseIf", "(II)I", Seq(classOf[Int], classOf[Int]))

        check((2, 1), 1, "IfElse", "ifElseIfBig", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((13, 13), 2, "IfElse", "ifElseIfBig", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((9, 9), 3, "IfElse", "ifElseIfBig", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((11, 11), 4, "IfElse", "ifElseIfBig", "(II)I", Seq(classOf[Int], classOf[Int]))
        check((1, 10), 5, "IfElse", "ifElseIfBig", "(II)I", Seq(classOf[Int], classOf[Int]))
      }
    }
    'narrow - {
      'supertype - check((1, 2), 6, checkMissing = true)
      'supertypeindirect - check((1, 2), 6, checkMissing = true)
      'parametric - check((1, 2), 6, checkMissing = true)
      'parametricindirect - check((1, 2), 6, checkMissing = true)
      'narrowreturn - check((1, 2), 6, checkMissing = true)
      //    'lambda - check()
      //    'lambdaindirect - check()
    }
  }
}
