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

        check(5, 32, "Loops", "basicFor", "(I)I", Seq(classOf[Int]))
        check(5, 8, "Loops", "basicWhile", "(I)I", Seq(classOf[Int]))

        check(121.0, 11.143835769253432, "Loops", "sqrtFinder", "(D)D", Seq(classOf[Double]))

        check(0, 1, "Switches", "smallSwitch", "(I)I", Seq(classOf[Int]))
        check(1, 0, "Switches", "smallSwitch", "(I)I", Seq(classOf[Int]))
        check(2, 2, "Switches", "smallSwitch", "(I)I", Seq(classOf[Int]))
        check(3, 2, "Switches", "smallSwitch", "(I)I", Seq(classOf[Int]))

        check(0, 1123.213, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(1, 3212.321, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(2, 123123.123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(3, 123.312, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(4, 123123.1231, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(5, 1231.3212, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(6, 123.132123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(7, 32123.123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(8, 123123.12312, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(9, 123123.3123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(10, 123123.1312, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(11, 123123.2123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(12, 123321.123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(13, 123123.12312, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(14, 123123.1231, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(15, 1321231.1231, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(16, 23123123.1231, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(17, 123123123.123123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(18, 123123.1231, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(19, 23123.12321, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(20, 12312312.321, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(21, 1232312.312, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(22, 123123123.132123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(23, 123123123.1231, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(24, 132123.1231, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(25, 12312321.123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(26, 1232123.312, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(27, 123123.12312, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(28, 13212312.123123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(29, 2123123.1231231, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(30, 123123.123123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))
        check(31, 123123.123123, "Switches", "bigDenseSwitch", "(I)D", Seq(classOf[Int]))

        check(1, 3212.321, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(2, 123123.123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(4, 123.312, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(8, 123123.1231, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(16, 1231.3212, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(32, 123.132123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(62, 32123.123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(128, 123123.12312, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(256, 123123.3123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(512, 123123.1312, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(1024, 123123.2123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(2048, 123321.123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(4096, 123123.12312, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(8192, 123123.1231, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(16384, 1321231.1231, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(32768, 23123123.1231, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(65536, 123123123.123123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(131072, 123123.1231, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(262144, 23123.12321, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(524288, 12312312.321, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(1048576, 1232312.312, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(2097152, 123123123.132123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))
        check(-1, 123123.123123, "Switches", "bigSparseSwitch", "(I)D", Seq(classOf[Int]))


        check('a', 1, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('b', 2, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('c', 3, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('d', 4, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('e', 5, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('f', 6, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('g', 7, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('h', 8, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('i', 9, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('j', 0, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))
        check('z', 10, "Switches", "charSwitch", "(C)I", Seq(classOf[Char]))

        check(1.toByte,      1, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
        check(2.toByte,      2, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
        check(4.toByte,      3, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
        check(16.toByte,     4, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
        check(32.toByte,     5, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
        check(64.toByte,     6, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
        check(127.toByte,    7, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
        check(-128.toByte,   8, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
        check(3.toByte,     10, "Switches", "byteSwitch", "(B)I", Seq(classOf[Byte]))
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
