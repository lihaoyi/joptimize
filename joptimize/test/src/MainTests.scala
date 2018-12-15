package joptimize
import java.net.URLClassLoader

import org.objectweb.asm.Type
import utest._
import utest.framework.TestPath

import scala.reflect.ClassTag
object MainTests extends TestSuite{
  val classesRoot = os.Path("out/joptimize/test/compile/dest/classes", os.pwd)
  val outRoot = os.Path("out/scratch", os.pwd)
  val testRoot = classesRoot / "joptimize" / "examples"

  class Optimized(clsName: String,
                  methodName: String,
                  returnClass: Class[_],
                  argClasses: Class[_]*)
                 (implicit tp: TestPath){
    val methodDesc = Type.getMethodDescriptor(
      Type.getType(returnClass), argClasses.map(Type.getType):_*
    )
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


    def check0(cases: (Any, Any)*) = {
      for ((args, expected) <- cases) {
        val cl = new URLClassLoader(Array((outRoot / tp.value.last).toNIO.toUri.toURL))
        try {
          val cls = cl.loadClass(s"joptimize.examples.${tp.value.mkString(".")}.$clsName")
          val method = cls.getMethod(methodName, argClasses: _*)
          val argsList = args match {
            case () => Nil
            case p: Product => p.productIterator.toSeq
            case x => Seq(x)
          }
          val res = method.invoke(null, argsList.asInstanceOf[Seq[AnyRef]]: _*)

          // Make sure the correct value is computed
          (res, expected) match {
            case (a: Array[AnyRef], b: Array[AnyRef]) =>
              assert {
                java.util.Arrays.deepEquals(a, b)
              }
            case _ => assert {
              res == expected
            }
          }


//          if (checkMissing) {
//            val cls2 = cl.loadClass(s"joptimize.examples.${tp.value.mkString(".")}.$clsName$$")
//            assert(
//               That the previously-existing method has been removed
//              !cls2.getMethods.exists(_.getName == "call"),
//               And the n>=2 duplicate methods are in place (presumably being used)
//              cls2.getMethods.count(_.getName.startsWith("call")) >= 2
//            )
//          }
        } finally {
          cl.close()
        }
      }
    }
  }

  def classOf0[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass
  case class opt0[R: ClassTag]
                 (cls: String, method: String)
                 (implicit tp: TestPath) extends Optimized(cls, method, classOf0[R]){

    def check(args: (Unit, R)*) = check0(args:_*)
  }
  case class opt1[T1: ClassTag, R: ClassTag]
                 (cls: String, method: String)
                 (implicit tp: TestPath) extends Optimized(cls, method, classOf0[R], classOf0[T1]){
    def check(args: (T1, R)*) = check0(args:_*)
  }
  case class opt2[T1: ClassTag, T2: ClassTag, R: ClassTag]
                 (cls: String, method: String)
                 (implicit tp: TestPath) extends Optimized(cls, method, classOf0[R], classOf0[T1], classOf0[T2]){
    def check(args: ((T1, T2), R)*) = check0(args:_*)
  }
  case class opt3[T1: ClassTag, T2: ClassTag, T3: ClassTag, R: ClassTag]
                 (cls: String, method: String)
                 (implicit tp: TestPath) extends Optimized(cls, method, classOf0[R], classOf0[T1], classOf0[T2], classOf0[T3]){
    def check(args: ((T1, T2, T3), R)*) = check0(args:_*)
  }
  def tests = Tests{
    'simple - {
      'arrays - {
        opt2[Int, Int, Array[Array[Int]]]("MultiDimArrays", "make2D")
            .check((1, 2) -> Array.fill(1, 2)(0))
        opt3[Int, Int, Int, Array[Array[Array[Int]]]]("MultiDimArrays", "make3D")
            .check((1, 2, 3) -> Array.fill(1, 2, 3)(0))
        opt0[Int]("MultiDimArrays", "getAndSet")
          .check(() -> 900)
      }
      'controlflow - {
        opt2[Int, Int, Int]("IfElse", "basicIf").check(
          (10, 11) -> 10,
          (10, 9) -> -10
        )

        opt1[Int, Int]("IfElse", "ifNonIntZero").check(
          10 -> 10,
          -9 -> 9
        )

        opt2[Int, Int, Int]("IfElse", "ifNonIntBinary").check(
          (10, 11) -> -10,
          (10, 9) -> 10,
          (200, 200) -> -200
        )

        opt2[Int, Int, Int]("IfElse", "ifElseIf").check(
          (3, 2) -> 3,
          (2, 2) -> -2,
          (1, 2) -> 2
        )

        opt2[Int, Int, Int]("IfElse", "ifElseIfBig").check(
          (2, 1) -> 1,
          (13, 13) -> 2,
          (9, 9) -> 3,
          (11, 11) -> 4,
          (1, 10) -> 5
        )

        opt1[Int, Int]("Loops", "basicFor").check(
          5 -> 32
        )
        opt1[Int, Int]("Loops", "basicWhile").check(
          5 -> 8
        )
        opt1[Double, Double]("Loops", "sqrtFinder").check(
          121.0 -> 11.143835769253432
        )

        opt1[Int, Int]("Switches", "smallSwitch").check(
          0 -> 1,
          1 -> 0,
          2 -> 2,
          3 -> 2,
        )

        opt1[Int, Double]("Switches", "bigDenseSwitch").check(
          0 -> 1123.213,
          1 -> 3212.321,
          2 -> 123123.123,
          3 -> 123.312,
          4 -> 123123.1231,
          5 -> 1231.3212,
          6 -> 123.132123,
          7 -> 32123.123,
          8 -> 123123.12312,
          9 -> 123123.3123,
          10 -> 123123.1312,
          11 -> 123123.2123,
          12 -> 123321.123,
          13 -> 123123.12312,
          14 -> 123123.1231,
          15 -> 1321231.1231,
          16 -> 23123123.1231,
          17 -> 123123123.123123,
          18 -> 123123.1231,
          19 -> 23123.12321,
          20 -> 12312312.321,
          21 -> 1232312.312,
          22 -> 123123123.132123,
          23 -> 123123123.1231,
          24 -> 132123.1231,
          25 -> 12312321.123,
          26 -> 1232123.312,
          27 -> 123123.12312,
          28 -> 13212312.123123,
          29 -> 2123123.1231231,
          30 -> 123123.123123,
          31 -> 123123.123123,
        )

        opt1[Int, Double]("Switches", "bigSparseSwitch").check(
          1 -> 3212.321,
          2 -> 123123.123,
          4 -> 123.312,
          8 -> 123123.1231,
          16 -> 1231.3212,
          32 -> 123.132123,
          62 -> 32123.123,
          128 -> 123123.12312,
          256 -> 123123.3123,
          512 -> 123123.1312,
          1024 -> 123123.2123,
          2048 -> 123321.123,
          4096 -> 123123.12312,
          8192 -> 123123.1231,
          16384 -> 1321231.1231,
          32768 -> 23123123.1231,
          65536 -> 123123123.123123,
          131072 -> 123123.1231,
          262144 -> 23123.12321,
          524288 -> 12312312.321,
          1048576 -> 1232312.312,
          2097152 -> 123123123.132123,
          -1 -> 123123.123123
        )


        opt1[Char, Int]("Switches", "charSwitch").check(
          'a' -> 1,
          'b' -> 2,
          'c' -> 3,
          'd' -> 4,
          'e' -> 5,
          'f' -> 6,
          'g' -> 7,
          'h' -> 8,
          'i' -> 9,
          'j' -> 0,
          'z' -> 10
        )

        opt1[Byte, Int]("Switches", "byteSwitch").check(
          1.toByte -> 1,
          2.toByte -> 2,
          4.toByte -> 3,
          16.toByte -> 4,
          32.toByte -> 5,
          64.toByte -> 6,
          127.toByte -> 7,
          -128.toByte -> 8,
          3.toByte -> 10,
        )
      }
      'statics - {
        opt1[Int, Int]("Statics", "helloWorld").check(
          1 -> 2,
          2 -> 4
        )
        opt1[Int, Int]("Statics", "timesTwo").check(
          1 -> 2,
          2 -> 4
        )
        opt2[Int, Int, Int]("Statics", "helloWorld2").check(
          (1, 1) -> 0,
          (5, 2) -> 6
        )
        opt2[Int, Int, Int]("Statics", "timesTwo2").check(
          (1, 1) -> 0,
          (5, 2) -> 6
        )
        opt1[Int, Int]("Statics", "tailFactorial").check(
          1 -> 1,
          2 -> 2,
          3 -> 6,
          4 -> 24
        )
        opt1[Int, Int]("Statics", "fibonacci").check(
          1 -> 1,
          2 -> 2,
          3 -> 3,
          4 -> 5,
          5 -> 8
        )
        opt1[Int, Int]("Statics", "call").check(
          1 -> 2,
          2 -> 3,
          3 -> 4
        )
        opt1[Int, Int]("Statics", "callAtPhiBoundary").check(
          -1 -> 1,
          0 -> 1,
          1 -> 2
        )
      }
    }
    'narrow - {
      'supertype - opt2("Main", "main").check((1, 2) -> 6)
      'supertypeindirect - opt2("Main", "main").check((1, 2) -> 6)
      'parametric - opt2("Main", "main").check((1, 2) -> 6)
      'parametricindirect - opt2("Main", "main").check((1, 2) -> 6)
      'narrowreturn - opt2("Main", "main").check((1, 2) -> 6)
      'forcewide - opt2("Main", "main").check((1, 2) -> 6)
      //    'lambda - check()
      //    'lambdaindirect - check()
    }
  }
}
