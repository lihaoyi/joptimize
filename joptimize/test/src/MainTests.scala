package joptimize
import java.net.URLClassLoader

import org.objectweb.asm.Type
import utest._
import utest.framework.TestPath

import scala.reflect.ClassTag
object MainTests extends TestSuite{
  def tests = Tests{
    'simple - {
      'IfElse - {
        'basicIf - opt2[Int, Int, Int]().check(
          (10, 11) -> 10,
          (10, 9) -> -10
        )
        'ifNonIntZero - opt1[Int, Int]().check(
          10 -> 10,
          -9 -> 9
        )
        'ifNonIntBinary - opt2[Int, Int, Int]().check(
          (10, 11) -> -10,
          (10, 9) -> 10,
          (200, 200) -> -200
        )
        'ifElseIf - opt2[Int, Int, Int]().check(
          (3, 2) -> 3,
          (2, 2) -> -2,
          (1, 2) -> 2
        )
        'ifElseIfBig - opt2[Int, Int, Int]().check(
          (2, 1) -> 1,
          (13, 13) -> 2,
          (9, 9) -> 3,
          (11, 11) -> 4,
          (1, 10) -> 5
        )
      }
      'Loops - {
        'basicFor - opt1[Int, Int]().check(
          5 -> 32
        )

        'basicWhile - opt1[Int, Int]().check(
          5 -> 8
        )
        'sqrtFinder - opt1[Double, Double]().check(
          121.0 -> 11.143835769253432
        )

      }

      'Switches - {
        'smallSwitch - opt1[Int, Int]().check(
          0 -> 1,
          1 -> 0,
          2 -> 2,
          3 -> 2,
        )

        'bigDenseSwitch - opt1[Int, Double]().check(
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

        'bigSparseSwitch - opt1[Int, Double]().check(
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
        'charSwitch - opt1[Char, Int]().check(
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

        'byteSwitch - opt1[Byte, Int]().check(
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
      'Statics - {
        'helloWorld - opt1[Int, Int]().check(
          1 -> 2,
          2 -> 4
        )
        'timesTwo - opt1[Int, Int]().check(
          1 -> 2,
          2 -> 4
        )
        'helloWorld2 - opt2[Int, Int, Int]().check(
          (1, 1) -> 0,
          (5, 2) -> 6
        )
        'timesTwo2 - opt2[Int, Int, Int]().check(
          (1, 1) -> 0,
          (5, 2) -> 6
        )
        'tailFactorial - opt1[Int, Int]().check(
          1 -> 1,
          2 -> 2,
          3 -> 6,
          4 -> 24
        )
        'fibonacci - opt1[Int, Int]().check(
          1 -> 1,
          2 -> 2,
          3 -> 3,
          4 -> 5,
          5 -> 8
        )
        'call - opt1[Int, Int]().check(
          1 -> 2,
          2 -> 3,
          3 -> 4
        )
        'callAtPhiBoundary - opt1[Int, Int]().check(
          -1 -> 1,
          0 -> 1,
          1 -> 2
        )
      }
      'MultiDimArrays - {
        'make2D - opt2[Int, Int, Array[Array[Int]]]().check((1, 2) -> Array.fill(1, 2)(0))
        'make3D - opt3[Int, Int, Int, Array[Array[Array[Int]]]]().check((1, 2, 3) -> Array.fill(1, 2, 3)(0))
        'getAndSet - opt0[Int]().check(() -> 900)
      }
    }
    'narrow - {
      'Supertype - {
        'main - opt2[Int, Int, Int]()
          .check((1, 2) -> 6)
          .checkRemoved("Supertype$.call")
          .checkMangled("Supertype$.call")
      }
      'Parametric - {
        'main - opt2[Int, Int, Int]()
          .check((1, 2) -> 6)
          .checkRemoved("Parametric$.call")
          .checkMangled("Parametric$.call")
      }
      'Supertype2 - {
        'main - opt2[Int, Int, Int]()
          .check((1, 2) -> 6)
          .checkRemoved("Supertype2$.call")
          .checkMangled("Supertype2$.call")
      }
      'Parametric2 - {
        'main - opt2[Int, Int, Int]()
          .check((1, 2) -> 6)
          .checkRemoved("Parametric2$.call")
          .checkMangled("Parametric2$.call")
      }
      'NarrowReturn - {
        'main - opt2[Int, Int, Int]()
          .check((1, 2) -> 6)
          .checkRemoved("NarrowReturn$.call")
          .checkMangled("NarrowReturn$.call")
      }
      'ForceWide - {
        'main - opt2[Int, Int, Int]()
          .check((1, 2) -> 6)
          .checkPresent("ForceWide$.call")
      }
    }
    'opt - {
      'SimpleDce - {
        'main - opt2[Int, Int, Int]()
          .check((1, 2) -> 6)
          .checkPresent("SimpleDce$.call1", "SimpleDce$.call2")
          .checkRemoved("SimpleDce$.call3")
      }
      'InstanceDce - {
        'simple1 - opt2[Int, Int, Int]()
          .check((1, 2) -> 6)
          .checkPresent("SimpleDce$.call1", "SimpleDce$.call2")
          .checkRemoved("SimpleDce$.call3")
      }
    }
  }

  val classesRoot = os.Path("out/joptimize/test/compile/dest/classes", os.pwd)
  val outRoot = os.Path("out/scratch", os.pwd)
  val testRoot = classesRoot / "joptimize" / "examples"

  class Optimized(returnClass: Class[_],
                  argClasses: Class[_]*)
                 (implicit tp: TestPath){
    val methodDesc = Type.getMethodDescriptor(
      Type.getType(returnClass), argClasses.map(Type.getType):_*
    )

    val outputFileMap = JOptimize.run(
      os.walk(testRoot / tp.value.dropRight(2))
        .filter(os.isFile)
        .map(p => (p.relativeTo(classesRoot).toString, os.read.bytes(p)))
        .toMap,
      Seq(MethodSig(s"joptimize/examples/${tp.value.dropRight(1).mkString("/")}", tp.value.last, methodDesc, static = true)),
      eliminateOldMethods = true
    )

    os.remove.all(outRoot / tp.value.last)
    for((k, bytes) <- outputFileMap){
      os.write(outRoot / tp.value.last / os.RelPath(k), bytes, createFolders = true)
    }

    def checkWithClassloader(f: URLClassLoader => Any): this.type = {
      val cl = new URLClassLoader(Array((outRoot / tp.value.last).toNIO.toUri.toURL))
      try {
        f(cl)
      } finally {
        cl.close()
      }
      this
    }

    def check0(cases: (Any, Any)*) = {
      for ((args, expected) <- cases) checkWithClassloader{ cl =>
        val cls = cl.loadClass(s"joptimize.examples.${tp.value.dropRight(1).mkString(".")}")
        val method = cls.getMethod(tp.value.last, argClasses: _*)
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
      }
      this
    }

    def resolveMethod(sigString: String, cl: ClassLoader) = {

      val (clsName, methodName) =
        if (sigString.contains('#')){
          val Array(clsName, methodName) = sigString.split('#')
          val clsNameChunks = clsName.split('.')
          (clsNameChunks.last, methodName)
        }else{
          val clsNameChunks0 = sigString.split('.')
          val clsNameChunks = clsNameChunks0.dropRight(1)
          val methodName = clsNameChunks0.last
          (clsNameChunks.last, methodName)
        }

      val cls2 = cl.loadClass(
        s"joptimize.examples.${tp.value.dropRight(2).mkString(".")}.$clsName"
      )
      (cls2, methodName)
    }

    def checkPresent(sigStrings: String*) = checkWithClassloader{ cl =>
      for(sigString <- sigStrings) {
        val (cls2, methodName) = resolveMethod(sigString, cl)
        // That the previously-existing method has been removed
        assert(cls2.getMethods.exists(_.getName == methodName))
      }
    }

    def checkMangled(sigStrings: String*) = checkWithClassloader{ cl =>
      for(sigString <- sigStrings) {
        val (cls2, methodName) = resolveMethod(sigString, cl)
        // That the previously-existing method has been removed
        // And the n>=2 duplicate methods are in place (presumably being used)
        assert(cls2.getMethods.count(_.getName.startsWith(methodName + "__")) >= 2)
      }
    }

    def checkRemoved(sigStrings: String*) = checkWithClassloader{ cl =>
      for(sigString <- sigStrings){
        val (cls2, methodName) = resolveMethod(sigString, cl)
        // That the previously-existing method has been removed
        assert(!cls2.getMethods.exists(_.getName == methodName))
      }
    }
  }

  def classOf0[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass
  case class opt0[R: ClassTag]()
                              (implicit tp: TestPath) extends Optimized(classOf0[R]){

    def check(args: (Unit, R)*) = check0(args:_*)
  }
  case class opt1[T1: ClassTag, R: ClassTag]()
                                            (implicit tp: TestPath) extends Optimized(classOf0[R], classOf0[T1]){
    def check(args: (T1, R)*) = check0(args:_*)
  }
  case class opt2[T1: ClassTag, T2: ClassTag, R: ClassTag]()
                                                          (implicit tp: TestPath) extends Optimized(classOf0[R], classOf0[T1], classOf0[T2]){
    def check(args: ((T1, T2), R)*) = check0(args:_*)
  }
  case class opt3[T1: ClassTag, T2: ClassTag, T3: ClassTag, R: ClassTag]()
                                                                        (implicit tp: TestPath) extends Optimized(classOf0[R], classOf0[T1], classOf0[T2], classOf0[T3]){
    def check(args: ((T1, T2, T3), R)*) = check0(args:_*)
  }
}
