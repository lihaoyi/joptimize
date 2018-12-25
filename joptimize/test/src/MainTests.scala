package joptimize
import java.net.URLClassLoader

import utest._
import utest.framework.TestPath

import scala.collection.mutable
import scala.reflect.ClassTag

object MainTests extends TestSuite{
  def tests = Tests{
    'simple - {
      'IfElse - {
        'basicIf - annotatedTest
        'ifNonIntZero - annotatedTest
        'ifNonIntBinary - annotatedTest
        'ifElseIf - annotatedTest
        'ifElseIfBig - annotatedTest
      }
      'Loops - {
        'basicFor - annotatedTest
        'basicWhile - annotatedTest
        'sqrtFinder - annotatedTest

      }

      'Switches - {
        'smallSwitch - annotatedTest
        'bigDenseSwitch - annotatedTest
        'bigSparseSwitch - annotatedTest
        'charSwitch - annotatedTest
        'byteSwitch - annotatedTest
      }
      'Statics - {
        'helloWorld - annotatedTest
        'timesTwo - annotatedTest
        'add - annotatedTest
        'helloWorld2 - annotatedTest
        'timesTwo2 - annotatedTest
        'tailFactorial - annotatedTest
        'fibonacci - annotatedTest
        'call - annotatedTest
        'callAtPhiBoundary - annotatedTest
      }
      'MultiDimArrays - {
        'make2D - annotatedTest
        'make3D - annotatedTest
        'getAndSet - annotatedTest
      }
      'InvokeDynamic - {
        'lambda - annotatedTest
        'lambdaArg - annotatedTest
        'lambdaBoxed - annotatedTest
        'lambdaBoxedArgs - annotatedTest
        'concat - annotatedTest
      }
      'ScalaInvokeDynamic - {
        'lambda - annotatedTest
        'lambdaBoxed - annotatedTest
        'concat - annotatedTest
      }
      'Inheritance - {
        'implement - annotatedTest
        'abstractClass - annotatedTest
        'shadowedInheritedGet - annotatedTest
        'shadowedInheritedSet - annotatedTest
        'superMethod - annotatedTest
        'staticInheritance - annotatedTest
        'staticInheritanceMethod - annotatedTest
      }
      'Exceptions - {
        'throwCatch - annotatedTest
        'multiCatch - annotatedTest
        'nullPointer - annotatedTest
        'arrayIndexOutOfBounds - annotatedTest
      }
    }
    'narrow - {
      'Supertype - {
        'main - annotatedTest
        'mainDeep - annotatedTest
      }
      'Parametric - {
        'main - annotatedTest
        'mainDeep - annotatedTest
      }
      'NarrowReturn - {
        'main - annotatedTest
      }
      'MergeReturn - {
        'main - annotatedTest
      }
      'IntersectionReturn - {
        'main - annotatedTest
      }
      'ForceWide - {
        'main - annotatedTest
      }
    }
    'opt - {
      'SimpleDce - {
        'main - annotatedTest
      }
      'BooleanJumpFlatten - {
        'simpleTrue - annotatedTest
        'simpleFalse - annotatedTest
      }
      'InstanceofJumpFlatten - {
        'simpleBar - annotatedTest
        'simpleBaz - annotatedTest
        'simpleQux - annotatedTest
        'simpleBarMatch - annotatedTest
        'simpleBazMatch - annotatedTest
        'simpleQuxMatch - annotatedTest
      }
      'BooleanWidening - {
        'simple - annotatedTest
      }
      'InstanceDce - {
        'simple1 - annotatedTest
        'simple2 - annotatedTest
        'simple3 - annotatedTest

        'single1 - annotatedTest
        'single2 - annotatedTest
        'single3 - annotatedTest
        'single4 - annotatedTest

        'unknown1 - annotatedTest
        'unknown2 - annotatedTest
        'unknown3 - annotatedTest
      }
      'InterfacePreservation - {
        'shallow - annotatedTest
        'deep - annotatedTest
      }
      'ConstantMethod - {
        'intMain - annotatedTest
        'boolMain - annotatedTest
        'impureMain - annotatedTest
      }

      'Liveness - {
        'simple - annotatedTest
        'simple2a - annotatedTest
        'simple2b - annotatedTest

        'chained - annotatedTest
        'chained2a - annotatedTest
        'chained2b - annotatedTest
      }
    }
  }

  val classesRoot = os.Path("out/joptimize/test/compile/dest/classes", os.pwd)
  val outRoot = os.Path("out/scratch", os.pwd)
  val originalRoot = os.Path("out/original", os.pwd)
  val testRoot = classesRoot / "joptimize" / "examples"

  def annotatedTest(implicit tp: TestPath) = {
    val rawCls = Class.forName(s"joptimize.examples.${tp.value.dropRight(1).mkString(".")}")
    val rawMethod = rawCls.getDeclaredMethods.find(_.getName == tp.value.last).get
    val methodDesc = Desc(
      rawMethod.getParameterTypes.map(c => JType.fromJavaCls(c)),
      JType.fromJavaCls(rawMethod.getReturnType)
    )

    val inputFileMap = os.walk(testRoot / tp.value.dropRight(2))
      .filter(os.isFile)
      .map(p => (p.relativeTo(classesRoot).toString, os.read.bytes(p)))
      .toMap

    os.remove.all(originalRoot / tp.value)
    for((k, bytes) <- inputFileMap){
      os.write(originalRoot / tp.value / os.RelPath(k), bytes, createFolders = true)
    }

    val outputFileMap = JOptimize.run(
      inputFileMap,
      Seq(MethodSig(s"joptimize/examples/${tp.value.dropRight(1).mkString("/")}", tp.value.last, methodDesc, static = true)),
      eliminateOldMethods = true
    )

    val Seq(inputBytes, outputBytes) =
      Seq(inputFileMap, outputFileMap).map(_.values.map(_.length).sum)

    os.remove.all(outRoot / tp.value)
    for((k, bytes) <- outputFileMap){
      os.write(outRoot / tp.value / os.RelPath(k), bytes, createFolders = true)
    }

    val testAnnot = rawMethod.getAnnotation(classOf[joptimize.Test])
    val cases = testAnnot.inputs() match{
      case Array() => Iterator(Array())
      case multiple => multiple.grouped(rawMethod.getParameterCount)
    }

    val output = mutable.Buffer.empty[ujson.Value]
    for (args <- cases) checkWithClassloader{ cl =>
      val cls = cl.loadClass(s"joptimize.examples.${tp.value.dropRight(1).mkString(".")}")
      val method = cls.getDeclaredMethod(tp.value.last, rawMethod.getParameterTypes: _*)
      method.setAccessible(true)
      rawMethod.setAccessible(true)

      val boxedArgs =
        for((a, p) <- args.zip(rawMethod.getParameterTypes))
        yield {
          if (p == classOf[Int]) Int.box(a)
          else if (p == classOf[Char]) Char.box(a.toChar)
          else if (p == classOf[Byte]) Byte.box(a.toByte)
          else if (p == classOf[Boolean]) Boolean.box(a != 0)
          else throw new Exception(p.toString)
        }

      val expected = rawMethod.invoke(null, boxedArgs: _*)
      val res = method.invoke(null, boxedArgs: _*)

      output.append(
        ujson.Arr(
          ujson.Arr(boxedArgs.map(sketchyToJson(_)):_*),
          sketchyToJson(res)
        )
      )
      // Make sure the correct value is computed
      (res, expected) match {
        case (a: Array[AnyRef], b: Array[AnyRef]) =>
          assert(java.util.Arrays.deepEquals(a, b))
        case _ => assert {
          res == expected
        }
      }
    }
    checkWithClassloader { cl =>
      testAnnot.checkPresent().foreach(checkPresent(cl, _))
      testAnnot.checkRemoved().foreach(checkRemoved(cl, _))
      testAnnot.checkMangled().foreach(checkMangled(cl, _))
      testAnnot.checkNotMangled().foreach(checkNotMangled(cl, _))
      testAnnot.checkClassRemoved().foreach(checkClassRemoved(cl, _))
    }

    ujson.Arr(output:_*)
  }

  /**
    * We convert the argument/return values of each test case into JSON just to
    * give us a convenient, concise format for printing them
    */
  def sketchyToJson(x: AnyRef): ujson.Value = x match{
    case n: java.lang.Integer => ujson.Num(n.toDouble)
    case n: java.lang.Byte => ujson.Num(n.toDouble)
    case n: java.lang.Double => ujson.Num(n.toDouble)
    case n: java.lang.Float => ujson.Num(n.toDouble)
    case n: java.lang.String => ujson.Str(n)
    case n: java.lang.Character => ujson.Str(n.toString)
    case n: java.lang.Boolean => ujson.Bool(n)
    case n: Array[_] => ujson.Arr(n.map(c => sketchyToJson(c.asInstanceOf[AnyRef])):_*)
  }

  def checkWithClassloader(f: URLClassLoader => Any)(implicit tp: TestPath) = {
    val cl = new URLClassLoader(Array((outRoot / tp.value).toNIO.toUri.toURL))
    try f(cl)
    finally cl.close()
  }

  def resolveMethod(sigString: String, cl: ClassLoader)(implicit tp: TestPath) = {

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

  def checkPresent(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    val (cls2, methodName) = resolveMethod(sigString, cl)
    // That the previously-existing method has been removed
    assert(cls2.getDeclaredMethods.exists(_.getName == methodName))
  }

  def checkMangled(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    val (cls2, methodName) = resolveMethod(sigString, cl)
    // That the previously-existing method has been removed
    // And the n>=2 duplicate methods are in place (presumably being used)
    assert(cls2.getDeclaredMethods.count(_.getName.startsWith(methodName + "__")) >= 2)
  }

  def checkNotMangled(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    val (cls2, methodName) = resolveMethod(sigString, cl)
    // That the previously-existing method has been removed
    // And the n>=2 duplicate methods are in place (presumably being used)
    assert(cls2.getDeclaredMethods.count(_.getName.startsWith(methodName + "__")) == 0)
  }

  def checkRemoved(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    val (cls2, methodName) = resolveMethod(sigString, cl)
    // That the previously-existing method has been removed
    assert(!cls2.getDeclaredMethods.exists(_.getName == methodName))
  }

  def checkClassRemoved(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    intercept[ClassNotFoundException]{
      cl.loadClass(
        s"joptimize.examples.${tp.value.dropRight(2).mkString(".")}.$sigString"
      )
    }
  }
}
