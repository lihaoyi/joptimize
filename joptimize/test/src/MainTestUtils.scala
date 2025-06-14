package test
import java.io.ByteArrayOutputStream
import java.net.URLClassLoader
import java.util.zip.ZipFile

import joptimize.model.{Desc, JType, MethodSig}
import org.objectweb.asm.tree.{ClassNode, IntInsnNode, LdcInsnNode}
import org.objectweb.asm.{ClassReader, Opcodes}
import utest._
import utest.framework.TestPath

import scala.collection.mutable

object MainTestUtils {

  val classesRoot = os.Path(sys.env("CLASSES_FOLDER"), os.pwd)
  val outRoot = os.Path("out/scratch", os.pwd)

  def annotatedTest(implicit tp: TestPath) = {


    val rawCls = Class.forName(s"test.${tp.value.dropRight(1).mkString(".")}")
    val rawMethod = rawCls.getDeclaredMethods.find(_.getName == tp.value.last).get
    val methodDesc = Desc(
      rawMethod.getParameterTypes.map(c => JType.fromJavaCls(c)),
      JType.fromJavaCls(rawMethod.getReturnType)
    )
    val testAnnot = rawMethod.getAnnotation(classOf[Test])

    val cases = testAnnot.inputs() match {
      case Array() => Iterator(Array())
      case multiple => multiple.grouped(rawMethod.getParameterCount)
    }

    def loadIfExists(folderEnv: String, name: String): Option[Array[Byte]] = {
      val p = os.Path(sys.env(folderEnv)) / os.RelPath(name)
      if (os.exists(p)) Some(os.read.bytes(p))
      else None
    }
    val output = mutable.Buffer.empty[ujson.Value]
    for (inline <- Seq(false, true)){
      println(s"  test.MainTests.${tp.value.mkString(".")} inline: $inline")
      os.remove.all(os.pwd / 'out / 'scratch)
      os.remove.all(outRoot / tp.value)
      val outputFileMap = joptimize.JOptimize.run(
        name => loadIfExists("CLASSES_FOLDER", name),
        Seq(MethodSig(s"test/${tp.value.dropRight(1).mkString("/")}", tp.value.last, methodDesc, static = true)),
        log = joptimize.DummyLogger,
//        log = new joptimize.FileLogger.Global(logRoot = outRoot),
        inline = inline
      )

      for ((k, bytes) <- outputFileMap) {
        os.write(outRoot / os.RelPath(k), bytes, createFolders = true)
        //      val subPath = os.RelPath(k).relativeTo(ignorePrefix)
        //      assert(subPath.ups == 0)
        //      os.write(outRoot / tp.value / subPath, bytes, createFolders = true)
      }

      for(args <- cases){
        val boxedArgs =
          for ((a, p) <- args.zip(rawMethod.getParameterTypes))
            yield {
              if (p == classOf[Int]) Int.box(a)
              else if (p == classOf[Char]) Char.box(a.toChar)
              else if (p == classOf[Byte]) Byte.box(a.toByte)
              else if (p == classOf[Boolean]) Boolean.box(a != 0)
              else if (p == classOf[Short]) Short.box(a.toShort)
              else if (p == classOf[Long]) Long.box(a.toLong)
              else if (p == classOf[Float]) Float.box(a.toFloat)
              else if (p == classOf[Double]) Double.box(a.toDouble)
              else if (p == classOf[java.lang.String]) a.toString
              else throw new Exception(p.toString)
            }

        val expectedResult = withClassloader(classesRoot) { cl =>
          val cls = cl.loadClass(s"test.${tp.value.dropRight(1).mkString(".")}")
          val method = cls.getDeclaredMethod(tp.value.last, rawMethod.getParameterTypes: _*)
          method.setAccessible(true)
          method.invoke(null, boxedArgs: _*)
        }
        val joptimizedResult = withClassloader(outRoot) { cl =>
          val cls = cl.loadClass(s"test.${tp.value.dropRight(1).mkString(".")}")
          val method = cls.getDeclaredMethod(tp.value.last, rawMethod.getParameterTypes: _*)
          method.setAccessible(true)
          method.invoke(null, boxedArgs: _*)
        }
        output.append(
          ujson.Arr(
            ujson.Arr(boxedArgs.map(sketchyToJson(_)): _*),
            sketchyToJson(joptimizedResult)
          )
        )

        // Make sure the correct value is computed
        (joptimizedResult, expectedResult) match {
          case (a: Array[AnyRef], b: Array[AnyRef]) => assert(java.util.Arrays.deepEquals(a, b))
          case (a: Array[_], b: Array[_]) =>
            val lhs = a.toSeq
            val rhs = b.toSeq
            assert(lhs == rhs)

          case _ =>
            val argList = args.toList
            assert {
              identity(argList)
              joptimizedResult == expectedResult
            }
        }
      }
      if (testAnnot.assertOnInlined() == inline) {
        withClassloader(outRoot) { cl =>
          testAnnot.addedNumConst().foreach(checkAddedNumConst(cl, _))
          testAnnot.removedNumConst().foreach(checkRemovedNumConst(cl, _))
          testAnnot.checkPresent().foreach(checkPresent(cl, _))
          testAnnot.checkRemoved().foreach(checkRemoved(cl, _))
          testAnnot.checkMangled().foreach(checkMangled(cl, _))
          testAnnot.checkNotMangled().foreach(checkNotMangled(cl, _))
          testAnnot.checkClassRemoved().foreach(checkClassRemoved(cl, _))
        }
      }
    }



    ujson.Arr.from(output)
  }



  /**
    * We convert the argument/return values of each test case into JSON just to
    * give us a convenient, concise format for printing them
    */
  def sketchyToJson(x: AnyRef): ujson.Value = x match{
    case n: java.lang.Integer => ujson.Num(n.toDouble)
    case n: java.lang.Byte => ujson.Num(n.toDouble)
    case n: java.lang.Double => ujson.Num(n.toDouble)
    case n: java.lang.Short => ujson.Num(n.toShort)
    case n: java.lang.Long => ujson.Num(n.toLong)
    case n: java.lang.Float => ujson.Num(n.toDouble)
    case n: java.lang.String => ujson.Str(n)
    case n: java.lang.Character => ujson.Str(n.toString)
    case n: java.lang.Boolean => ujson.Bool(n)
    case n: Array[_] => ujson.Arr(n.map(c => sketchyToJson(c.asInstanceOf[AnyRef])):_*)
  }

  def withClassloader[T](clRoot: os.Path)(f: URLClassLoader => T)(implicit tp: TestPath): T = {
    val cl = new URLClassLoader(
      Array(clRoot.toNIO.toUri.toURL),
      ClassLoader.getSystemClassLoader().getParent()
    )
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
      s"test.${tp.value.dropRight(2).mkString(".")}.$clsName"
    )
    (cls2, methodName)
  }

  def checkAddedNumConst(cl: ClassLoader, const: Int)(implicit tp: TestPath) = {
    val constants = checkNumConst0(cl)
    assert(constants.contains(const))
  }

  def checkRemovedNumConst(cl: ClassLoader, const: Int)(implicit tp: TestPath) = {
    val constants = checkNumConst0(cl)
    assert(!constants.contains(const))
  }

  def checkNumConst0(cl: ClassLoader)(implicit tp: TestPath): collection.Seq[Int] = {

    val clsName = tp.value(tp.value.length - 2)
    val bytestream = os.read.inputStream(
      os.resource(cl) / "test" / tp.value.dropRight(2) / (clsName + ".class")
    )
    val cr = new ClassReader(bytestream)
    val cn = new ClassNode()
    cr.accept(cn, ClassReader.SKIP_FRAMES)
    import collection.JavaConverters._
    val allInsns = cn.methods.asScala.flatMap(_.instructions.iterator().asScala)
    allInsns.map(x => (x.getOpcode, x)).flatMap{
      case (Opcodes.ICONST_0, _) => Some(0)
      case (Opcodes.ICONST_1, _) => Some(1)
      case (Opcodes.ICONST_2, _) => Some(2)
      case (Opcodes.ICONST_3, _) => Some(3)
      case (Opcodes.ICONST_4, _) => Some(4)
      case (Opcodes.ICONST_5, _) => Some(5)
      case (Opcodes.ICONST_M1, _) => Some(-1)

      case (Opcodes.LCONST_0, _) => Some(0)
      case (Opcodes.LCONST_1, _) => Some(1)

      case (Opcodes.FCONST_0, _) => Some(0)
      case (Opcodes.FCONST_1, _) => Some(1)
      case (Opcodes.FCONST_2, _) => Some(2)

      case (Opcodes.DCONST_0, _) => Some(0)
      case (Opcodes.DCONST_1, _) => Some(1)

      case (Opcodes.BIPUSH, i: IntInsnNode) => Some(i.operand)
      case (Opcodes.SIPUSH, i: IntInsnNode) => Some(i.operand)
      case (Opcodes.LDC, i: LdcInsnNode) =>
        i.cst match{
          case n: java.lang.Integer => Some(n.toInt)
          case n: java.lang.Long => Some(n.toInt)
          case n: java.lang.Float => Some(n.toInt)
          case n: java.lang.Double => Some(n.toInt)
          case _ => None
        }
      case (k, i) => None
    }

  }
  def checkPresent(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    val (cls2, methodName) = resolveMethod(sigString, cl)
    // That the previously-existing method has been removed
    assert(cls2.getDeclaredMethods.exists(_.getName == methodName))
  }

  def mangledMethodCount(cls2: Class[_], methodName: String) = cls2.getDeclaredMethods.count(_.getName.startsWith(methodName + "__"))

  def checkMangled(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    val (cls2, methodName) = resolveMethod(sigString, cl)
    // That the previously-existing method has been removed
    // And the n>=2 duplicate methods are in place (presumably being used)
    assert(mangledMethodCount(cls2, methodName) >= 2)
  }

  def checkNotMangled(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    val (cls2, methodName) = resolveMethod(sigString, cl)
    // That the previously-existing method has been removed
    // And the n>=2 duplicate methods are in place (presumably being used)
    assert(mangledMethodCount(cls2, methodName) == 0)
  }

  def checkRemoved(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    val (cls2, methodName) = resolveMethod(sigString, cl)
    // That the previously-existing method has been removed
    assert(!cls2.getDeclaredMethods.exists(_.getName == methodName))
  }

  def checkClassRemoved(cl: ClassLoader, sigString: String)(implicit tp: TestPath) = {
    intercept[ClassNotFoundException]{
      cl.loadClass(
        s"test.${tp.value.dropRight(2).mkString(".")}.$sigString"
      )
    }
  }
}
