package joptimize
import fansi.Str
import joptimize.model.{IType, InferredSig, MethodSig}
import joptimize.viewer.model.LogMessage
import sourcecode.{File, Line, Text}

trait Logger {
  def pprint(
    value0: => sourcecode.Text[Any]
  )(implicit f: sourcecode.File, line: sourcecode.Line): Unit

  def graph(label: String)(
    g: => LogMessage.Graph
  )(implicit f: sourcecode.File, line: sourcecode.Line): Unit
  def apply(
    value0: => sourcecode.Text[fansi.Str]
  )(implicit f: sourcecode.File, line: sourcecode.Line): Unit
  def println(value: => String)(implicit f: sourcecode.File, line: sourcecode.Line): Unit

  def method(originalSig: MethodSig): Logger.Method
  def check(action: => Unit): Unit
  def global(): Logger.Global
  def inferredMethod(isig: InferredSig): Logger.InferredMethod

  def block[T](t: => T)(implicit e: sourcecode.Enclosing, pkg: sourcecode.Pkg): T
}

object Logger {
  trait Global extends Logger
  trait Method extends Logger
  trait InferredMethod extends Logger
}

object DummyLogger extends Logger with Logger.Global with Logger.Method with Logger.InferredMethod {
  def pprint(value0: => Text[Any])(implicit f: File, line: Line): Unit = ()

  def graph(label: String)(g: => LogMessage.Graph)(implicit f: File, line: Line): Unit = ()

  def apply(value0: => Text[Str])(implicit f: File, line: Line): Unit = ()

  def println(value: => String)(implicit f: File, line: Line): Unit = ()

  def method(originalSig: MethodSig) = this

  def check(action: => Unit) = ()

  def global() = this

  def inferredMethod(isig: InferredSig) = this

  def block[T](t: => T)(implicit e: sourcecode.Enclosing, pkg: sourcecode.Pkg): T = t
}

abstract class FileLogger(logRoot: os.Path, segments: Seq[String], name: String) extends Logger {
  val destFile = logRoot / segments / (name + ".js")
  if (!os.exists(destFile))
    os.write.over(destFile, "", createFolders = true)

  var indent = 0
  def renderAnsiLine(
    labelOpt: Option[String],
    printed: fansi.Str
  )(implicit file: sourcecode.File, line: sourcecode.Line) = {
    val prefix =
      computePrefix(labelOpt, file, line)

    os.write
      .append(
        destFile,
        Seq(upickle.default.write((indent, LogMessage.Message(prefix ++ printed))), "\n"),
        createFolders = true
      )
  }

  def computePrefix(labelOpt: Option[String], file: File, line: Line) = {
    fansi.Color.Green(file.value.split('/').last) ++
    fansi.Str(":") ++
    fansi.Color.Green(line.value.toString) ++
    labelOpt.fold(fansi.Str(""))(
      label =>
        fansi.Str(" ") ++
        fansi.Color.Cyan(label)
    ) ++
    fansi.Str(": ")
  }

  def pprint(
    value0: => sourcecode.Text[Any]
  )(implicit f: sourcecode.File, line: sourcecode.Line) = {
    renderAnsiLine(Some(value0.source), _root_.pprint.apply(value0.value, height = 99999))
  }

  def graph(
    label: String
  )(g: => LogMessage.Graph)(implicit f: sourcecode.File, line: sourcecode.Line) = {
    os.write
      .append(
        destFile,
        Seq(
          upickle
            .default
            .write((indent, LogMessage.Message(computePrefix(None, f, line) ++ label))),
          "\n",
          upickle.default.write((indent, g)),
          "\n"
        ),
        createFolders = true
      )
  }

  def check(action: => Unit) = action

  def apply(
    value0: => sourcecode.Text[fansi.Str]
  )(implicit f: sourcecode.File, line: sourcecode.Line) = {
    renderAnsiLine(Some(value0.source), value0.value)
  }
  def println(value: => String)(implicit f: sourcecode.File, line: sourcecode.Line) = {
    renderAnsiLine(None, value)
  }

  def method(originalSig: MethodSig) = new FileLogger.Method(logRoot, originalSig)
  def global() = new FileLogger.Global(logRoot)
  def inferredMethod(isig: InferredSig) =
    new FileLogger.InferredMethod(logRoot, isig.method, isig.inferred)
  def block[T](t: => T)(implicit e: sourcecode.Enclosing, pkg: sourcecode.Pkg): T = {
    println(e.value.drop(pkg.value.length + 1))
    indent += 1
    try t
    finally indent -= 1
  }
}

object FileLogger {
  class Global(logRoot: os.Path)
      extends FileLogger(
        logRoot,
        Nil,
        "index"
      )
      with Logger.Global

  class Method(logRoot: os.Path, originalSig: MethodSig)
      extends FileLogger(
        logRoot,
        originalSig.cls.name.split('/') :+ Util.mangleName0(originalSig, originalSig.desc.args),
        "index"
      )
      with Logger.Method

  class InferredMethod(logRoot: os.Path, originalSig: MethodSig, inferredArgs: Seq[IType])
      extends FileLogger(
        logRoot,
        originalSig.cls.name.split('/') :+ Util.mangleName0(originalSig, originalSig.desc.args),
        Util.mangleArgs(inferredArgs)
      )
      with Logger.InferredMethod
}
