package joptimize
import fansi.Str
import joptimize.model.{IType, InferredSig, MethodSig}
import joptimize.viewer.model.LogMessage
import sourcecode.{File, Line, Text}


trait Logger{
  def pprint(value0: => sourcecode.Text[Any])
            (implicit f: sourcecode.File, line: sourcecode.Line): Unit

  def graph(g: => LogMessage.Graph)
           (implicit f: sourcecode.File, line: sourcecode.Line): Unit
  def apply(value0: => sourcecode.Text[fansi.Str])(implicit f: sourcecode.File, line: sourcecode.Line): Unit
  def println(value: => String)(implicit f: sourcecode.File, line: sourcecode.Line): Unit

  def method(originalSig: MethodSig): Logger.Method
  def check(action: => Unit): Unit
  def global(): Logger.Global
  def inferredMethod(isig: InferredSig): Logger.InferredMethod
}

object Logger{
  trait Global extends Logger
  trait Method extends Logger
  trait InferredMethod extends Logger
}

object DummyLogger extends Logger with Logger.Global with Logger.Method with Logger.InferredMethod {
  def pprint(value0: => Text[Any])(implicit f: File, line: Line): Unit = ()

  def graph(g: => LogMessage.Graph)(implicit f: File, line: Line): Unit = ()

  def apply(value0: => Text[Str])(implicit f: File, line: Line): Unit = ()

  def println(value: => String)(implicit f: File, line: Line): Unit = ()

  def method(originalSig: MethodSig) = this

  def check(action: => Unit) = ()

  def global() = this

  def inferredMethod(isig: InferredSig) = this
}

abstract class FileLogger(logRoot: os.Path, ignorePrefix: os.RelPath, segments: Seq[String], name: String) extends Logger {
  val destFile = logRoot / (os.rel / segments relativeTo ignorePrefix) / (name + ".js")
  if (!os.exists(destFile))
  os.write.over(destFile, "", createFolders = true)

  def renderAnsiLine(labelOpt: Option[String],
                     printed: fansi.Str)
                    (implicit file: sourcecode.File, line: sourcecode.Line) = {
    val prefix =
      computePrefix(labelOpt, file, line)

    os.write.append(
      destFile,
      Seq(upickle.default.write(LogMessage.Message(prefix ++ printed)), "\n")
    )
  }

  def computePrefix(labelOpt: Option[String], file: File, line: Line) = {
    fansi.Color.Green(file.value.split('/').last) ++
      fansi.Str(":") ++
      fansi.Color.Green(line.value.toString) ++
      labelOpt.fold(fansi.Str(""))(label =>
        fansi.Str(" ") ++
          fansi.Color.Cyan(label)
      ) ++
      fansi.Str(": ")
  }

  def pprint(value0: => sourcecode.Text[Any])
            (implicit f: sourcecode.File, line: sourcecode.Line) = {
    renderAnsiLine(Some(value0.source), _root_.pprint.apply(value0.value, height=99999))
  }

  def graph(g: => LogMessage.Graph)
           (implicit f: sourcecode.File, line: sourcecode.Line) = {
    os.write.append(
      destFile,
      Seq(
        upickle.default.write(LogMessage.Message(computePrefix(None, f, line))), "\n",
        upickle.default.write(g), "\n"
      )
    )
  }

  def check(action: => Unit) = action

  def apply(value0: => sourcecode.Text[fansi.Str])(implicit f: sourcecode.File, line: sourcecode.Line) = {
    renderAnsiLine(Some(value0.source), value0.value)
  }
  def println(value: => String)(implicit f: sourcecode.File, line: sourcecode.Line) = {
    renderAnsiLine(None, value)
  }

  def method(originalSig: MethodSig) = new FileLogger.Method(logRoot, ignorePrefix, originalSig)
  def global() = new FileLogger.Global(logRoot, ignorePrefix)
  def inferredMethod(isig: InferredSig) = new FileLogger.InferredMethod(logRoot, ignorePrefix, isig.method, isig.inferred)
}

object FileLogger{
  class Global(logRoot: os.Path, ignorePrefix: os.RelPath)
    extends FileLogger(
      logRoot,
      ignorePrefix,
      ignorePrefix.segments,
      "global"
    ) with Logger.Global

  class Method(logRoot: os.Path, ignorePrefix: os.RelPath, originalSig: MethodSig)
    extends FileLogger(
      logRoot,
      ignorePrefix,
      originalSig.cls.name.split('/') :+ Util.mangleName0(originalSig, originalSig.desc.args),
      "original"
    ) with Logger.Method

  class InferredMethod(logRoot: os.Path, ignorePrefix: os.RelPath, originalSig: MethodSig, inferredArgs: Seq[IType])
    extends FileLogger(
      logRoot,
      ignorePrefix,
      originalSig.cls.name.split('/') :+ Util.mangleName0(originalSig, originalSig.desc.args),
      Util.mangleArgs(inferredArgs)
    ) with Logger.InferredMethod
}