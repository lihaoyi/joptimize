package joptimize
import joptimize.model.{IType, MethodSig}
import joptimize.viewer.model.LogMessage

import scala.collection.mutable

abstract class Logger(logRoot: os.Path, ignorePrefix: os.RelPath, segments: Seq[String], name: String) {
  val destFile = logRoot / (os.rel / segments relativeTo ignorePrefix) / (name + ".js")
  if (!os.exists(destFile))
  os.write.over(destFile, "", createFolders = true)

  def renderAnsiLine(labelOpt: Option[String],
                     printed: fansi.Str)(implicit file: sourcecode.File, line: sourcecode.Line) = {
    val prefix =
      fansi.Color.Green(file.value.split('/').last) ++
        fansi.Str(":") ++
        fansi.Color.Green(line.value.toString) ++
        labelOpt.fold(fansi.Str(""))(label =>
          fansi.Str(" ") ++
            fansi.Color.Cyan(label)
        ) ++
        fansi.Str(": ")

    os.write.append(
      destFile,
      Seq(upickle.default.write(LogMessage.Message(prefix ++ printed)), "\n")
    )
  }

  def pprint(value0: sourcecode.Text[Any])
            (implicit f: sourcecode.File, line: sourcecode.Line)= {
    renderAnsiLine(Some(value0.source), _root_.pprint.apply(value0.value, height=99999))
  }

  def graph(g: LogMessage.Graph)
           (implicit f: sourcecode.File, line: sourcecode.Line)= {
    os.write.append(
      destFile,
      Seq(upickle.default.write(g), "\n")
    )
  }
  def apply(value0: sourcecode.Text[fansi.Str])(implicit f: sourcecode.File, line: sourcecode.Line)= {
    renderAnsiLine(Some(value0.source), value0.value)
  }
  def println(value: String)(implicit f: sourcecode.File, line: sourcecode.Line)= {
    renderAnsiLine(None, value)
  }

  def method(originalSig: MethodSig) = new Logger.Method(logRoot, ignorePrefix, originalSig)
  def global() = new Logger.Global(logRoot, ignorePrefix)
  def inferredMethod(originalSig: MethodSig, inferredArgs: Seq[IType]) = new Logger.InferredMethod(logRoot, ignorePrefix, originalSig, inferredArgs)
}

object Logger{
  class Global(logRoot: os.Path, ignorePrefix: os.RelPath)
    extends Logger(
      logRoot,
      ignorePrefix,
      ignorePrefix.segments,
      "global"
    )
  class Method(logRoot: os.Path, ignorePrefix: os.RelPath, originalSig: MethodSig)
    extends Logger(
      logRoot,
      ignorePrefix,
      originalSig.cls.name.split('/') :+ Util.mangleName0(originalSig, originalSig.desc.args),
      "original"
    )

  class InferredMethod(logRoot: os.Path, ignorePrefix: os.RelPath, originalSig: MethodSig, inferredArgs: Seq[IType])
    extends Logger(
      logRoot,
      ignorePrefix,
      originalSig.cls.name.split('/') :+ Util.mangleName0(originalSig, originalSig.desc.args),
      Util.mangleArgs(inferredArgs)
    )
}