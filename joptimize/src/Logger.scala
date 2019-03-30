package joptimize
import scalatags.Text.all._
import scalatags.Text.tags2
import joptimize.model.{IType, MethodSig}

import scala.collection.mutable

class Logger(logRoot: os.Path, ignorePrefix: os.RelPath, originalSig: MethodSig, inferredArgs: Seq[IType]) {
  val destFile = logRoot / (os.rel / originalSig.cls.name.split('/') relativeTo ignorePrefix) / (Util.mangleName0(originalSig, inferredArgs) + ".html")
  val headerStyles = tags2.style("""
    |svg path:hover{
    |    stroke: red;
    |    stroke-width: 4px;
    |}
  """.stripMargin)
  os.write.over(
    destFile,
    "<html><body style=\"background-color: black; color: c7c7c7;\">" + headerStyles,
    createFolders = true
  )

  def renderLine(printed: Frag)(implicit file: sourcecode.File, line: sourcecode.Line) = {
    os.write.append(destFile, printed.render)
  }

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

    renderLine(pre(ansiToHtml((prefix ++ printed).render)))
  }

  def pprint(value0: sourcecode.Text[Any])
            (implicit f: sourcecode.File, line: sourcecode.Line)= {
    renderAnsiLine(Some(value0.source), _root_.pprint.apply(value0.value))
  }

  def html(value: Frag)
          (implicit f: sourcecode.File, line: sourcecode.Line)= {
    renderLine(value)
  }
  def apply(value0: sourcecode.Text[fansi.Str])(implicit f: sourcecode.File, line: sourcecode.Line)= {
    renderAnsiLine(Some(value0.source), value0.value)
  }
  def println(value: String)(implicit f: sourcecode.File, line: sourcecode.Line)= {
    renderAnsiLine(None, value)
  }

  // http://flatuicolors.com/
  val red = "#c91b00"
  val green = "#00c200"
  val yellow = "#c7c400"
  val blue = "#0225c7"
  val magenta = "#c930c7"
  val cyan = "#00c5c7"
  val black = "#000"
  val white = "#fff"

  val foregrounds = Map[fansi.Attr, String](
    fansi.Color.Black -> black,
    fansi.Color.Red -> red,
    fansi.Color.Green-> green,
    fansi.Color.Yellow-> yellow,
    fansi.Color.Blue -> blue,
    fansi.Color.Magenta-> magenta,
    fansi.Color.Cyan -> cyan,
    fansi.Color.White -> white
  )
  val backgrounds = Map[fansi.Attr, String](
    fansi.Back.Black -> black,
    fansi.Back.Red -> red,
    fansi.Back.Green-> green,
    fansi.Back.Yellow-> yellow,
    fansi.Back.Blue -> blue,
    fansi.Back.Magenta-> magenta,
    fansi.Back.Cyan -> cyan,
    fansi.Back.White -> white
  )
  def ansiToHtml(ansiInput: String): Frag = {
    val wrapped = mutable.Buffer.empty[scalatags.Text.Frag]
    val parsed = fansi.Str(ansiInput, errorMode = fansi.ErrorMode.Strip)
    val chars = parsed.getChars
    val colors = parsed.getColors

    var i = 0
    var previousColor = 0L
    val snippetBuffer = new mutable.StringBuilder()

    def createSnippet() = {
      val foreground = fansi.Color.lookupAttr(previousColor & fansi.Color.mask)
      val background = fansi.Back.lookupAttr(previousColor & fansi.Back.mask)
      val snippet = snippetBuffer.toString
      snippetBuffer.clear()
      wrapped.append(span(
        foregrounds.get(foreground).map(color := _),
        backgrounds.get(background).map(backgroundColor := _),
        snippet
      ))
    }

    while(i < parsed.length){
      if (colors(i) != previousColor && snippetBuffer.nonEmpty) createSnippet()
      previousColor = colors(i)
      snippetBuffer += chars(i)
      i += 1
    }
    createSnippet()
    wrapped.toVector
  }

}
