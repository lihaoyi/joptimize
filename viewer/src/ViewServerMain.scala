package joptimize.viewer
import java.io.StringReader

import scalatags.Text.all._
import scalatags.Text.tags2

import scala.collection.mutable
import joptimize.viewer.model.LogMessage
import org.xml.sax.InputSource
import org.xml.sax.helpers.{AttributesImpl, XMLFilterImpl, XMLReaderFactory}
object ViewServerMain extends cask.MainRoutes{
  val logRoot = os.pwd / 'out / 'scratch

  @cask.get("/", subpath = true)
  def view(request: cask.Request) = {
    val segments = request.remainingPathSegments
    val logPath = logRoot / segments

    html(
      head(
        link(rel:="stylesheet", href:="https://bootswatch.com/4/darkly/bootstrap.min.css"),
        tags2.style("""
          |svg path:hover{
          |    stroke: red;
          |    stroke-width: 5px;
          |}
        """.stripMargin)
      ),
      body(
        tags2.nav(aria.label := "breadcrumb")(
          ol(cls := "breadcrumb")(
            li(cls := "breadcrumb-item")(
              a(href := "/")(
                "scratch"
              )
            ),
            for(i <- segments.indices.dropRight(1)) yield {
              li(cls := "breadcrumb-item")(
                a(href := segments.take(i + 1).map("/" + _).mkString)(
                  segments(i)
                )
              )
            },
            for(i <- segments.indices.lastOption) yield {
              li(cls := "breadcrumb-item active")(
                segments(i)
              )
            }
          )
        ),
        div(cls := "container-fluid")(
          if (logPath.last.endsWith(".js")) renderEntries(logPath)
          else if (os.isDir(logPath)){
            frag(
              ul(
                for(p <- os.list(logPath))
                yield li(a(href := s"/${p.relativeTo(logRoot)}", s"${p.relativeTo(logPath)}"))
              ),
              if (os.exists(logPath / "index.js")) renderEntries(logPath / "index.js") else ()
            )
          } else ???
        )
      )
    ).render
  }

  def renderEntries(logPath: os.Path) = {
    val logEntries = os.read.lines(logPath).map(upickle.default.read[(Int, LogMessage)](_))
    pre(
      logEntries.map{
        case (indent, LogMessage.Message(str)) => div(paddingLeft := 20 * indent, ansiToHtml(str))
        case (indent, LogMessage.Graph(nodes, edges)) => div(paddingLeft := 20 * indent, renderGraph(nodes, edges))
      }
    )
  }
  def renderGraph(nodes: IndexedSeq[LogMessage.Graph.Node],
                  edges: Seq[LogMessage.Graph.Edge]): Frag = {

    import guru.nidi.graphviz.model.Factory._
    import guru.nidi.graphviz.attribute._
    import guru.nidi.graphviz.engine._

    val allGraphvizNodes = nodes.map(x => x -> node(x.text)).toMap

    val groupedEdges = edges.groupBy(_.src)
    val (liveNodes, deadNodes) = nodes.partition(_.live)
    val g = graph("Program")
      .directed()
      .graphAttr()
      .`with`(RankDir.TOP_TO_BOTTOM)
      .`with`(
        groupedEdges.toSeq.map{ case (src, edges) =>
          allGraphvizNodes(nodes(src)).link(
            edges.map{edge =>
              to(allGraphvizNodes(nodes(edge.dest))).`with`(
                (if (edge.dashed) Style.DASHED else Style.SOLID)
                .and(Style.lineWidth(if (edge.thick) 3 else 1)),
                if (edge.forwardArrow) Arrow.INV.tail().dir(Arrow.DirType.FORWARD)
                else Arrow.INV.tail().dir(Arrow.DirType.BACK)
              )
            }:_*
          )
        }:_*
      )
      .`with`(
        deadNodes.map { n =>
          allGraphvizNodes(n).`with`(
            n.color match {
              case "blue" => Color.BLUE
              case "cyan" => Color.CYAN
              case "magenta" => Color.MAGENTA
              case "red" => Color.RED
            },
            Style.FILLED,
            Font.name("courier"),
            Shape.RECTANGLE
          )
        }
      :_*)
      .`with`(graph.cluster().named("Live").`with`(
        liveNodes.map { n =>
          allGraphvizNodes(n).`with`(
            n.color match {
              case "blue" => Color.BLUE
              case "cyan" => Color.CYAN
              case "magenta" => Color.MAGENTA
              case "red" => Color.RED
            },
            Style.FILLED,
            Font.name("courier"),
            Shape.RECTANGLE
          )
        }
      :_*))


    val dotString = Graphviz.fromGraph(g).render(Format.SVG).toString

    import javax.xml.transform.OutputKeys
    import javax.xml.transform.TransformerFactory
    import javax.xml.transform.sax.SAXSource
    import javax.xml.transform.stream.StreamResult
    import java.io.StringWriter

    val stringWriter = new StringWriter
    val transformer = TransformerFactory.newInstance.newTransformer
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    transformer.transform(
      new SAXSource(
        new XMLFilterImpl(XMLReaderFactory.createXMLReader()) {
          override def startElement(uri: String, localName: String, qName: String,
                                    attrs: org.xml.sax.Attributes): Unit = {
            val newAttributes =
              if (localName != "svg") attrs
              else {
                val newAttrs = new AttributesImpl(attrs)
                newAttrs.setValue(
                  newAttrs.getIndex("width"),
                  newAttrs.getValue("width").stripSuffix("pt")
                )
                newAttrs.setValue(
                  newAttrs.getIndex("height"),
                  newAttrs.getValue("height").stripSuffix("pt")
                )
                newAttrs
              }
            super.startElement(uri, localName, qName, newAttributes)

          }
        },
        new InputSource(new StringReader(dotString))
      ),
      new StreamResult(stringWriter)
    )

    scalatags.Text.all.pre(scalatags.Text.all.raw(stringWriter.toString))
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


  def ansiToHtml(parsed: fansi.Str): Frag = {
    val wrapped = mutable.Buffer.empty[scalatags.Text.Frag]
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


  initialize()
}