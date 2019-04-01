package joptimize.viewer.model
import upickle.default.{ReadWriter, macroRW, readwriter}
sealed trait LogMessage
object LogMessage{

  case class Message(value: fansi.Str) extends LogMessage
  implicit val strRW: ReadWriter[fansi.Str] = readwriter[String].bimap(
    str => str.render,
    fansi.Str(_)
  )
  implicit val messageRW: ReadWriter[Message] = macroRW
  case class Graph(nodes: IndexedSeq[Graph.Node], edges: Seq[Graph.Edge]) extends LogMessage
  implicit val graphRW: ReadWriter[Graph] = macroRW
  implicit val logMessageRw: ReadWriter[LogMessage] = macroRW
  object Graph{
    case class Node(text: String, color: String, live: Boolean)
    implicit val nodeRW: ReadWriter[Node] = macroRW
    case class Edge(src: Int, dest: Int, forwardArrow: Boolean, dashed: Boolean, thick: Boolean)
    implicit val edgeRW: ReadWriter[Edge] = macroRW
  }
}