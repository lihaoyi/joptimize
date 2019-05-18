package joptimize.analyzer
import java.io.{PrintWriter, StringReader, StringWriter}

import fansi.Str
import joptimize.Util
import joptimize.graph.HavlakLoopTree
import joptimize.model.{Desc, JType, MethodBody, SSA}
import joptimize.viewer.model.LogMessage
import org.objectweb.asm.tree.{AbstractInsnNode, InsnList}
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}
import org.xml.sax.InputSource
import org.xml.sax.helpers.{AttributesImpl, XMLFilterImpl, XMLReaderFactory}
import pprint.Tree

import collection.mutable
object Renderer {


  def prettyprint(insnNode: AbstractInsnNode) = {
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)
    prettyprint0(insnNode, printer, methodPrinter)
  }
  def prettyprint0(insnNode: AbstractInsnNode,
                   printer: Textifier,
                   methodPrinter: TraceMethodVisitor) = {
    insnNode.accept(methodPrinter)
    val sw = new StringWriter
    printer.print(new PrintWriter(sw))
    printer.getText.clear
    sw.toString.stripSuffix("\n")
  }

  def renderInsns(insns: InsnList,
                  printer: Textifier,
                  methodPrinter: TraceMethodVisitor,
                  target: AbstractInsnNode = null,
                  decorate: AbstractInsnNode => fansi.Str = _ => "",
                  prefix: Seq[fansi.Str] = Seq("\n")): fansi.Str = {
    import collection.JavaConverters._
    val listing = if (target == null) insns.iterator.asScala else Iterator(target)
    val indices = insns.iterator().asScala.zipWithIndex.toMap
    fansi.Str.join(
      prefix ++ listing
        .flatMap{ k =>
          val rhs = prettyprint0(k, printer, methodPrinter)
          val splitIndex0 = rhs.indexWhere(_ != ' ')
          val splitIndex = rhs.indexWhere(_ == ' ', splitIndex0) match{
            case -1 => rhs.length
            case n => n
          }
          val (opcode, rest) = rhs.splitAt(splitIndex)
          Seq[fansi.Str](
            "\n",
            fansi.Color.Magenta("#" + indices(k).toString.padTo(3, ' ')),
            fansi.Color.Yellow(opcode),
            fansi.Color.Green(rest),
            " ",
            decorate(k)
          )
        }
        .toSeq
        .drop(1):_*
    )
  }

  def renderBlockCode(blockCode: Seq[Seq[AbstractInsnNode]],
                      finalInsns: InsnList) = {
    val output = mutable.Buffer.empty[fansi.Str]
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)
    for(insns <- blockCode){
      for(insn <- insns) {
        output.append(
          Renderer.renderInsns(finalInsns, printer, methodPrinter, insn, prefix = Nil)
        )
      }
      output.append("")
    }
    fansi.Str(output.mkString("\n"))
  }

  def renderLoopTree(loopTree: HavlakLoopTree.Loop[SSA.Block],
                     savedLocals: Map[SSA.Node, (Int, String)]) = {
    val out = mutable.Buffer.empty[String]
    def rec(l: HavlakLoopTree.Loop[SSA.Block], depth: Int, label0: List[Int]): Unit = {
      val indent = "    " * depth
      val id = label0.reverseIterator.map("-" + _).mkString
      val reducible = if (l.isReducible) "" else " (Irreducible)"
      val header = savedLocals(l.primaryHeader)._2
      val blockStr = l.basicBlocks.filter(_ != l.primaryHeader).map(x => savedLocals(x)._2).mkString("[", ", ", "]")
      out.append(s"${indent}loop$id$reducible, header: $header, blocks: $blockStr")

      for ((c, i) <- l.children.zipWithIndex) rec(c, depth + 1, i :: label0)
    }

    rec(loopTree, 0, Nil)

    fansi.Str(out.mkString("\n"))
  }

  def renderControlFlowGraph(controlFlowEdges: Seq[(SSA.Control, SSA.Control)],
                             savedLocals: Map[SSA.Node, (Int, String)]) = {
    Renderer.renderGraph(
      controlFlowEdges,
      (lhs, rhs, indent) =>
        fansi.Color.Magenta(savedLocals(lhs)._2) ++
          " <- " ++
          fansi.Str.join(rhs.flatMap(r => Seq[Str](", ", fansi.Color.Magenta(savedLocals(r)._2))).drop(1):_*)
    )
  }
  def renderGraph(edges: Seq[(SSA.Control, SSA.Control)],
                  coloring: (SSA.Control, Seq[SSA.Control], String) => fansi.Str,
                  annotation: (SSA.Control, String) => Seq[fansi.Str] = (_, _) => Nil): fansi.Str = {
    val loopTree = HavlakLoopTree.analyzeLoops(edges, edges.flatMap{case (k, v) => Seq(k, v)})
    val loopNestMap = mutable.LinkedHashMap.empty[SSA.Node, Int]

    def recLoop(loop: HavlakLoopTree.Loop[SSA.Control], depth: Int): Unit = {
      loop.basicBlocks.foreach(loopNestMap(_) = depth)
      loop.children.foreach(recLoop(_, depth + 1))
    }

    recLoop(loopTree, 0)

    val successor = edges.groupBy(_._1).map{case (k, v) => (k, v.map(_._2))}
    val predecessor = edges.groupBy(_._2).map{case (k, v) => (k, v.map(_._1))}
    val allVertices = edges.flatMap{case (a, b) => Seq(a, b)}
    val startNode = allVertices.find(!predecessor.contains(_)).get

    val out = mutable.Buffer.empty[fansi.Str]
    val seen = mutable.LinkedHashSet.empty[SSA.Control]
    def recPrint(block: SSA.Control): Unit = if (!seen(block)){
      seen.add(block)
      if (out.nonEmpty) out.append("\n")
      val indent = "  " * loopNestMap(block)
      out.append(indent)
      out.append(coloring(block, predecessor.getOrElse(block, Nil), indent))
      for(a <- annotation(block, indent)){
        out.append("\n")
        out.append(a)
      }
      for(s <- successor.getOrElse(block, Nil)) recPrint(s)
    }

    recPrint(startNode)
    fansi.Str.join(out:_*)
  }


  def binOpString(op: SSA.BinOp.Code): String = {
    import SSA.BinOp._
    op match{
      case IADD | FADD | LADD | DADD=> "+"
      case ISUB | FSUB | LSUB | DSUB=> "-"
      case IMUL | FMUL | LMUL | DMUL=> "*"
      case IDIV | FDIV | LDIV | DDIV=> "/"
      case IREM | FREM | LREM | DREM=> "%"
      case ISHL | LSHL => "<<"
      case ISHR | LSHR => ">>"
      case IUSHR | LUSHR => ">>>"
      case IAND | LAND => "&"
      case IOR | LOR => "|"
      case IXOR | LXOR => "^"
      case LCMP => "<>"
      case FCMPL => ">"
      case FCMPG => "<"
      case DCMPL => ">"
      case DCMPG => "<"
    }
  }
  def unaryOpString(op: SSA.UnaOp.Code): String = {
    import SSA.UnaOp._
    op match{
      case INEG => "-"
      case L2I | F2I |D2I => "(int)"
      case I2B => "(byte)"
      case I2C => "(char)"
      case I2S => "(short)"
      case FNEG => "-"
      case I2F | L2F | D2F => "(float)"
      case LNEG => "-"
      case I2L | F2L | D2L => "(long)"
      case DNEG => "-"
      case I2D | L2D | F2D => "(double)"
    }
  }
  def binBranchString(op: SSA.BinBranch.Code): String = {
    import SSA.BinBranch._
    op match{
      case IF_ICMPEQ | IF_ACMPEQ => "=="
      case IF_ICMPNE | IF_ACMPNE => "!="
      case IF_ICMPLT => "<"
      case IF_ICMPGE => ">="
      case IF_ICMPGT => ">"
      case IF_ICMPLE => "<="
    }
  }
  def unaryBranchString(op: SSA.UnaBranch.Code): String = {
    import SSA.UnaBranch._
    op match{
      case IFEQ => "== 0"
      case IFNE => "!= 0"
      case IFLT => "< 0"
      case IFGE => ">= 0"
      case IFGT => "> 0"
      case IFLE => "<= 0"
      case IFNULL => "== null"
      case IFNONNULL => "!= null"
    }
  }


  def findControlFlowGraph(methodBody: MethodBody) = {
    val controlFlowEdges = mutable.Buffer.empty[(SSA.Control, SSA.Control)]
    val visited = mutable.LinkedHashSet.empty[SSA.Control]

    def rec(current: SSA.Control): Unit = if (!visited(current)){
      visited.add(current)

      for(control <- current.controls){
        rec(control)
        controlFlowEdges.append(control -> current)
      }
    }

    methodBody.allTerminals.foreach(rec)
    controlFlowEdges
  }

  def dumpSvg(methodBody: MethodBody): LogMessage.Graph = {



    val live = Util.breadthFirstSeen[SSA.Node](methodBody.allTerminals.toSet)(_.upstream)

    val (seen, terminals, allEdges) = Util.breadthFirstAggregation0[SSA.Node, Boolean](methodBody.allTerminals.toSet)(
      x => x.upstream.map(_ -> true) ++ x.downstreamList.map(_ -> false)
    )

    val (liveNodes, deadNodes) = seen.keySet.partition(live)

    val allGraphvizNodes = mutable.LinkedHashMap[SSA.Node, LogMessage.Graph.Node]()

    for(x <- seen.keysIterator) {
      allGraphvizNodes.put(
        x,
        LogMessage.Graph.Node(
          x.toString,
          x match {
            case n: SSA.Val => "cyan"
            case n: SSA.State => "blue"
            case c: SSA.Block => "magenta"
            case c: SSA.Control => "red"
          },
          live(x)
        )
      )
    }
    val nodeIndices = allGraphvizNodes.keysIterator.zipWithIndex.toMap


    val directedEdges: Map[SSA.Node, Seq[(SSA.Node, Int, Int)]] = allEdges
      .map{case (a0, b0, isUpstream) => if (isUpstream) (a0, b0) else (b0, a0)}
      .distinct
      .map{case (a, b) => (a, b, a.downstreamList.count(_ == b), b.upstream.count(_ == a))}
      .groupBy(_._1)
      .map{case (k, vs) => (k, vs.map(v => (v._2, v._3, v._4)))}

    val logEdges = for{
      (x, ys) <- directedEdges
      (y, downs, ups) <- ys
      thick = x.isInstanceOf[SSA.Control] && y.isInstanceOf[SSA.Control]
      defaultEdge = LogMessage.Graph.Edge(nodeIndices(x), nodeIndices(y), true, false, thick)
      edge <-
        if (downs == ups) Seq.fill(downs)(defaultEdge)
        else if (downs < ups) {
          Seq.fill(downs)(defaultEdge) ++
            Seq.fill(ups - downs)(defaultEdge.copy(dashed = true))
        }
        else if (downs > ups) {
          Seq.fill(ups)(defaultEdge) ++
            Seq.fill(downs - ups)(defaultEdge.copy(dashed = true, forwardArrow = false))
        }
        else ???
    } yield edge


    LogMessage.Graph(
      allGraphvizNodes.valuesIterator.toArray[LogMessage.Graph.Node],
      logEdges.toSeq
    )
  }
}
