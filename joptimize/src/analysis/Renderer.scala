package joptimize.analysis
import java.io.{PrintWriter, StringWriter}

import fansi.Str
import joptimize.Util

import joptimize.graph.HavlakLoopTree
import joptimize.model.{Program, SSA}
import org.objectweb.asm.tree.{AbstractInsnNode, InsnList}
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}
import pprint.Tree

import collection.mutable
object Renderer {


  def prettyprint(insnNode: AbstractInsnNode,
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
                  target: AbstractInsnNode = null): fansi.Str = {
    import collection.JavaConverters._
    val listing = if (target == null) insns.iterator.asScala else Iterator(target)
    val indices = insns.iterator().asScala.zipWithIndex.toMap
    fansi.Str.join(
      listing
        .flatMap{ k =>
          val rhs = prettyprint(k, printer, methodPrinter)
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
            fansi.Color.Green(rest)
          )
        }
        .toSeq
        .drop(1):_*
    )
  }

  def renderBlockCode(blockCode: Seq[(Seq[AbstractInsnNode], Option[AbstractInsnNode])],
                      finalInsns: InsnList) = {
    val output = mutable.Buffer.empty[fansi.Str]
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)
    for((insns, footer) <- blockCode){
      for(insn <- insns ++ footer) output.append(Renderer.renderInsns(finalInsns, printer, methodPrinter, insn))
      output.append("")
    }
    output.mkString("\n")
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

    out.mkString("\n")
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

  def renderSSA(program: Program,
                naming: Namer.Result,
                scheduledVals: Map[SSA.Val, SSA.Control] = Map.empty): fansi.Str = {

    def apply(lhs: String, operands: Tree*) = pprint.Tree.Apply(lhs, operands.toIterator)
    def atom(lhs: String) = pprint.Tree.Lazy(ctx => Iterator(lhs))
    def literal(lhs: String) = pprint.Tree.Literal(lhs)
    def infix(lhs: Tree, op: String, rhs: Tree) = pprint.Tree.Infix(lhs, op, rhs)

    def renderBlock(block: SSA.Control) = {
      fansi.Color.Magenta(naming.savedLocals(block)._2).toString
    }
    def rec(ssa: SSA.Node): Tree = ssa match{
      case x: SSA.Control => atom(naming.savedLocals(x)._2)
      case x: SSA.Val =>
        if (naming.savedLocals.contains(x)) atom(fansi.Color.Cyan(naming.savedLocals(x)._2).toString())
        else recVal(x)
    }

    def recVal(ssa: SSA.Val): Tree = ssa match{
      case n: SSA.Arg => literal("arg" + n.index)
      case n: SSA.Copy => apply("copy", rec(n.src))
      case phi: SSA.Phi =>
        val block = Seq(atom(renderBlock(phi.block)))
        val children = phi.incoming.map{case (block, ssa) => infix(atom(renderBlock(block)), ":", rec(ssa))}.toSeq
        apply("phi", block ++ children:_*)
      case n: SSA.BinOp => infix(rec(n.a), binOpString(n.opcode), rec(n.b))
      case n: SSA.UnaOp => apply(unaryOpString(n.opcode), rec(n.a))
      case n: SSA.CheckCast => apply("cast", rec(n.src), atom(n.desc.name))
      case n: SSA.ArrayLength => apply("arraylength", rec(n.src))
      case n: SSA.InstanceOf => apply("instanceof", rec(n.src), atom(n.desc.name))
      case n: SSA.PushI => literal(n.value + "")
      case n: SSA.PushJ => literal(n.value + "L")
      case n: SSA.PushF => literal(n.value + "F")
      case n: SSA.PushD => literal(n.value + "D")
      case n: SSA.PushS => literal(pprint.Util.literalize(n.value))
      case n: SSA.PushNull => literal("null")
      case n: SSA.PushCls => literal(n.value.name)
      case n: SSA.InvokeStatic => apply(n.cls.javaName + "." + n.name + n.desc.unparse, n.srcs.map(rec):_*)
      case n: SSA.InvokeSpecial => apply(n.cls.javaName + "##" + n.name + n.desc.unparse, n.srcs.map(rec):_*)
      case n: SSA.InvokeVirtual => apply(n.cls.javaName + "#" + n.name + n.desc.unparse, n.srcs.map(rec):_*)
      case n: SSA.InvokeDynamic => ???
      case n: SSA.New => apply("new", atom(n.cls.name))
      case n: SSA.NewArray => apply("newarray", rec(n.src), atom(n.typeRef.name))
      case n: SSA.MultiANewArray =>
        val atoms = atom(n.desc.name) +: n.dims.map(rec)
        apply("multianewarray", atoms:_*)
      case n: SSA.PutStatic => apply("putstatic", rec(n.src), atom(n.cls.name), atom(n.name), atom(n.desc.name))
      case n: SSA.GetStatic => apply("getstatic", atom(n.cls.name), atom(n.name), atom(n.desc.name))
      case n: SSA.PutField => apply("putfield", rec(n.src), rec(n.obj), atom(n.owner.name), atom(n.name), atom(n.desc.name))
      case n: SSA.GetField => apply("getfield", rec(n.obj), atom(n.owner.name), atom(n.name), atom(n.desc.name))
      case n: SSA.PutArray => apply("putarray", rec(n.src), rec(n.indexSrc), rec(n.arrayValue))
      case n: SSA.GetArray => apply("getarray", rec(n.array), rec(n.indexSrc))
      case n: SSA.MonitorEnter => ???
      case n: SSA.MonitorExit => ???
    }

    def recBlock(block: SSA.Control): (Str, Tree) = block match{
      case n: SSA.True => (renderBlock(n), apply("true", atom(fansi.Color.Magenta(naming.savedLocals(n.branch)._2).toString)))
      case n: SSA.False => (renderBlock(n), apply("false", atom(fansi.Color.Magenta(naming.savedLocals(n.branch)._2).toString)))

      case reg: SSA.Merge =>
        val name = if (reg.upstream.isEmpty) "start" else "merge"
        val rhs = apply(name + reg.insnIndex, reg.upstream.iterator.map(x => atom(fansi.Color.Magenta(naming.savedLocals(x)._2).toString)).toSeq:_*)
        (fansi.Color.Magenta(naming.savedLocals(reg)._2), rhs)

      case n: SSA.AThrow => ???
      case n: SSA.TableSwitch => ???
      case n: SSA.LookupSwitch => ???

      case n: SSA.ReturnVal =>
        (renderBlock(n), apply("return", atom(renderBlock(n.block)), rec(n.a)))

      case n: SSA.Return =>
        (renderBlock(n), apply("return", atom(renderBlock(n.block))))

      case n: SSA.UnaBranch =>
        val rhs = apply("if", atom(renderBlock(n.block)), rec(n.a), atom(unaryBranchString(n.opcode)))
        (fansi.Color.Magenta(naming.savedLocals(n)._2), rhs)

      case n: SSA.BinBranch =>
        val rhs = apply("if", atom(renderBlock(n.block)), infix(rec(n.a), binBranchString(n.opcode), rec(n.b)))
        (fansi.Color.Magenta(naming.savedLocals(n)._2), rhs)
    }

    def renderStmt(r: SSA.Node, leftOffset: Int) = {
      if (r.isInstanceOf[SSA.Arg]) None
      else {
        val out = mutable.Buffer.empty[Str]
        val (lhs, rhs) = r match {
          case r: SSA.Control =>
            recBlock(r)
          case r: SSA.Val => (fansi.Color.Cyan(naming.savedLocals(r)._2), recVal(r))
        }

        val printLeft = r match{
          case s: SSA.Val => s.getSize != 0
          case _ => true
        }
        if (printLeft) out.append(lhs, " = ")

        out.appendAll(
          new pprint.Renderer(80, fansi.Color.Yellow, fansi.Color.Green, 2)
            .rec(rhs, lhs.length + " = ".length, leftOffset).iter
        )
        Some(out)
      }
    }

    val out =
      if (scheduledVals.nonEmpty){
        renderGraph(
          Renderer.findControlFlowGraph(program),
          (l, rhs, indent) => fansi.Str.join(renderStmt(l, indent.length / 2).get:_*),
          (l, indent) => {

            val n = scheduledVals
              .collect{case (a, b) if b == l && naming.saveable(a) => a}
              .toSeq.sortBy(naming.finalOrderingMap(_))
              .map{ a =>
                renderStmt(a, indent.length / 2).map(x => fansi.Str.join(fansi.Str(indent) +: x:_*))
              }

            n.flatten.toSeq
          }
        )
      }else{
        fansi.Str.join(
          naming.saveable
            .toSeq
            .sortBy(naming.finalOrderingMap)
            .flatMap(
              renderStmt(_, 0) match{
                case None => Nil
                case Some(x) => fansi.Str("\n") +: x
              }
            ):_*
        )
      }

    out
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


  def findControlFlowGraph(program: Program) = {
    val controlFlowEdges = mutable.Buffer.empty[(SSA.Control, SSA.Control)]
    val visited = mutable.LinkedHashSet.empty[SSA.Control]

    def rec(current: SSA.Control): Unit = if (!visited(current)){
      visited.add(current)

      for(control <- current.controls){
        rec(control)
        controlFlowEdges.append(control -> current)
      }
    }

    program.allTerminals.foreach(rec)
    controlFlowEdges
  }
}
