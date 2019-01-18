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
  private val printer = new Textifier
  private val methodPrinter = new TraceMethodVisitor(printer)

  def prettyprint(insnNode: AbstractInsnNode) = {
    insnNode.accept(methodPrinter)
    val sw = new StringWriter
    printer.print(new PrintWriter(sw))
    printer.getText.clear
    sw.toString.stripSuffix("\n")
  }

  def renderInsns(insns: InsnList, target: AbstractInsnNode = null): fansi.Str = {
    import collection.JavaConverters._
    val listing = if (target == null) insns.iterator.asScala else Iterator(target)
    val indices = insns.iterator().asScala.zipWithIndex.toMap
    fansi.Str.join(
      listing
        .flatMap{ k =>
          val rhs = prettyprint(k)
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

  def renderGraph(edges: Seq[(SSA.Control, SSA.Control)],
                  coloring: (SSA.Control, Seq[SSA.Control], String) => fansi.Str,
                  annotation: (SSA.Control, String) => Seq[fansi.Str] = (_, _) => Nil): fansi.Str = {
    val loopTree = HavlakLoopTree.analyzeLoops(edges)
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

  def renderSSA(program: Program, scheduledVals: Map[SSA.Val, SSA.Control] = Map.empty): (fansi.Str, Map[SSA.Node, String]) = {

    val (finalOrderingMap, saveable, savedLocals) = Util.findSaveable(program, scheduledVals)

    val savedBlocks = mutable.LinkedHashMap.empty[SSA.Control, (Int, String)]
    def getBlockId(c: SSA.Control) = fansi.Color.Magenta(getBlockId0(c)._2)
    def getBlockId0(c: SSA.Control): (Int, String) =
      savedBlocks.getOrElseUpdate(
        c,
        (savedBlocks.size, c match{
          case _: SSA.Return => "return" + savedBlocks.size
          case _: SSA.ReturnVal => "return" + savedBlocks.size
          case _: SSA.AThrow => "return" + savedBlocks.size
          case n: SSA.True => "true" + getBlockId0(n.block)._1
          case n: SSA.False => "false" + getBlockId0(n.block)._1
          case r: SSA.Merge =>
            val name = if (c.upstream.isEmpty) "start" else "block"
            name + savedBlocks.size
          case _: SSA.UnaBranch => "branch" + savedBlocks.size
          case _: SSA.BinBranch => "branch" + savedBlocks.size
        })
      )

    def apply(lhs: String, operands: Tree*) = pprint.Tree.Apply(lhs, operands.toIterator)
    def atom(lhs: String) = pprint.Tree.Lazy(ctx => Iterator(lhs))
    def literal(lhs: String) = pprint.Tree.Literal(lhs)
    def infix(lhs: Tree, op: String, rhs: Tree) = pprint.Tree.Infix(lhs, op, rhs)

    def renderBlock(block: SSA.Control) = {
      atom(getBlockId(block).toString)
    }
    def rec(ssa: SSA.Node): Tree = ssa match{
      case x: SSA.Control => atom(getBlockId(x).toString)
      case x: SSA.Val =>
        if (savedLocals.contains(x)) atom(fansi.Color.Cyan(savedLocals(x)._2).toString())
        else recVal(x)
    }

    def recVal(ssa: SSA.Val): Tree = ssa match{
      case n: SSA.Copy => apply("copy", rec(n.src))
      case phi: SSA.Phi =>
        val block = Seq(renderBlock(phi.block))
        val children = phi.incoming.map{case (block, ssa) => infix(renderBlock(block), ":", rec(ssa))}.toSeq
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
      case n: SSA.MultiANewArray => apply("multianewarray", atom(n.desc.name))
      case n: SSA.PutStatic => apply("putstatic", rec(n.src), atom(n.cls.name), atom(n.name), atom(n.desc.name))
      case n: SSA.GetStatic => apply("getstatic", atom(n.cls.name), atom(n.name), atom(n.desc.name))
      case n: SSA.PutField => apply("putfield", rec(n.src), rec(n.obj), atom(n.owner.name), atom(n.name), atom(n.desc.name))
      case n: SSA.GetField => apply("getfield", rec(n.obj), atom(n.owner.name), atom(n.name), atom(n.desc.name))
      case n: SSA.PutArray => apply("putarray", rec(n.src), rec(n.indexSrc), rec(n.arrayValue))
      case n: SSA.GetArray => apply("putarray", rec(n.indexSrc), rec(n.array))
      case n: SSA.MonitorEnter => ???
      case n: SSA.MonitorExit => ???
    }

    def recBlock(block: SSA.Control): (Str, Tree) = block match{
      case n: SSA.True => (getBlockId(block), apply("true", atom(getBlockId(n.block).toString)))
      case n: SSA.False => (getBlockId(block), apply("false", atom(getBlockId(n.block).toString)))

      case reg: SSA.Merge =>
        val name = if (reg.upstream.isEmpty) "start" else "merge"
        val rhs = apply(name + reg.insnIndex, reg.upstream.iterator.map(x => atom(getBlockId(x).toString)).toSeq:_*)
        (getBlockId(reg), rhs)

      case n: SSA.AThrow => ???
      case n: SSA.TableSwitch => ???
      case n: SSA.LookupSwitch => ???

      case n: SSA.ReturnVal =>
        (getBlockId(block), apply("return", atom(getBlockId(n.block).toString()), rec(n.a)))

      case n: SSA.Return =>
        (getBlockId(block), apply("return", atom(getBlockId(n.block).toString())))

      case n: SSA.UnaBranch =>
        val rhs = apply("if", renderBlock(n.block), rec(n.a), atom(unaryBranchString(n.opcode)))
        (getBlockId(n), rhs)

      case n: SSA.BinBranch =>
        val rhs = apply("if", renderBlock(n.block), infix(rec(n.a), binBranchString(n.opcode), rec(n.b)))
        (getBlockId(n), rhs)
    }

    def renderStmt(r: SSA.Node, leftOffset: Int) = {
      if (r.isInstanceOf[SSA.Arg]) None
      else {
        val out = mutable.Buffer.empty[Str]
        val (lhs, rhs) = r match {
          case r: SSA.Control =>
            recBlock(r)
          case r: SSA.Val => (fansi.Color.Cyan(savedLocals(r)._2), recVal(r))
        }

        out.append(lhs, " = ")
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
          Util.findControlFlowGraph(program),
          (l, rhs, indent) => fansi.Str.join(renderStmt(l, indent.length / 2).get:_*),
          (l, indent) => {

            val n = scheduledVals
              .collect{case (a, b) if b == l && saveable(a) => a}
              .toSeq.sortBy(finalOrderingMap(_))
              .map{ a =>
                renderStmt(a, indent.length / 2).map(x => fansi.Str.join(fansi.Str(indent) +: x:_*))
              }

            n.flatten.toSeq
          }
        )
      }else{
        fansi.Str.join(
          saveable
            .toSeq
            .sortBy(finalOrderingMap)
            .flatMap(
              renderStmt(_, 0) match{
                case None => Nil
                case Some(x) => fansi.Str("\n") +: x
              }
            ):_*
        )
      }

    val mapping = (savedLocals.mapValues(_._2) ++ savedBlocks.mapValues(_._2)).toMap
    (out, mapping)
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
}
