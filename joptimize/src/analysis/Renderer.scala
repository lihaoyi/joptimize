package joptimize.analysis
import java.io.{PrintWriter, StringWriter}
import java.util

import fansi.Str
import joptimize.Util
import joptimize.graph.{HavlakLoopTree, TarjansStronglyConnectedComponents}
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

  def renderGraph(edges: Seq[(SSA.Block, SSA.Block)], coloring: SSA.Block => fansi.Str): fansi.Str = {
    val loopTree = HavlakLoopTree.analyzeLoops(edges)
    val loopNestMap = mutable.LinkedHashMap.empty[SSA.Node, Int]

    def recLoop(loop: HavlakLoopTree.Loop[SSA.Block], depth: Int): Unit = {
      loop.basicBlocks.foreach(loopNestMap(_) = depth)
      loop.children.foreach(recLoop(_, depth + 1))
    }

    recLoop(loopTree, 0)

    val allDownstreams = edges.flatMap{case (a, b) => Seq(b)}.toSet
    val successor = edges.groupBy(_._1).map{case (k, v) => (k, v.map(_._2))}
    val predecessor = edges.groupBy(_._2).map{case (k, v) => (k, v.map(_._1))}
    val allVertices = edges.flatMap{case (a, b) => Seq(a, b)}
    val startNode = allVertices.find(!allDownstreams(_)).get

    val out = mutable.Buffer.empty[fansi.Str]
    val seen = mutable.LinkedHashSet.empty[SSA.Block]
    def recPrint(block: SSA.Block): Unit = if (!seen(block)){
      seen.add(block)
      out.append("  " * loopNestMap(block))
      out.append(coloring(block), " <- ")
      out.appendAll(predecessor.getOrElse(block, Nil).flatMap(src => Seq(fansi.Str(", "), coloring(src))).drop(1))
      out.append("\n")
      for(s <- successor.getOrElse(block, Nil)) recPrint(s)
    }

    recPrint(startNode)
    fansi.Str.join(out:_*)
  }

  def renderSSA(program: Program, scheduledVals: Map[SSA.Val, SSA.Block] = Map.empty): (fansi.Str, Map[SSA.Node, String]) = {

    val allTerminals = program.allTerminals
    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[SSA.Node](allTerminals.toSet) { x =>
        val addedControl = x match{
          case v: SSA.Val => scheduledVals.get(v).toSeq
          case _ => Nil
        }
        x.upstream ++ addedControl
      }

    val finalOrderingMap = sortVerticesForPrinting(allVertices, downstreamEdges){
      case (_, _: SSA.Phi | _: SSA.Region) => true
      case (v: SSA.Val, c: SSA.Block) => true
      case _ => false
    }

    val saveable =
      downstreamEdges.groupBy(_._1)
        .filter{ case (k, x) =>
          val scheduled = k match{
            case v: SSA.Val =>
              val downstreamControls = x.collect{case (_, t: SSA.Val) => scheduledVals.get(t)}.flatten
              scheduledVals.get(v).exists(c => downstreamControls.exists(_ != c))
            case _ => false
          }
          k.upstream.nonEmpty && (x.distinct.size > 1 || allTerminals.contains(k) || scheduled)
        }
        .keySet ++
        allVertices.collect{ case k: SSA.Phi => k case b: SSA.Block => b}

    val out = mutable.Buffer.empty[Str]

    val savedLocals = new util.IdentityHashMap[SSA.Val, Int]()

    for((r: SSA.Val, i) <- saveable.zipWithIndex) savedLocals.put(r, i)

    val savedBlocks = mutable.LinkedHashMap.empty[SSA.Block, (Int, String)]
    def getBlockId(c: SSA.Block) = fansi.Color.Magenta(getBlockId0(c)._2)
    def getBlockId0(c: SSA.Block): (Int, String) =
      savedBlocks.getOrElseUpdate(
        c,
        (savedBlocks.size, c match{
          case _: SSA.Return => "return" + savedBlocks.size
          case _: SSA.ReturnVal => "return" + savedBlocks.size
          case _: SSA.AThrow => "return" + savedBlocks.size
          case n: SSA.True => "true" + getBlockId0(n.node)._1
          case n: SSA.False => "false" + getBlockId0(n.node)._1
          case r: SSA.Region => "region" + savedBlocks.size
          case _: SSA.UnaBranch => "branch" + savedBlocks.size
          case _: SSA.BinBranch => "branch" + savedBlocks.size
        })
      )

    def apply(lhs: String, operands: Tree*) = pprint.Tree.Apply(lhs, operands.toIterator)
    def atom(lhs: String) = pprint.Tree.Lazy(ctx => Iterator(lhs))
    def literal(lhs: String) = pprint.Tree.Literal(lhs)
    def infix(lhs: Tree, op: String, rhs: Tree) = pprint.Tree.Infix(lhs, op, rhs)

    def renderBlock(block: SSA.Block) = {
      atom(getBlockId(block).toString)
    }
    def rec(ssa: SSA.Node): pprint.Tree = ssa match{
      case x: SSA.Block => atom(getBlockId(x).toString)
      case x: SSA.Val =>
        if (savedLocals.containsKey(x)) atom(fansi.Color.Cyan("local" + savedLocals.get(ssa)).toString())
        else recVal(x)
    }

    def recVal(ssa: SSA.Val): Tree = ssa match{
      case phi: SSA.Phi =>
        val block = Seq(renderBlock(phi.block))
        val children = phi.incoming.map{case (block, ssa) => infix(renderBlock(block), ":", rec(ssa))}.toSeq
        apply("phi", block ++ children:_*)
      case n: SSA.Arg => atom(fansi.Color.Cyan("arg" + n.index).toString)
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

    def recBlock(block: SSA.Block): (fansi.Str, Tree) = block match{
      case n: SSA.True => (getBlockId(block), apply("true", atom(getBlockId(n.node).toString)))
      case n: SSA.False => (getBlockId(block), apply("false", atom(getBlockId(n.node).toString)))

      case reg: SSA.Region =>
        val rhs = apply("region" + reg.insnIndex, reg.upstream.iterator.map(x => atom(getBlockId(x).toString)).toSeq:_*)
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

    pprint.log(finalOrderingMap)
    def renderStmt(r: SSA.Node) = {
      val (lhs, rhs) = r match{
        case r: SSA.Block =>
          out.append("\n")
          recBlock(r)
        case r: SSA.Val => (fansi.Color.Cyan("local" + savedLocals.get(r)), recVal(r))
      }

      out.append(lhs, " = ")
      out.appendAll(
        new pprint.Renderer(80, fansi.Color.Yellow, fansi.Color.Green, 2)
          .rec(rhs, lhs.length + " = ".length, 1).iter
      )
      out.append("\n")
    }

    if (scheduledVals.nonEmpty){
      for(r0 <- saveable.toSeq.sortBy(finalOrderingMap)){
        if (r0.isInstanceOf[SSA.Block]){
          val blockValues = scheduledVals.collect{case (a, b) if b == r0 && saveable(a) => a }
          for(r <- Seq(r0) ++ blockValues) renderStmt(r)
        }
      }
    }else{
      saveable.toSeq.sortBy(finalOrderingMap).foreach(renderStmt)
    }

    import collection.JavaConverters._
    val mapping = (savedLocals.asScala.mapValues("local" + _) ++ savedBlocks.mapValues(_._2)).toMap
    (fansi.Str.join(out:_*), mapping)
  }

  /**
    * We order vertices in two ways:
    *
    * 1) Between cycles, in topological data/control-flow order
    * 2) Within cycles, in dataflow order *ignoring phis/regions* to break the cycle
    *
    * That gives us roughly a sequence of instructions that start from top to
    * bottom, and within each cycle data/control always flows downwards except for
    * jumps which may return to an earlier phi/region node.
    */
  def sortVerticesForPrinting[T](allVertices: Set[T],
                                 downstreamEdges: Seq[(T, T)])
                                (backEdge: (T, T) => Boolean) = {
    val vertexToIndex = allVertices.zipWithIndex.toMap
    val indexToVertex = vertexToIndex.map(_.swap)


    val brokenEdgeLists = Util.edgeListToIndexMap(
      downstreamEdges.filter(x => !backEdge(x._1, x._2)),
      vertexToIndex
    )

    val brokenOrderingList = TarjansStronglyConnectedComponents(Util.mapToAdjacencyLists(brokenEdgeLists, allVertices.size)).map { case Seq(x) => x }

    val brokenOrdering = brokenOrderingList.zipWithIndex.toMap

    val groupedEdgeLists = Util.edgeListToIndexMap(downstreamEdges, vertexToIndex)

    val groupedOrdering = TarjansStronglyConnectedComponents(Util.mapToAdjacencyLists(groupedEdgeLists, allVertices.size))

    val orderingList = groupedOrdering.flatMap(_.sortBy(brokenOrdering)).map(indexToVertex)

    val finalOrderingMap = orderingList.reverse.zipWithIndex.toMap
    finalOrderingMap
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
