package joptimize.analysis
import java.io.{PrintWriter, StringWriter}
import java.util

import fansi.Str
import joptimize.Util
import joptimize.graph.TarjansStronglyConnectedComponents
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

  def renderGraph(edges: Seq[(SSA.Ctrl, SSA.Ctrl)], coloring: SSA.Ctrl => fansi.Str): fansi.Str = {
    val allVertices = edges.flatMap(x => Seq(x._1, x._2)).distinct
    val vertexToIndex = allVertices.zipWithIndex.toMap
    val edgeGroups = edges.groupBy(_._2).map{case (k, v) => (k, v.map(_._1))}

    val finalOrderingMap = Renderer.sortVerticesForPrinting(allVertices.toSet, edges)(
      (src, dest) => dest.isInstanceOf[SSA.Region]
    )
    val tarjansInput = allVertices.map(edgeGroups.getOrElse(_, Nil).map(vertexToIndex))
    val sorted = TarjansStronglyConnectedComponents(tarjansInput).flatten.map(allVertices(_)).sortBy(finalOrderingMap)
    val out = mutable.Buffer.empty[fansi.Str]

    for(dest <- sorted){

      val srcs = edgeGroups.getOrElse(dest, Nil)
      out.append(coloring(dest), " <- ")
      out.appendAll(srcs.flatMap(src => Seq(fansi.Str(", "), coloring(src))).drop(1))
      out.append("\n")
    }
    fansi.Str.join(out:_*)
  }

  def renderSSA(program: Program): (fansi.Str, Map[SSA.Node, String]) = {

    val allTerminals = program.allTerminals
    val regionMerges = program.regionMerges
    val phiMerges = program.phiMerges
    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[SSA.Node](allTerminals.toSet)(program.upstream)

    val finalOrderingMap = sortVerticesForPrinting(allVertices, downstreamEdges)(
      (src, dest) => dest.isInstanceOf[SSA.Phi] || dest.isInstanceOf[SSA.Region]
    )

    val saveable =
      downstreamEdges.groupBy(_._1)
        .filter{ case (k, x) => k.upstream.nonEmpty && x.distinct.size > 1 || x.distinct.size == 1 && allTerminals.contains(k) }
        .keySet ++
        allVertices.collect{ case k: SSA.Phi => k case b: SSA.Ctrl => b}

    val out = mutable.Buffer.empty[Str]

    val renderRoots = allVertices.filter(i => saveable.contains(i))

    val savedLocals = new util.IdentityHashMap[SSA.Val, Int]()

    for((r: SSA.Val, i) <- renderRoots.zipWithIndex) savedLocals.put(r, i)

    val savedControls = mutable.LinkedHashMap.empty[SSA.Ctrl, (Int, String)]
    def getControlId(c: SSA.Ctrl) = fansi.Color.Magenta(getControlId0(c)._2)
    def getControlId0(c: SSA.Ctrl): (Int, String) =
      savedControls.getOrElseUpdate(
        c,
        (savedControls.size, c match{
          case _: SSA.Return => "return" + savedControls.size
          case _: SSA.ReturnVal => "return" + savedControls.size
          case _: SSA.AThrow => "return" + savedControls.size
          case SSA.True(branch) => "true" + getControlId0(branch)._1
          case SSA.False(branch) => "false" + getControlId0(branch)._1
          case _ => "ctrl" + savedControls.size
        })
      )

    def apply(lhs: String, operands: Tree*) = pprint.Tree.Apply(lhs, operands.toIterator)
    def atom(lhs: String) = pprint.Tree.Lazy(ctx => Iterator(lhs))
    def literal(lhs: String) = pprint.Tree.Literal(lhs)
    def infix(lhs: Tree, op: String, rhs: Tree) = pprint.Tree.Infix(lhs, op, rhs)

    def renderControl(control: SSA.Ctrl) = {
      atom(getControlId(control).toString)
    }
    def rec(ssa: SSA.Node): pprint.Tree = ssa match{
      case x: SSA.Ctrl => atom(getControlId(x).toString)
      case x: SSA.Val =>
        if (savedLocals.containsKey(ssa)) atom(fansi.Color.Cyan("local" + savedLocals.get(ssa)).toString())
        else recVal(x)
    }

    def recVal(ssa: SSA.Val): Tree = ssa match{
      case phi: SSA.Phi => apply("phi", phiMerges(phi)._2.map{case (ctrl, ssa) => infix(renderControl(ctrl), ":", rec(ssa))}.toSeq:_*)
      case SSA.Arg(index, typeSize) => atom(fansi.Color.Cyan("arg" + index).toString)
      case SSA.BinOp(a, b, opcode) => infix(rec(a), binOpString(opcode), rec(b))
      case SSA.UnaOp(a, opcode) => apply(unaryOpString(opcode), rec(a))
      case SSA.CheckCast(src, desc) => apply("cast", rec(src), atom(desc.name))
      case SSA.ArrayLength(src) => apply("arraylength", rec(src))
      case SSA.InstanceOf(src, desc) => apply("instanceof", rec(src), atom(desc.name))
      case SSA.PushI(value) => literal(value + "")
      case SSA.PushJ(value) => literal(value + "L")
      case SSA.PushF(value) => literal(value + "F")
      case SSA.PushD(value) => literal(value + "D")
      case SSA.PushS(value) => literal(pprint.Util.literalize(value))
      case SSA.PushNull() => literal("null")
      case SSA.PushCls(value) => literal(value.name)
      case SSA.InvokeStatic(srcs, cls, name, desc) => apply(cls.javaName + "." + name + desc.unparse, srcs.map(rec):_*)
      case SSA.InvokeSpecial(srcs, cls, name, desc) => apply(cls.javaName + "##" + name + desc.unparse, srcs.map(rec):_*)
      case SSA.InvokeVirtual(srcs, cls, name, desc) => apply(cls.javaName + "#" + name + desc.unparse, srcs.map(rec):_*)
      case SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs) => ???
      case SSA.New(cls) => apply("new", atom(cls.name))
      case SSA.NewArray(src, typeRef) => apply("newarray", rec(src), atom(typeRef.name))
      case SSA.MultiANewArray(desc, dims) => apply("multianewarray", atom(desc.name))
      case SSA.PutStatic(src, cls, name, desc) => apply("putstatic", rec(src), atom(cls.name), atom(name), atom(desc.name))
      case SSA.GetStatic(cls, name, desc) => apply("getstatic", atom(cls.name), atom(name), atom(desc.name))
      case SSA.PutField(src, obj, owner, name, desc) => apply("putfield", rec(src), rec(obj), atom(owner.name), atom(name), atom(desc.name))
      case SSA.GetField(obj, owner, name, desc) => apply("getfield", rec(obj), atom(owner.name), atom(name), atom(desc.name))
      case SSA.PutArray(src, indexSrc, array) => apply("putarray", rec(src), rec(indexSrc), rec(array))
      case SSA.GetArray(indexSrc, array, typeSize) => apply("putarray", rec(indexSrc), rec(array))
      case SSA.MonitorEnter(indexSrc) => ???
      case SSA.MonitorExit(indexSrc) => ???
    }


    def recCtrl(ctrl: SSA.Ctrl): (fansi.Str, Tree) = ctrl match{
      case SSA.True(src) => (getControlId(ctrl), apply("true", atom(getControlId(src).toString)))
      case SSA.False(src) => (getControlId(ctrl), apply("false", atom(getControlId(src).toString)))

      case reg: SSA.Region =>
        val rhs = apply("region", regionMerges(reg).iterator.map(x => atom(getControlId(x).toString)).toSeq:_*)
        (getControlId(reg), rhs)

      case SSA.AThrow(src) => ???
      case SSA.TableSwitch(src, min, max) => ???
      case SSA.LookupSwitch(src, keys) => ???

      case SSA.ReturnVal(inner, a) =>
        (getControlId(ctrl), apply("return", atom(getControlId(inner).toString()), rec(a)))

      case SSA.Return(inner) =>
        (getControlId(ctrl), apply("return", atom(getControlId(inner).toString())))

      case n @ SSA.UnaBranch(control, a, opcode) =>
        val rhs = apply("if", renderControl(control), rec(a), atom(unaryBranchString(opcode)))
        (getControlId(n), rhs)

      case n @ SSA.BinBranch(control, a, b, opcode) =>
        val rhs = apply("if", renderControl(control), infix(rec(a), binBranchString(opcode), rec(b)))
        (getControlId(n), rhs)
    }

    for(r <- renderRoots.toSeq.sortBy(finalOrderingMap)){
      val (lhs, rhs) = r match{
        case r: SSA.Ctrl => recCtrl(r)
        case r: SSA.Val => (fansi.Color.Cyan("local" + savedLocals.get(r)), recVal(r))
      }

      out.append(lhs, " = ")
      out.appendAll(
        new pprint.Renderer(80, fansi.Color.Yellow, fansi.Color.Green, 2)
          .rec(rhs, lhs.length + " = ".length, 1).iter
      )
      out.append("\n")
    }

    import collection.JavaConverters._
    val mapping = (savedLocals.asScala.mapValues("local" + _) ++ savedControls.mapValues(_._2)).toMap
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
