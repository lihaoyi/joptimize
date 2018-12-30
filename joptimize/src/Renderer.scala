package joptimize
import java.io.{PrintWriter, StringWriter}
import java.util

import org.objectweb.asm.tree.{AbstractInsnNode, InsnList}
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

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

  def render(insns: InsnList, target: AbstractInsnNode = null): fansi.Str = {
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

  def render(allTerminals: Seq[SSA],
             phiMerges: Map[SSA.Phi, Set[SSA]]): fansi.Str = {

    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[SSA](allTerminals.toSet){ ssa =>
        ssa.upstream ++ (ssa match{
          case phi: SSA.Phi => phiMerges.getOrElse(phi, Nil)
          case _ => Nil
        })
      }

    val saveable = downstreamEdges
      .groupBy(_._1)
      .map{
        case (k: SSA.PushI, _) => (k, false)
        case (k: SSA.PushJ, _) => (k, false)
        case (k: SSA.PushF, _) => (k, false)
        case (k: SSA.PushD, _) => (k, false)
        case (k: SSA.PushS, _) => (k, false)
        case (k: SSA.PushNull, _) => (k, false)
        case (k: SSA.PushCls, _) => (k, false)
        case (k: SSA.Phi, _) => (k, true)
        case (k, x) =>
          val n = x.distinct.size
          (k, n > 1 || n == 1 && allTerminals.contains(k))
      }

    val out = mutable.Buffer.empty[fansi.Str]

    val renderRoots = allVertices.filter(i => allTerminals.contains(i) || saveable.getOrElse(i, false))
    val savedLocals = new util.IdentityHashMap[SSA, Int]()
    for((r, i) <- renderRoots.zipWithIndex) savedLocals.put(r, i)

    def apply(lhs: String, operands: pprint.Tree*) = pprint.Tree.Apply(lhs, operands.toIterator)
    def atom(lhs: String) = pprint.Tree.Lazy(ctx => Iterator(lhs))
    def literal(lhs: String) = pprint.Tree.Literal(lhs)
    def infix(lhs: pprint.Tree, op: String, rhs: pprint.Tree) = pprint.Tree.Infix(lhs, op, rhs)

    def treeify0(ssa: SSA): pprint.Tree = ssa match{
      case phi: SSA.Phi => apply("phi", phiMerges(phi).map(treeify).toSeq:_*)
      case SSA.Arg(index, typeSize) => atom(fansi.Color.Cyan("arg" + index).toString)
      case SSA.BinOp(a, b, opcode) => infix(treeify(a), binOpString(opcode), treeify(b))
      case SSA.UnaryOp(a, opcode) => apply(unaryOpString(opcode), treeify(a))
      case SSA.UnaryBranch(a, opcode) => apply("if", treeify(a), atom(unaryBranchString(opcode)))
      case SSA.BinBranch(a, b, opcode) => apply("if", infix(treeify(a), binBranchString(opcode), treeify(b)))
      case SSA.ReturnVal(_, a) => apply("return", treeify(a))
      case SSA.Return(_) => apply("return")
      case SSA.AThrow(src) => apply("throw", treeify(src))
      case SSA.TableSwitch(src, min, max) => ???
//        val args =
//          Seq(treeify(src), atom(min.toString), atom(max.toString), blockify(default)) ++
//          targets.map(blockify)
//        apply("tableswitch", args:_*)

      case SSA.LookupSwitch(src, keys) => ???
//        val args =
//          Seq(treeify(src)) ++ keys.map(i => atom(i.toString)) ++
//          Seq(blockify(default)) ++ targets.map(blockify)
//        apply("lookupswitch", args:_*)

      case SSA.Goto() => apply("goto")
      case SSA.CheckCast(src, desc) => apply("cast", treeify(src), atom(desc.name))
      case SSA.ArrayLength(src) => apply("arraylength", treeify(src))
      case SSA.InstanceOf(src, desc) => apply("instanceof", treeify(src), atom(desc.name))
      case SSA.PushI(value) => literal(value + "")
      case SSA.PushJ(value) => literal(value + "L")
      case SSA.PushF(value) => literal(value + "F")
      case SSA.PushD(value) => literal(value + "D")
      case SSA.PushS(value) => literal(pprint.Util.literalize(value))
      case SSA.PushNull() => literal("null")
      case SSA.PushCls(value) => literal(value.name)
      case SSA.InvokeStatic(srcs, cls, name, desc) => apply(cls.javaName + "." + name + desc.unparse, srcs.map(treeify):_*)
      case SSA.InvokeSpecial(srcs, cls, name, desc) => apply(cls.javaName + "##" + name + desc.unparse, srcs.map(treeify):_*)
      case SSA.InvokeVirtual(srcs, cls, name, desc) => apply(cls.javaName + "#" + name + desc.unparse, srcs.map(treeify):_*)
      case SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs) => ???
      case SSA.New(cls) => apply("new", atom(cls.name))
      case SSA.NewArray(src, typeRef) => apply("newarray", treeify(src), atom(typeRef.name))
      case SSA.MultiANewArray(desc, dims) => apply("multianewarray", atom(desc.name))
      case SSA.PutStatic(src, cls, name, desc) => apply("putstatic", treeify(src), atom(cls.name), atom(name), atom(desc.name))
      case SSA.GetStatic(cls, name, desc) => apply("getstatic", atom(cls.name), atom(name), atom(desc.name))
      case SSA.PutField(src, obj, owner, name, desc) => apply("putfield", treeify(src), treeify(obj), atom(owner.name), atom(name), atom(desc.name))
      case SSA.GetField(obj, owner, name, desc) => apply("getfield", treeify(obj), atom(owner.name), atom(name), atom(desc.name))
      case SSA.PutArray(src, indexSrc, array) => apply("putarray", treeify(src), treeify(indexSrc), treeify(array))
      case SSA.GetArray(indexSrc, array, typeSize) => apply("putarray", treeify(indexSrc), treeify(array))
      case SSA.MonitorEnter(indexSrc) => ???
      case SSA.MonitorExit(indexSrc) => ???
    }
    def treeify(ssa: SSA): pprint.Tree = {
      if (savedLocals.containsKey(ssa)) atom(fansi.Color.Cyan("local" + savedLocals.get(ssa)).toString())
      else treeify0(ssa)
    }

    out.append("\n")
    for(r <- renderRoots){
      val (lhs, sep) =
        if (r.getSize == 0) ("  ", "")
        else ("  local" + savedLocals.get(r), " = ")

      out.append(fansi.Color.Cyan(lhs))
      out.append(sep)
      out.appendAll(
        new pprint.Renderer(80, fansi.Color.Yellow, fansi.Color.Green, 2)
          .rec(treeify0(r), lhs.length + sep.length, 1).iter
      )
      out.append("\n")
    }
    out.append("\n")

    fansi.Str.join(out:_*)
  }

  def binOpString(op: SSA.BinOp.Code): String = op match{
    case SSA.BinOp.IADD => "+"
    case SSA.BinOp.ISUB => "-"
    case SSA.BinOp.IMUL => "*"
    case SSA.BinOp.IDIV => "/"
    case SSA.BinOp.IREM => "%"
    case SSA.BinOp.ISHL => "<<"
    case SSA.BinOp.ISHR => ">>"
    case SSA.BinOp.IUSHR => ">>>"
    case SSA.BinOp.IAND => "&"
    case SSA.BinOp.IOR => "|"
    case SSA.BinOp.IXOR => "^"
    case SSA.BinOp.FADD => "+"
    case SSA.BinOp.FSUB => "-"
    case SSA.BinOp.FMUL => "*"
    case SSA.BinOp.FDIV => "/"
    case SSA.BinOp.FREM => "%"
    case SSA.BinOp.LCMP => "<>"
    case SSA.BinOp.FCMPL => ">"
    case SSA.BinOp.FCMPG => "<"
    case SSA.BinOp.DCMPL => ">"
    case SSA.BinOp.DCMPG => "<"
    case SSA.BinOp.LADD => "+"
    case SSA.BinOp.LSUB => "-"
    case SSA.BinOp.LMUL => "*"
    case SSA.BinOp.LDIV => "/"
    case SSA.BinOp.LREM => "%"
    case SSA.BinOp.LSHL => "<<"
    case SSA.BinOp.LSHR => ">>"
    case SSA.BinOp.LUSHR => ">>>"
    case SSA.BinOp.LAND => "&"
    case SSA.BinOp.LOR => "|"
    case SSA.BinOp.LXOR => "^"
    case SSA.BinOp.DADD => "+"
    case SSA.BinOp.DSUB => "-"
    case SSA.BinOp.DMUL => "*"
    case SSA.BinOp.DDIV => "/"
    case SSA.BinOp.DREM => "%"
  }
  def unaryOpString(op: SSA.UnaryOp.Code): String = op match{
    case SSA.UnaryOp.INEG => "-"
    case SSA.UnaryOp.L2I => "(int)"
    case SSA.UnaryOp.F2I => "(int)"
    case SSA.UnaryOp.D2I => "(int)"
    case SSA.UnaryOp.I2B => "(byte)"
    case SSA.UnaryOp.I2C => "(char)"
    case SSA.UnaryOp.I2S => "(short)"
    case SSA.UnaryOp.FNEG => "-"
    case SSA.UnaryOp.I2F => "(float)"
    case SSA.UnaryOp.L2F => "(float)"
    case SSA.UnaryOp.D2F => "(float)"
    case SSA.UnaryOp.LNEG => "-"
    case SSA.UnaryOp.I2L => "(long)"
    case SSA.UnaryOp.F2L => "(long)"
    case SSA.UnaryOp.D2L => "(long)"
    case SSA.UnaryOp.DNEG => "-"
    case SSA.UnaryOp.I2D => "(double)"
    case SSA.UnaryOp.L2D => "(double)"
    case SSA.UnaryOp.F2D => "(double)"
  }
  def binBranchString(op: SSA.BinBranch.Code): String = op match{

    case SSA.BinBranch.IF_ICMPEQ => "=="
    case SSA.BinBranch.IF_ICMPNE => "!="
    case SSA.BinBranch.IF_ICMPLT => "<"
    case SSA.BinBranch.IF_ICMPGE => ">="
    case SSA.BinBranch.IF_ICMPGT => ">"
    case SSA.BinBranch.IF_ICMPLE => "<="
    case SSA.BinBranch.IF_ACMPEQ => "=="
    case SSA.BinBranch.IF_ACMPNE => "!="
  }
  def unaryBranchString(op: SSA.UnaryBranch.Code): String = op match{
    case SSA.UnaryBranch.IFEQ => "== 0"
    case SSA.UnaryBranch.IFNE => "!= 0"
    case SSA.UnaryBranch.IFLT => "< 0"
    case SSA.UnaryBranch.IFGE => ">= 0"
    case SSA.UnaryBranch.IFGT => "> 0"
    case SSA.UnaryBranch.IFLE => "<= 0"
    case SSA.UnaryBranch.IFNULL => "== null"
    case SSA.UnaryBranch.IFNONNULL => "!= null"
  }
}
