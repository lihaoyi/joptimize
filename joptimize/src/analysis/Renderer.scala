package joptimize.analysis
import java.io.{PrintWriter, StringWriter}
import java.util

import joptimize.Util
import joptimize.model.{Program, SSA}
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

  def renderSSA(program: Program): fansi.Str = {

    val allTerminals = program.allTerminals
    val regionMerges = program.regionMerges
    val phiMerges = program.phiMerges
    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[SSA.Token](allTerminals.toSet){
        case ctrl: SSA.Region => regionMerges(ctrl).toSeq
        case SSA.True(inner) => Seq(inner)
        case SSA.False(inner) => Seq(inner)

        case ssa: SSA =>
          ssa.allUpstream ++ (ssa match{
            case phi: SSA.Phi => phiMerges(phi).flatMap(x => Seq(x._1, x._2))
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
      } ++ allVertices.collect{
        case b: SSA.Region => (b, true)
        case b: SSA.BinBranch => (b, true)
        case b: SSA.UnaBranch => (b, true)
        case b: SSA.Return => (b, true)
        case b: SSA.ReturnVal => (b, true)
      }

    val out = mutable.Buffer.empty[fansi.Str]

    val renderRoots = allVertices.filter(i => saveable.getOrElse(i, false))

    val savedLocals = new util.IdentityHashMap[SSA.Token, Int]()

    for((r, i) <- renderRoots.zipWithIndex) savedLocals.put(r, i)

    val savedControls = mutable.LinkedHashMap.empty[SSA.Control, Int]
    def getControlId(c: SSA.Control) = savedControls.getOrElseUpdate(c, savedControls.size)
    def apply(lhs: String, operands: pprint.Tree*) = pprint.Tree.Apply(lhs, operands.toIterator)
    def atom(lhs: String) = pprint.Tree.Lazy(ctx => Iterator(lhs))
    def literal(lhs: String) = pprint.Tree.Literal(lhs)
    def infix(lhs: pprint.Tree, op: String, rhs: pprint.Tree) = pprint.Tree.Infix(lhs, op, rhs)

    def renderControl(control: SSA.Control) = {
      atom(
        fansi.Color.Cyan(
          regionMerges.zipWithIndex.find(_._1._1 == control)match{
            case Some(x) => "region" + x._2
            case None =>
              "ctrl" + getControlId(control)
          }
        ).toString
      )
    }

    def renderBranchPrefix(n: SSA) = {
      fansi.Color.Cyan("ctrl" + getControlId(SSA.True(n))) + ", " +
      fansi.Color.Cyan("ctrl" + getControlId(SSA.False(n))) + " = if"
    }

    def treeify0(ssa: SSA): pprint.Tree = ssa match{
      case phi: SSA.Phi =>
        pprint.log(phiMerges(phi))
        apply("phi", phiMerges(phi).map{case (ctrl, ssa) => infix(renderControl(ctrl), ":", treeify(ssa))}.toSeq:_*)
      case SSA.Arg(index, typeSize) => atom(fansi.Color.Cyan("arg" + index).toString)
      case SSA.BinOp(a, b, opcode) => infix(treeify(a), binOpString(opcode), treeify(b))
      case SSA.UnaOp(a, opcode) => apply(unaryOpString(opcode), treeify(a))
      case n @ SSA.UnaBranch(control, a, opcode) =>
        apply(
          renderBranchPrefix(n),
          renderControl(control),
          treeify(a),
          atom(unaryBranchString(opcode))
        )
      case n @ SSA.BinBranch(control, a, b, opcode) =>
        apply(
          renderBranchPrefix(n),
          renderControl(control),
          infix(treeify(a), binBranchString(opcode), treeify(b))
        )
      case SSA.ReturnVal(ctrl, a) =>
        pprint.log(ctrl)
        apply("return", atom(fansi.Color.Cyan("region" + getControlId(ctrl)).toString()), treeify(a))
      case SSA.Return(ctrl) =>
        apply("return", atom(fansi.Color.Cyan("region" + getControlId(ctrl)).toString()))
      case SSA.AThrow(src) => apply("throw", treeify(src))
      case SSA.TableSwitch(src, min, max) => ???
      case SSA.LookupSwitch(src, keys) => ???
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
    val regionIndices = regionMerges.keysIterator.zipWithIndex.toMap

    pprint.log(renderRoots)
    for(r <- renderRoots){
      r match{
        case reg: SSA.Region =>
          out.append(fansi.Color.Cyan("region" + regionIndices(reg)), " = ", fansi.Color.Yellow("region"), "(")
          out.append(
            regionMerges(reg).map { c =>
              fansi.Color.Cyan(
                Option(savedControls.getOrElseUpdate(c, savedControls.size)) match{
                  case None => "region" + regionIndices(c.asInstanceOf[SSA.Region])
                  case Some(x) => "ctrl" + x
                }
              )
            }.mkString(", ")
          )
          out.append(")\n")
        case SSA.True(inner) => Seq(inner)
        case SSA.False(inner) => Seq(inner)
        case r: SSA =>
          val (lhs, sep) =
            if (r.getSize == 0) ("", "")
            else ("local" + savedLocals.get(r), " = ")
          out.append(fansi.Color.Cyan(lhs))
          out.append(sep)

          out.appendAll(
            new pprint.Renderer(80, fansi.Color.Yellow, fansi.Color.Green, 2)
              .rec(treeify0(r), lhs.length + sep.length, 1).iter
          )
      }



      out.append("\n")
    }
    out.append("\n")

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
}
