package joptimize
import java.util

import collection.mutable
object Renderer {
  def render(allVisitedBlocks: Seq[Walker.BlockResult],
             merges: Seq[(Frame[SSA], Frame[SSA])]): String = {
    pprint.log(allVisitedBlocks.length)
    val mergeLookup = mutable.Map.empty[SSA, mutable.Buffer[SSA]]
    val allTerminals = allVisitedBlocks.flatMap(_.terminalInsns)



    val (allVertices, roots, downstreamEdges) =
      Util.breadthFirstAggregation[SSA](allTerminals.toSet){ ssa =>
        ssa.upstream ++ mergeLookup.getOrElse(ssa, Nil)
      }

    pprint.log(downstreamEdges)
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
        case (k, x) =>
          val n = x.distinct.size
          (k, n > 1 || n == 1 && allTerminals.contains(k))
      }

    val out = mutable.Buffer.empty[fansi.Str]
    for((block, blockIndex) <- allVisitedBlocks.zipWithIndex){
      pprint.log(blockIndex)
      pprint.log(block.terminalInsns)
      pprint.log(block.blockInsns.value)
      val renderRoots = block.blockInsns.value.filter(i => block.terminalInsns.contains(i) || saveable.getOrElse(i, false))
      val savedLocals = new util.IdentityHashMap[SSA, Int]()
      for((r, i) <- renderRoots.zipWithIndex) savedLocals.put(r, i)

      def apply(lhs: String, operands: pprint.Tree*) = pprint.Tree.Apply(lhs, operands.toIterator)
      def atom(lhs: String) = pprint.Tree.Lazy(ctx => Iterator(lhs))
      def literal(lhs: String) = pprint.Tree.Literal(lhs)
      def infix(lhs: pprint.Tree, op: String, rhs: pprint.Tree) = pprint.Tree.Infix(lhs, op, rhs)
      def blockify(lhs: Block) = atom(fansi.Color.Magenta("block" + allVisitedBlocks.indexWhere(_.blockInsns == lhs).toString).toString)

      def treeify0(ssa: SSA): pprint.Tree = ssa match{
        case SSA.Arg(index, typeSize) => atom(fansi.Color.Cyan("arg" + index).toString)
        case SSA.BinOp(a, b, opcode) => infix(treeify(a), binOpString(opcode), treeify(b))
        case SSA.UnaryOp(a, opcode) => apply(unaryOpString(opcode), treeify(a))
        case SSA.UnaryBranch(a, target, opcode) => apply("if", treeify(a), atom(unaryBranchString(opcode)), blockify(target))
        case SSA.BinBranch(a, b, target, opcode) => apply("if", infix(treeify(a), binBranchString(opcode), treeify(b)), blockify(target))
        case SSA.ReturnVal(a) => apply("return", treeify(a))
        case SSA.Return() => apply("return")
        case SSA.AThrow(src) => apply("throw", treeify(src))
        case SSA.TableSwitch(src, min, max, default, targets) =>
          val args =
            Seq(treeify(src), atom(min.toString), atom(max.toString), blockify(default)) ++
            targets.map(blockify)
          apply("tableswitch", args:_*)

        case SSA.LookupSwitch(src, default, keys, targets) =>
          val args =
            Seq(treeify(src)) ++ keys.map(i => atom(i.toString)) ++
            Seq(blockify(default)) ++ targets.map(blockify)
          apply("lookupswitch", args:_*)

        case SSA.Goto(target) => apply("goto", blockify(target))
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
        case SSA.InvokeStatic(state, srcs, cls, name, desc) => apply(cls.javaName + "." + name + desc.unparse, srcs.map(treeify):_*)
        case SSA.InvokeSpecial(state, srcs, cls, name, desc) => apply(cls.javaName + "##" + name + desc.unparse, srcs.map(treeify):_*)
        case SSA.InvokeVirtual(state, srcs, cls, name, desc) => apply(cls.javaName + "#" + name + desc.unparse, srcs.map(treeify):_*)
        case SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs) => ???
        case SSA.New(cls) => apply("new", atom(cls.name))
        case SSA.NewArray(src, typeRef) => apply("newarray", treeify(src), atom(typeRef.name))
        case SSA.MultiANewArray(desc, dims) => apply("multianewarray", atom(desc.name))
        case SSA.PutStatic(state, src, cls, name, desc) => apply("putstatic", treeify(src), atom(cls.name), atom(name), atom(desc.name))
        case SSA.GetStatic(state, cls, name, desc) => apply("getstatic", atom(cls.name), atom(name), atom(desc.name))
        case SSA.PutField(state, src, obj, owner, name, desc) => apply("putfield", treeify(src), treeify(obj), atom(owner.name), atom(name), atom(desc.name))
        case SSA.GetField(state, obj, owner, name, desc) => apply("getfield", treeify(obj), atom(owner.name), atom(name), atom(desc.name))
        case SSA.PutArray(state, src, indexSrc, array) => apply("putarray", treeify(src), treeify(indexSrc), treeify(array))
        case SSA.GetArray(state, indexSrc, array, typeSize) => apply("putarray", treeify(indexSrc), treeify(array))
        case SSA.MonitorEnter(indexSrc) => ???
        case SSA.MonitorExit(indexSrc) => ???
      }
      def treeify(ssa: SSA): pprint.Tree = {
        if (savedLocals.containsKey(ssa)) atom(fansi.Color.Cyan("local" + savedLocals.get(ssa)).toString())
        else treeify0(ssa)
      }
      val body = block.terminalInsns

      out.append(fansi.Color.Magenta("block" + blockIndex))
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
    }

    out.mkString
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
