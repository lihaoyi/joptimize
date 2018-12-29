package joptimize
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.analysis.Value

import scala.collection.mutable

class Block(val value: mutable.Buffer[SSA], var fallThrough: Option[Block])

sealed abstract class SSA(size: Int, upstream0: SSA*) extends Frame.Value{
  def upstream = upstream0
  def getSize = size

  def internalName = toString
}

object SSA{
  trait Codes{
    private[this] val lookup0 = mutable.Map.empty[Int, Code]
    class Code private[SSA] (val i: Int, val typeSize: Int = 0)(implicit name: sourcecode.Name){
      lookup0(i) = this
      override def toString = name.value
    }
    def lookup(i: Int) = lookup0(i)
  }
  case class State(base: Option[(State, SSA)]) extends SSA(0)
  case class Arg(index: Int, tpe: IType) extends SSA(tpe.size)
  case class BinOp(a: SSA, b: SSA, opcode: BinOp.Code) extends SSA(opcode.typeSize, a, b)
  object BinOp extends Codes{
    val IADD = new Code(Opcodes.IADD, 1)
    val ISUB = new Code(Opcodes.ISUB, 1)
    val IMUL = new Code(Opcodes.IMUL, 1)
    val IDIV = new Code(Opcodes.IDIV, 1)
    val IREM = new Code(Opcodes.IREM, 1)
    val ISHL = new Code(Opcodes.ISHL, 1)
    val ISHR = new Code(Opcodes.ISHR, 1)
    val IUSHR = new Code(Opcodes.IUSHR, 1)
    val IAND = new Code(Opcodes.IAND, 1)
    val IOR = new Code(Opcodes.IOR, 1)
    val IXOR = new Code(Opcodes.IXOR, 1)
    val FADD = new Code(Opcodes.FADD, 1)
    val FSUB = new Code(Opcodes.FSUB, 1)
    val FMUL = new Code(Opcodes.FMUL, 1)
    val FDIV = new Code(Opcodes.FDIV, 1)
    val FREM = new Code(Opcodes.FREM, 1)
    val LCMP = new Code(Opcodes.LCMP, 1)
    val FCMPL = new Code(Opcodes.FCMPL, 1)
    val FCMPG = new Code(Opcodes.FCMPG, 1)
    val DCMPL = new Code(Opcodes.DCMPL, 1)
    val DCMPG = new Code(Opcodes.DCMPG, 1)
    val LADD = new Code(Opcodes.LADD, 2)
    val LSUB = new Code(Opcodes.LSUB, 2)
    val LMUL = new Code(Opcodes.LMUL, 2)
    val LDIV = new Code(Opcodes.LDIV, 2)
    val LREM = new Code(Opcodes.LREM, 2)
    val LSHL = new Code(Opcodes.LSHL, 2)
    val LSHR = new Code(Opcodes.LSHR, 2)
    val LUSHR = new Code(Opcodes.LUSHR, 2)
    val LAND = new Code(Opcodes.LAND, 2)
    val LOR = new Code(Opcodes.LOR, 2)
    val LXOR = new Code(Opcodes.LXOR, 2)
    val DADD = new Code(Opcodes.DADD, 2)
    val DSUB = new Code(Opcodes.DSUB, 2)
    val DMUL = new Code(Opcodes.DMUL, 2)
    val DDIV = new Code(Opcodes.DDIV, 2)
    val DREM = new Code(Opcodes.DREM, 2)
  }
  case class UnaryOp(a: SSA, opcode: UnaryOp.Code) extends SSA(opcode.typeSize, a)
  object UnaryOp extends Codes{
    val INEG = new Code(Opcodes.INEG, 1)
    val L2I = new Code(Opcodes.L2I, 1)
    val F2I = new Code(Opcodes.F2I, 1)
    val D2I = new Code(Opcodes.D2I, 1)
    val I2B = new Code(Opcodes.I2B, 1)
    val I2C = new Code(Opcodes.I2C, 1)
    val I2S = new Code(Opcodes.I2S, 1)
    val FNEG = new Code(Opcodes.FNEG, 1)
    val I2F = new Code(Opcodes.I2F, 1)
    val L2F = new Code(Opcodes.L2F, 1)
    val D2F = new Code(Opcodes.D2F, 1)
    val LNEG = new Code(Opcodes.LNEG, 2)
    val I2L = new Code(Opcodes.I2L, 2)
    val F2L = new Code(Opcodes.F2L, 2)
    val D2L = new Code(Opcodes.D2L, 2)
    val DNEG = new Code(Opcodes.DNEG, 2)
    val I2D = new Code(Opcodes.I2D, 2)
    val L2D = new Code(Opcodes.L2D, 2)
    val F2D = new Code(Opcodes.F2D, 2)
  }

  case class UnaryBranch(a: SSA, target: Block, opcode: UnaryBranch.Code) extends SSA(0, a)
  object UnaryBranch  extends Codes{
    val IFEQ = new Code(Opcodes.IFEQ)
    val IFNE = new Code(Opcodes.IFNE)
    val IFLT = new Code(Opcodes.IFLT)
    val IFGE = new Code(Opcodes.IFGE)
    val IFGT = new Code(Opcodes.IFGT)
    val IFLE = new Code(Opcodes.IFLE)
    val IFNULL = new Code(Opcodes.IFNULL)
    val IFNONNULL = new Code(Opcodes.IFNONNULL)
  }
  case class BinBranch(a: SSA, b: SSA, target: Block, opcode: BinBranch.Code) extends SSA(0, a, b)

  object BinBranch  extends Codes{
    val IF_ICMPEQ = new Code(Opcodes.IF_ICMPEQ)
    val IF_ICMPNE = new Code(Opcodes.IF_ICMPNE)
    val IF_ICMPLT = new Code(Opcodes.IF_ICMPLT)
    val IF_ICMPGE = new Code(Opcodes.IF_ICMPGE)
    val IF_ICMPGT = new Code(Opcodes.IF_ICMPGT)
    val IF_ICMPLE = new Code(Opcodes.IF_ICMPLE)
    val IF_ACMPEQ = new Code(Opcodes.IF_ACMPEQ)
    val IF_ACMPNE = new Code(Opcodes.IF_ACMPNE)
  }
  case class ReturnVal(a: SSA) extends SSA(0, a)
  case class Return() extends SSA(0)
  case class AThrow(src: SSA) extends SSA(0, src)
  case class TableSwitch(src: SSA, min: Int, max: Int, default: Block, targets: Seq[Block]) extends SSA(0, src)
  case class LookupSwitch(src: SSA, default: Block, keys: Seq[Int], targets: Seq[Block]) extends SSA(0, src)
  case class Goto(target: Block) extends SSA(0)

  case class CheckCast(src: SSA, desc: JType) extends SSA(0, src)
  case class ArrayLength(src: SSA) extends SSA(1, src)
  case class InstanceOf(src: SSA, desc: JType) extends SSA(1, src)
  case class PushI(value: Int) extends SSA(1)
  case class PushJ(value: Long) extends SSA(2)
  case class PushF(value: Float) extends SSA(1)
  case class PushD(value: Double) extends SSA(2)
  case class PushS(value: String) extends SSA(1)
  case class PushNull() extends SSA(1)
  case class PushCls(value: JType.Cls) extends SSA(1)

  case class InvokeStatic(state: State,
                          srcs: Seq[SSA],
                          cls: JType.Cls,
                          name: String,
                          desc: Desc) extends SSA(desc.ret.size, srcs:_*)

  case class InvokeSpecial(state: State,
                           srcs: Seq[SSA],
                           cls: JType.Cls,
                           name: String,
                           desc: Desc) extends SSA(desc.ret.size, srcs:_*)

  case class InvokeVirtual(state: State,
                           srcs: Seq[SSA],
                           cls: JType.Cls,
                           name: String,
                           desc:Desc) extends SSA(desc.ret.size, srcs:_*)

  case class InvokeDynamic(name: String,
                           desc: String,
                           bsTag: Int,
                           bsOwner: String,
                           bsName: String,
                           bsDesc: String,
                           bsArgs: Seq[AnyRef]) extends SSA(???)

  case class New(cls: JType.Cls) extends SSA(1)
  case class NewArray(src: SSA, typeRef: JType) extends SSA(1, src)
  case class MultiANewArray(desc: JType, dims: Seq[SSA]) extends SSA(1)
  case class PutStatic(state: State, src: SSA, cls: JType.Cls, name: String, desc: JType) extends SSA(0, src)
  case class GetStatic(state: State, cls: JType.Cls, name: String, desc: JType) extends SSA(desc.size)
  case class PutField(state: State, src: SSA, obj: SSA, owner: JType.Cls, name: String, desc: JType) extends SSA(0, src, obj)
  case class GetField(state: State, obj: SSA, owner: JType.Cls, name: String, desc: JType) extends SSA(desc.size, obj)
  case class PutArray(state: State, src: SSA, indexSrc: SSA, array: SSA) extends SSA(0, src)
  case class GetArray(state: State, indexSrc: SSA, array: SSA, typeSize: Int) extends SSA(typeSize, indexSrc, array)

  case class MonitorEnter(indexSrc: SSA) extends SSA(0, indexSrc)
  case class MonitorExit(indexSrc: SSA) extends SSA(0, indexSrc)
}
