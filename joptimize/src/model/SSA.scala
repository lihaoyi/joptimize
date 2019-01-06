package joptimize.model
import org.objectweb.asm.Opcodes

import scala.collection.immutable.SortedSet
import scala.collection.mutable


object SSA{
  sealed abstract class Value(size: Int, upstream0: SSA.Token*) extends org.objectweb.asm.tree.analysis.Value with Token{
    def upstream = upstream0.collect{case s: Value => s}
    def allUpstream = upstream0
    def getSize = size

    def internalName = toString
  }
  trait Token
  trait Control extends Token
  trait Controlled extends Value{
    def control: Control
  }
  trait Codes{
    private[this] val lookup0 = mutable.Map.empty[Int, Code]
    class Code private[SSA] (val i: Int, val typeSize: Int = 0)(implicit name: sourcecode.Name){
      lookup0(i) = this
      override def toString = name.value
    }
    def lookup(i: Int) = lookup0(i)
  }
  case class State(base: Option[(State, Value)]) extends Value(0)
  class Phi(typeSize: Int) extends Value(typeSize)
  class Region() extends Control
  case class True(node: SSA.Controlled) extends Control
  case class False(node: SSA.Controlled) extends Control
  case class Arg(index: Int, tpe: IType) extends Value(tpe.size)
  case class BinOp(a: Value, b: Value, opcode: BinOp.Code) extends Value(opcode.typeSize, a, b)
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
  case class UnaOp(a: Value, opcode: UnaOp.Code) extends Value(opcode.typeSize, a)
  object UnaOp extends Codes{
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

  case class UnaBranch(control: Control, a: Value, opcode: UnaBranch.Code) extends Value(0, control, a) with Controlled
  object UnaBranch  extends Codes{
    val IFEQ = new Code(Opcodes.IFEQ)
    val IFNE = new Code(Opcodes.IFNE)
    val IFLT = new Code(Opcodes.IFLT)
    val IFGE = new Code(Opcodes.IFGE)
    val IFGT = new Code(Opcodes.IFGT)
    val IFLE = new Code(Opcodes.IFLE)
    val IFNULL = new Code(Opcodes.IFNULL)
    val IFNONNULL = new Code(Opcodes.IFNONNULL)
  }
  case class BinBranch(control: Control, a: Value, b: Value, opcode: BinBranch.Code) extends Value(0, control, a, b) with Controlled

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
  case class ReturnVal(control: Control, a: Value) extends Value(0, control, a) with Controlled
  case class Return(control: Control) extends Value(0, control) with Controlled
  case class AThrow(src: Value) extends Value(0, src)
  case class TableSwitch(src: Value, min: Int, max: Int) extends Value(0, src)
  case class LookupSwitch(src: Value, keys: Seq[Int]) extends Value(0, src)

  case class CheckCast(src: Value, desc: JType) extends Value(0, src)
  case class ArrayLength(src: Value) extends Value(1, src)
  case class InstanceOf(src: Value, desc: JType) extends Value(1, src)
  case class PushI(value: Int) extends Value(1)
  case class PushJ(value: Long) extends Value(2)
  case class PushF(value: Float) extends Value(1)
  case class PushD(value: Double) extends Value(2)
  case class PushS(value: String) extends Value(1)
  case class PushNull() extends Value(1)
  case class PushCls(value: JType.Cls) extends Value(1)

  case class InvokeStatic(srcs: Seq[Value],
                          cls: JType.Cls,
                          name: String,
                          desc: Desc) extends Value(desc.ret.size, srcs:_*)

  case class InvokeSpecial(srcs: Seq[Value],
                           cls: JType.Cls,
                           name: String,
                           desc: Desc) extends Value(desc.ret.size, srcs:_*)

  case class InvokeVirtual(srcs: Seq[Value],
                           cls: JType.Cls,
                           name: String,
                           desc:Desc) extends Value(desc.ret.size, srcs:_*)

  case class InvokeDynamic(name: String,
                           desc: String,
                           bsTag: Int,
                           bsOwner: String,
                           bsName: String,
                           bsDesc: String,
                           bsArgs: Seq[AnyRef]) extends Value(???)

  case class New(cls: JType.Cls) extends Value(1)
  case class NewArray(src: Value, typeRef: JType) extends Value(1, src)
  case class MultiANewArray(desc: JType, dims: Seq[Value]) extends Value(1)
  case class PutStatic(src: Value, cls: JType.Cls, name: String, desc: JType) extends Value(0, src)
  case class GetStatic(cls: JType.Cls, name: String, desc: JType) extends Value(desc.size)
  case class PutField(src: Value, obj: Value, owner: JType.Cls, name: String, desc: JType) extends Value(0, src, obj)
  case class GetField(obj: Value, owner: JType.Cls, name: String, desc: JType) extends Value(desc.size, obj)
  case class PutArray(src: Value, indexSrcValue: Value, arrayValue: Value) extends Value(0, src)
  case class GetArray(indexSrc: Value, array: Value, typeSize: Int) extends Value(typeSize, indexSrc, array)

  case class MonitorEnter(indexSrc: Value) extends Value(0, indexSrc)
  case class MonitorExit(indexSrc: Value) extends Value(0, indexSrc)
}
