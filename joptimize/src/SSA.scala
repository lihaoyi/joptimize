package joptimize
import org.objectweb.asm.tree.analysis.Value

import scala.collection.mutable

class Block(val value: mutable.Buffer[SSA], var fallThrough: Option[Block])

sealed abstract class SSA(size: Int, upstream0: SSA*) extends Value{
  def upstream = upstream0
  def getSize = size
  override def equals(obj: Any) = this eq obj.asInstanceOf[AnyRef]

}

object SSA{
  case class Phi(typeSize: Int, inputs: mutable.Buffer[SSA]) extends SSA(typeSize)

  case class Arg(index: Int, typeSize: Int) extends SSA(typeSize)
  case class BinOp(a: SSA, b: SSA, opcode: Int, typeSize: Int) extends SSA(typeSize, a, b)
  case class UnaryOp(a: SSA, opcode: Int, typeSize: Int) extends SSA(typeSize, a)
  case class Inc(a: SSA, increment: Int) extends SSA(1, a)

  case class UnaryBranch(a: SSA, target: Block, opcode: Int) extends SSA(0, a)
  case class BinBranch(a: SSA, b: SSA, target: Block, opcode: Int) extends SSA(0, a, b)

  case class ReturnVal(a: SSA) extends SSA(0, a)
  case class Return() extends SSA(0)
  case class AThrow(src: SSA) extends SSA(0, src)
  case class TableSwitch(src: SSA, min: Int, max: Int, default: Block, targets: Seq[Block]) extends SSA(0, src)
  case class LookupSwitch(src: SSA, default: Block, keys: Seq[Int], tartargets: Seq[Block]) extends SSA(0, src)
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

  case class InvokeStatic(srcs: Seq[SSA],
                          cls: JType.Cls,
                          name: String,
                          desc: Desc) extends SSA(desc.ret.size, srcs:_*)

  case class InvokeSpecial(srcs: Seq[SSA],
                           cls: JType.Cls,
                           name: String,
                           desc: Desc) extends SSA(desc.ret.size, srcs:_*)

  case class InvokeVirtual(srcs: Seq[SSA],
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
  case class PutStatic(src: SSA, cls: JType.Cls, name: String, desc: JType) extends SSA(0, src)
  case class GetStatic(cls: JType.Cls, name: String, desc: JType) extends SSA(desc.size)
  case class PutField(src: SSA, obj: SSA, owner: JType.Cls, name: String, desc: JType) extends SSA(0, src, obj)
  case class GetField(obj: SSA, owner: JType.Cls, name: String, desc: JType) extends SSA(desc.size, obj)
  case class PutArray(src: SSA, indexSrc: SSA, array: SSA) extends SSA(0, src)
  case class GetArray(indexSrc: SSA, array: SSA, typeSize: Int) extends SSA(typeSize, indexSrc, array)

  case class MonitorEnter(indexSrc: SSA) extends SSA(0, indexSrc)
  case class MonitorExit(indexSrc: SSA) extends SSA(0, indexSrc)
}
