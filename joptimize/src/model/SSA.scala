package joptimize.model
import org.objectweb.asm.Opcodes

import scala.collection.immutable.SortedSet
import scala.collection.mutable


object SSA{

  def update(down: SSA.Node, self: SSA.Node, other: SSA.Node) = {

    def swap[T <: Node](x: T): T = {
      if (x == self) other.asInstanceOf[T]
      else x
    }
    down match{
      case n: SSA.Copy => n.src = swap(n.src)
      case phi: SSA.Phi =>
        phi.block = swap(phi.block)
        phi.incoming = phi.incoming.map{case (k, v) => (swap(k), swap(v))}
      case n: SSA.Arg =>
      case n: SSA.BinOp =>
        n.a = swap(n.a)
        n.b = swap(n.b)
      case n: SSA.UnaOp => n.a = swap(n.a)
      case n: SSA.CheckCast => n.src = swap(n.src)
      case n: SSA.ArrayLength => n.src = swap(n.src)
      case n: SSA.InstanceOf => n.src = swap(n.src)
      case n: SSA.PushI =>
      case n: SSA.PushJ =>
      case n: SSA.PushF =>
      case n: SSA.PushD =>
      case n: SSA.PushS =>
      case n: SSA.PushNull =>
      case n: SSA.PushCls =>
      case n: SSA.InvokeStatic => n.srcs = n.srcs.map(swap)
      case n: SSA.InvokeSpecial => n.srcs = n.srcs.map(swap)
      case n: SSA.InvokeVirtual => n.srcs = n.srcs.map(swap)
      case n: SSA.InvokeDynamic => ???
      case n: SSA.NewArray => n.src = swap(n.src)
      case n: SSA.MultiANewArray =>
      case n: SSA.PutStatic => n.src = swap(n.src)
      case n: SSA.GetStatic =>
      case n: SSA.PutField =>
        n.src = swap(n.src)
        n.obj = swap(n.obj)
      case n: SSA.GetField => n.obj = swap(n.obj)
      case n: SSA.PutArray =>
        n.src = swap(n.src)
        n.indexSrc = swap(n.indexSrc)
      case n: SSA.GetArray =>
        n.indexSrc = swap(n.indexSrc)
        n.array = swap(n.array)
      case n: SSA.MonitorEnter => n.indexSrc = swap(n.indexSrc)
      case n: SSA.MonitorExit => n.indexSrc = swap(n.indexSrc)
      case r: SSA.Merge =>
        r.incoming = r.incoming.map(swap)
      case n: SSA.True => n.branch = swap(n.branch)
      case n: SSA.False => n.branch = swap(n.branch)
      case n: SSA.UnaBranch =>
        n.block = swap(n.block)
        n.a = swap(n.a)
      case n: SSA.BinBranch =>
        n.block = swap(n.block)
        n.a = swap(n.a)
        n.b = swap(n.b)
      case n: SSA.ReturnVal =>
        n.block = swap(n.block)
        n.a = swap(n.a)
      case n: SSA.Return => n.block = swap(n.block)
      case n: SSA.AThrow => n.src = swap(n.src)
      case n: SSA.TableSwitch => n.src = swap(n.src)
      case n: SSA.LookupSwitch => n.src = swap(n.src)
    }

  }
  trait Node{
    def checkLinks() = {
      val brokenUps = upstream.filter(!_.downstream.contains(this))
      val brokenDowns = downstream.filter(!_.upstream.contains(this))
      assert(brokenUps.isEmpty, (this, brokenUps))
      assert(brokenDowns.isEmpty, (this, brokenDowns))
    }
    def upstream: Seq[Node]
    val downstream = mutable.LinkedHashSet.empty[Node]

    def replaceWith(other: Node) = {
      for(up <- upstream){
        up.downstream.remove(this)
        up.downstream.add(other)
      }

      for(down <- downstream) SSA.update(down, this, other)
    }
    def update(): Node = {
      upstream.foreach(_.downstream.add(this))
      this
    }

    override def hashCode = System.identityHashCode(this)
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
    update()
  }

  sealed abstract class Val(size: Int) extends org.objectweb.asm.tree.analysis.Value with Node{
    def getSize = size
    def internalName = toString
    override def update(): Val = {
      super.update()
      this
    }

  }
  sealed abstract class Control() extends Node{
    def controls: Seq[Control]
    override def update(): Control = {
      super.update()
      this
    }
  }
  sealed abstract class Block() extends Control(){

  }
  sealed abstract class Jump() extends Control(){
    def controls = Seq(block)
    def block: SSA.Block
  }
  sealed abstract class SimpleBlock() extends Block(){

    def block: SSA.Block
  }
  trait Codes{
    private[this] val lookup0 = mutable.LinkedHashMap.empty[Int, Code]
    class Code private[SSA] (val i: Int, val typeSize: Int = 0)(implicit name: sourcecode.Name){
      lookup0(i) = this
      override def toString = name.value
    }
    def lookup(i: Int) = lookup0(i)
  }

  class Phi(var block: Control, var incoming: Set[(SSA.Block, SSA.Val)], var typeSize: Int) extends Val(typeSize){
    override def upstream: Seq[SSA.Node] = Seq(block) ++ incoming.flatMap(x => Seq(x._1, x._2)).toArray[SSA.Node]
    override def toString = s"Phi@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
  }

  class Merge(var insnIndex: Int, var incoming: Set[Control]) extends Block(){
    def controls = upstream
    def upstream = incoming.toSeq

    override def toString = s"Region@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
  }
  case class True(var branch: Jump) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
  }
  case class False(var branch: Jump) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
  }
  case class Arg(var index: Int, var tpe: IType) extends Val(tpe.size){
    def upstream = Nil
  }
  case class BinOp(var a: Val, var b: Val, var opcode: BinOp.Code) extends Val(opcode.typeSize){
    def upstream = Seq(a, b)
  }
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
  case class UnaOp(var a: Val, var opcode: UnaOp.Code) extends Val(opcode.typeSize){
    def upstream = Seq(a)
  }
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

  case class UnaBranch(var block: Block, var a: Val, var opcode: UnaBranch.Code) extends Jump(){
    def upstream = Seq(block, a)
  }
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
  case class BinBranch(var block: Block, var a: Val, var b: Val, var opcode: BinBranch.Code) extends Jump(){
    def upstream = Seq(block, a, b)
  }

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
  case class ReturnVal(var block: Block, var a: Val) extends Jump(){
    def upstream = Seq(block, a)
  }
  case class Return(var block: Block) extends Jump(){
    def upstream = Seq(block)
  }
  case class AThrow(var block: Block, var src: Val) extends Jump(){
    def upstream = Seq(src)
  }
  case class TableSwitch(var block: Block, var src: Val, min: Int, max: Int) extends Jump(){
    def upstream = Seq(src)
  }
  case class LookupSwitch(var block: Block, var src: Val, var keys: Seq[Int]) extends Jump(){
    def upstream = Seq(src)
  }
  case class Copy(var src: Val) extends Val(src.getSize){
    def upstream = Seq(src)
  }
  case class CheckCast(var src: Val, var desc: JType) extends Val(0){
    def upstream = Seq(src)
  }
  case class ArrayLength(var src: Val) extends Val(1){
    def upstream = Seq(src)
  }
  case class InstanceOf(var src: Val, var desc: JType) extends Val(1){
    def upstream = Seq(src)
  }
  case class PushI(var value: Int) extends Val(1){
    def upstream = Nil
  }
  case class PushJ(var value: Long) extends Val(2){
    def upstream = Nil
  }
  case class PushF(var value: Float) extends Val(1){
    def upstream = Nil
  }
  case class PushD(var value: Double) extends Val(2){
    def upstream = Nil
  }
  case class PushS(var value: String) extends Val(1){
    def upstream = Nil
  }
  case class PushNull() extends Val(1){
    def upstream = Nil
  }
  case class PushCls(var value: JType.Cls) extends Val(1){
    def upstream = Nil
  }

  case class InvokeStatic(var srcs: Seq[Val],
                          var cls: JType.Cls,
                          var name: String,
                          var desc: Desc) extends Val(desc.ret.size){
    def upstream = srcs
  }

  case class InvokeSpecial(var srcs: Seq[Val],
                           var cls: JType.Cls,
                           var name: String,
                           var desc: Desc) extends Val(desc.ret.size){
    def upstream = srcs
  }

  case class InvokeVirtual(var srcs: Seq[Val],
                           var cls: JType.Cls,
                           var name: String,
                           var desc: Desc) extends Val(desc.ret.size){
    def upstream = srcs
  }

  case class InvokeDynamic(var name: String,
                           var desc: String,
                           var bsTag: Int,
                           var bsOwner: String,
                           var bsName: String,
                           var bsDesc: String,
                           var bsArgs: Seq[AnyRef]) extends Val(???){
    def upstream = Nil
  }

  case class New(var cls: JType.Cls) extends Val(1){
    def upstream = Nil
  }
  case class NewArray(var src: Val, var typeRef: JType) extends Val(1){
    def upstream = Seq(src)
  }
  case class MultiANewArray(var desc: JType, var dims: Seq[Val]) extends Val(1){
    def upstream = Nil
  }
  case class PutStatic(var src: Val, var cls: JType.Cls, var name: String, var desc: JType) extends Val(0){
    def upstream = Seq(src)
  }
  case class GetStatic(var cls: JType.Cls, var name: String, var desc: JType) extends Val(desc.size){
    def upstream = Nil
  }
  case class PutField(var src: Val, var obj: Val, var owner: JType.Cls, var name: String, var desc: JType) extends Val(0){
    def upstream = Seq(src, obj)
  }
  case class GetField(var obj: Val, var owner: JType.Cls, var name: String, var desc: JType) extends Val(desc.size){
    def upstream = Seq(obj)
  }
  case class PutArray(var src: Val, var indexSrc: Val, var arrayValue: Val) extends Val(0){
    def upstream = Seq(src)
  }
  case class GetArray(var indexSrc: Val, var array: Val, var typeSize: Int) extends Val(typeSize){
    def upstream = Seq(indexSrc, array)
  }

  case class MonitorEnter(var indexSrc: Val) extends Val(0){
    def upstream = Seq(indexSrc)
  }
  case class MonitorExit(var indexSrc: Val) extends Val(0){
    def upstream = Seq(indexSrc)
  }
}
