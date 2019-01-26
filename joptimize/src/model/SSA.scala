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
        n.state = swap(n.state)
        n.indexSrc = swap(n.indexSrc)
      case n: SSA.GetArray =>
        n.indexSrc = swap(n.indexSrc)
        n.state = swap(n.state)
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
        n.state = swap(n.state)
        n.block = swap(n.block)
        n.a = swap(n.a)
      case n: SSA.Return =>
        n.state = swap(n.state)
        n.block = swap(n.block)
      case n: SSA.AThrow =>
        n.state = swap(n.state)
        n.block = swap(n.block)
        n.src = swap(n.src)
      case n: SSA.TableSwitch => n.src = swap(n.src)
      case n: SSA.LookupSwitch => n.src = swap(n.src)
    }

  }
  trait Node{
    def checkLinks() = {
      val brokenUps = upstream.filter(!_.downstreamContains(this))
      val brokenDowns = downstream.keys.filter(!_.upstream.contains(this))
      assert(brokenUps.isEmpty, (this, brokenUps))
      assert(brokenDowns.isEmpty, (this, brokenDowns))
    }
    def upstream: Seq[Node]
    def upstreamVals: Seq[Val] = upstream.collect{case v: Val => v}
    private[this] val downstream = mutable.LinkedHashMap.empty[Node, Int]
    def downstreamAdd(n: Node) = downstream(n) = downstream.getOrElse(n, 0) + 1
    def downstreamContains(n: Node) = downstream.contains(n)
    def downstreamRemove(n: Node) = downstream.get(n) match{
      case None => // do nothing
      case Some(1) => downstream.remove(n)
      case Some(x) if x > 1 => downstream(n) = x - 1
    }
    def downstreamRemoveAll(n: Node) = downstream.get(n) match{
      case None => // do nothing
      case Some(_) => downstream.remove(n)
    }

    def downstreamList = downstream.keysIterator.toArray
    def downstreamSize = downstream.valuesIterator.sum

    def update(): Node = {
      upstream.filter(_ != null).foreach(_.downstreamAdd(this))
      this
    }

    override def hashCode = System.identityHashCode(this)
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
    update()
  }

  sealed abstract class Val(val jtype: JType) extends org.objectweb.asm.tree.analysis.Value with Node{
    def getSize = jtype.size
    def internalName = toString
    override def update(): Val = {
      super.update()
      this
    }
  }

  sealed trait State extends Val
  class ChangedState(parent: Node) extends Val(JType.Prim.V) with State{
    override def upstream = Option(parent).toSeq
  }

  sealed abstract class Control() extends Node{
    def controls: Seq[Control]
    override def update(): Control = {
      super.update()
      this
    }
  }
  sealed abstract class Block() extends Control()
  sealed abstract class Jump() extends Control(){
    def controls = Seq(block)
    def block: SSA.Block
  }
  sealed abstract class SimpleBlock() extends Block(){

    def block: SSA.Block
  }

  trait Codes{
    private[this] val lookup0 = mutable.LinkedHashMap.empty[Int, Code]
    class Code private[SSA] (val i: Int, val tpe: JType = JType.Prim.V)(implicit name: sourcecode.Name){
      lookup0(i) = this
      override def toString = name.value
    }
    def lookup(i: Int) = lookup0(i)
  }

  class Phi(var block: Block, var incoming: Set[(SSA.Block, SSA.Val)], var tpe: JType) extends Val(tpe) with State{
    override def upstream: Seq[SSA.Node] = Seq(block) ++ incoming.flatMap(x => Seq(x._1, x._2)).toArray[SSA.Node]
    override def toString = s"Phi@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
  }

  class Merge(var insnIndex: Int, var incoming: Set[Control]) extends Block() {
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
  case class Arg(var index: Int, var tpe: JType) extends Val(tpe){
    def upstream = Nil
  }
  case class BinOp(var a: Val, var b: Val, var opcode: BinOp.Code) extends Val(opcode.tpe){
    def upstream = Seq(a, b)
  }
  object BinOp extends Codes{
    val IADD = new Code(Opcodes.IADD, JType.Prim.I)
    val ISUB = new Code(Opcodes.ISUB, JType.Prim.I)
    val IMUL = new Code(Opcodes.IMUL, JType.Prim.I)
    val IDIV = new Code(Opcodes.IDIV, JType.Prim.I)
    val IREM = new Code(Opcodes.IREM, JType.Prim.I)
    val ISHL = new Code(Opcodes.ISHL, JType.Prim.I)
    val ISHR = new Code(Opcodes.ISHR, JType.Prim.I)
    val IUSHR = new Code(Opcodes.IUSHR, JType.Prim.I)
    val IAND = new Code(Opcodes.IAND, JType.Prim.I)
    val IOR = new Code(Opcodes.IOR, JType.Prim.I)
    val IXOR = new Code(Opcodes.IXOR, JType.Prim.I)
    val FADD = new Code(Opcodes.FADD, JType.Prim.F)
    val FSUB = new Code(Opcodes.FSUB, JType.Prim.F)
    val FMUL = new Code(Opcodes.FMUL, JType.Prim.F)
    val FDIV = new Code(Opcodes.FDIV, JType.Prim.F)
    val FREM = new Code(Opcodes.FREM, JType.Prim.F)
    val LCMP = new Code(Opcodes.LCMP, JType.Prim.I)
    val FCMPL = new Code(Opcodes.FCMPL, JType.Prim.I)
    val FCMPG = new Code(Opcodes.FCMPG, JType.Prim.I)
    val DCMPL = new Code(Opcodes.DCMPL, JType.Prim.I)
    val DCMPG = new Code(Opcodes.DCMPG, JType.Prim.I)
    val LADD = new Code(Opcodes.LADD, JType.Prim.J)
    val LSUB = new Code(Opcodes.LSUB, JType.Prim.J)
    val LMUL = new Code(Opcodes.LMUL, JType.Prim.J)
    val LDIV = new Code(Opcodes.LDIV, JType.Prim.J)
    val LREM = new Code(Opcodes.LREM, JType.Prim.J)
    val LSHL = new Code(Opcodes.LSHL, JType.Prim.J)
    val LSHR = new Code(Opcodes.LSHR, JType.Prim.J)
    val LUSHR = new Code(Opcodes.LUSHR, JType.Prim.J)
    val LAND = new Code(Opcodes.LAND, JType.Prim.J)
    val LOR = new Code(Opcodes.LOR, JType.Prim.J)
    val LXOR = new Code(Opcodes.LXOR, JType.Prim.J)
    val DADD = new Code(Opcodes.DADD, JType.Prim.D)
    val DSUB = new Code(Opcodes.DSUB, JType.Prim.D)
    val DMUL = new Code(Opcodes.DMUL, JType.Prim.D)
    val DDIV = new Code(Opcodes.DDIV, JType.Prim.D)
    val DREM = new Code(Opcodes.DREM, JType.Prim.D)
  }
  case class UnaOp(var a: Val, var opcode: UnaOp.Code) extends Val(opcode.tpe){
    def upstream = Seq(a)
  }
  object UnaOp extends Codes{
    val INEG = new Code(Opcodes.INEG, JType.Prim.I)
    val L2I = new Code(Opcodes.L2I, JType.Prim.I)
    val F2I = new Code(Opcodes.F2I, JType.Prim.I)
    val D2I = new Code(Opcodes.D2I, JType.Prim.I)
    val I2B = new Code(Opcodes.I2B, JType.Prim.B)
    val I2C = new Code(Opcodes.I2C, JType.Prim.C)
    val I2S = new Code(Opcodes.I2S, JType.Prim.S)
    val FNEG = new Code(Opcodes.FNEG, JType.Prim.F)
    val I2F = new Code(Opcodes.I2F, JType.Prim.F)
    val L2F = new Code(Opcodes.L2F, JType.Prim.F)
    val D2F = new Code(Opcodes.D2F, JType.Prim.F)
    val LNEG = new Code(Opcodes.LNEG, JType.Prim.J)
    val I2L = new Code(Opcodes.I2L, JType.Prim.J)
    val F2L = new Code(Opcodes.F2L, JType.Prim.J)
    val D2L = new Code(Opcodes.D2L, JType.Prim.J)
    val DNEG = new Code(Opcodes.DNEG, JType.Prim.D)
    val I2D = new Code(Opcodes.I2D, JType.Prim.D)
    val L2D = new Code(Opcodes.L2D, JType.Prim.D)
    val F2D = new Code(Opcodes.F2D, JType.Prim.D)
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
  case class ReturnVal(var state: Val, var block: Block, var a: Val) extends Jump(){
    def upstream = Seq(state, block, a)
    override def upstreamVals = Seq(a)
  }
  case class Return(var state: Val, var block: Block) extends Jump(){
    def upstream = Seq(state, block)
    override def upstreamVals = Seq()
  }
  case class AThrow(var state: Val, var block: Block, var src: Val) extends Jump(){
    def upstream = Seq(state, block, src)
    override def upstreamVals = Seq(src)
  }
  case class TableSwitch(var state: Val, var block: Block, var src: Val, min: Int, max: Int) extends Jump(){
    def upstream = Seq(state, block, src)
    override def upstreamVals = Seq(src)
  }
  case class LookupSwitch(var state: Val, var block: Block, var src: Val, var keys: Seq[Int]) extends Jump(){
    def upstream = Seq(state, block, src)
    override def upstreamVals = Seq(src)
  }
  case class Copy(var src: Val) extends Val(src.jtype){
    def upstream = Seq(src)
  }
  case class CheckCast(var src: Val, var desc: JType) extends Val(desc){
    def upstream = Seq(src)
  }
  case class ArrayLength(var src: Val) extends Val(JType.Prim.I){
    def upstream = Seq(src)
  }
  case class InstanceOf(var src: Val, var desc: JType) extends Val(JType.Prim.Z){
    def upstream = Seq(src)
  }
  case class PushI(var value: Int) extends Val(JType.Prim.I){
    def upstream = Nil
  }
  case class PushJ(var value: Long) extends Val(JType.Prim.J){
    def upstream = Nil
  }
  case class PushF(var value: Float) extends Val(JType.Prim.F){
    def upstream = Nil
  }
  case class PushD(var value: Double) extends Val(JType.Prim.D){
    def upstream = Nil
  }
  case class PushS(var value: String) extends Val(JType.Prim.S){
    def upstream = Nil
  }
  case class PushNull() extends Val(JType.Cls("java/lang/Object")){
    def upstream = Nil
  }
  case class PushCls(var value: JType.Cls) extends Val(value){
    def upstream = Nil
  }

  case class InvokeStatic(var srcs: Seq[Val],
                          var cls: JType.Cls,
                          var name: String,
                          var desc: Desc) extends Val(desc.ret){
    def upstream = srcs
  }

  case class InvokeSpecial(var srcs: Seq[Val],
                           var cls: JType.Cls,
                           var name: String,
                           var desc: Desc) extends Val(desc.ret){
    def upstream = srcs
  }

  case class InvokeVirtual(var srcs: Seq[Val],
                           var cls: JType.Cls,
                           var name: String,
                           var desc: Desc) extends Val(desc.ret){
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

  case class New(var cls: JType.Cls) extends Val(cls){
    def upstream = Nil
  }
  case class NewArray(var src: Val, var typeRef: JType) extends Val(typeRef){
    def upstream = Seq(src)
  }
  case class MultiANewArray(var desc: JType, var dims: Seq[Val]) extends Val(desc){
    def upstream = dims
  }
  case class PutStatic(var state: State, var src: Val, var cls: JType.Cls, var name: String, var desc: JType) extends Val(JType.Prim.V){
    def upstream = Seq(state, src)
  }
  case class GetStatic(var state: State, var cls: JType.Cls, var name: String, var desc: JType) extends Val(desc){
    def upstream = Seq(state)
  }
  case class PutField(var state: State, var src: Val, var obj: Val, var owner: JType.Cls, var name: String, var desc: JType) extends Val(JType.Prim.V){
    def upstream = Seq(state, src, obj)
  }
  case class GetField(var state: State, var obj: Val, var owner: JType.Cls, var name: String, var desc: JType) extends Val(desc){
    def upstream = Seq(state, obj)
  }
  case class PutArray(var state: State, var arrayValue: Val, var indexSrc: Val, var src: Val) extends Val(JType.Prim.V) {
    def upstream = Seq(state, arrayValue, indexSrc, src)
    override def upstreamVals = Seq(arrayValue, indexSrc, src)
  }
  case class GetArray(var state: State, var array: Val, var indexSrc: Val, var tpe: JType) extends Val(tpe) {
    def upstream = Seq(state, array, indexSrc)
    override def upstreamVals = Seq(array, indexSrc)
  }

  case class MonitorEnter(var indexSrc: Val) extends Val(JType.Prim.V){
    def upstream = Seq(indexSrc)
  }
  case class MonitorExit(var indexSrc: Val) extends Val(JType.Prim.V){
    def upstream = Seq(indexSrc)
  }
}
