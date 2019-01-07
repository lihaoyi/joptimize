package joptimize.model
import org.objectweb.asm.Opcodes

import scala.collection.immutable.SortedSet
import scala.collection.mutable


object SSA{

  def update(down: SSA.Node, self: SSA.Node, other: SSA.Node) = {
    def swap[T <: Node](x: T): T = if (x == self) other.asInstanceOf[T] else x
    down match{
      case phi: SSA.Phi =>
        phi.control = swap(phi.control)
        phi.incoming = phi.incoming.map{case (k, v) => (swap(k), swap(v))}
      case n @ SSA.Arg(index, typeSize) =>
      case n @ SSA.BinOp(a, b, opcode) =>
        n.a = swap(a)
        n.b = swap(b)
      case n @ SSA.UnaOp(a, opcode) => n.a = swap(a)
      case n @ SSA.CheckCast(src, desc) => n.src = swap(src)
      case n @ SSA.ArrayLength(src) => n.src = swap(src)
      case n @ SSA.InstanceOf(src, desc) => n.src = swap(src)
      case n @ SSA.PushI(value) =>
      case n @ SSA.PushJ(value) =>
      case n @ SSA.PushF(value) =>
      case n @ SSA.PushD(value) =>
      case n @ SSA.PushS(value) =>
      case n @ SSA.PushNull() =>
      case n @ SSA.PushCls(value) =>
      case n @ SSA.InvokeStatic(srcs, cls, name, desc) => n.srcs = srcs.map(swap)
      case n @ SSA.InvokeSpecial(srcs, cls, name, desc) => n.srcs = srcs.map(swap)
      case n @ SSA.InvokeVirtual(srcs, cls, name, desc) => n.srcs = srcs.map(swap)
      case n @ SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs) => ???
      case n @ SSA.NewArray(src, typeRef) => n.src = swap(src)
      case n @ SSA.MultiANewArray(desc, dims) =>
      case n @ SSA.PutStatic(src, cls, name, desc) => n.src = swap(src)
      case n @ SSA.GetStatic(cls, name, desc) =>
      case n @ SSA.PutField(src, obj, owner, name, desc) =>
        n.src = swap(src)
        n.obj = swap(obj)
      case n @ SSA.GetField(obj, owner, name, desc) => n.obj = swap(obj)
      case n @ SSA.PutArray(src, indexSrc, array) =>
        n.src = swap(src)
        n.indexSrc = swap(indexSrc)
      case n @ SSA.GetArray(indexSrc, array, typeSize) =>
        n.indexSrc = swap(indexSrc)
        n.array = swap(array)
      case n @ SSA.MonitorEnter(indexSrc) => n.indexSrc = swap(indexSrc)
      case n @ SSA.MonitorExit(indexSrc) => n.indexSrc = swap(indexSrc)
      case r: SSA.Region =>
        r.incoming = r.incoming.map(swap)
      case n: SSA.True => n.node = swap(n.node)
      case n: SSA.False => n.node = swap(n.node)
      case n @ SSA.UnaBranch(control, a, opcode) =>
        n.control = swap(control)
        n.a = swap(a)
      case n @ SSA.BinBranch(control, a, b, opcode) =>
        n.control = swap(control)
        n.a = swap(a)
        n.b = swap(b)
      case n @ SSA.ReturnVal(control, a) =>
        n.control = swap(control)
        n.a = swap(a)
      case n @ SSA.Return(control) => n.control = swap(control)
      case n @ SSA.AThrow(src) => n.src = swap(src)
      case n @ SSA.TableSwitch(src, min, max) => n.src = swap(src)
      case n @ SSA.LookupSwitch(src, keys) => n.src = swap(src)
    }

  }
  trait Node{
    def upstream: Seq[Node]
    val downstream = mutable.Set.empty[Node]
    lazy val upstreamVals = upstream.collect{case s: Val => s}
    lazy val upstreamCtrls = upstream.collect{case s: Ctrl => s}

    def replaceWith(other: Node) = {
      for(up <- upstream){
        up.downstream.remove(this)
        up.downstream.add(other)
      }

      for(down <- downstream) SSA.update(down, this, other)
    }
    def update() = {
      upstream.foreach(_.downstream.add(this))
    }
    update()
  }

  sealed abstract class Val(size: Int) extends org.objectweb.asm.tree.analysis.Value with Node{
    def getSize = size
    def internalName = toString
  }
  sealed abstract class Ctrl() extends Node
  trait Codes{
    private[this] val lookup0 = mutable.Map.empty[Int, Code]
    class Code private[SSA] (val i: Int, val typeSize: Int = 0)(implicit name: sourcecode.Name){
      lookup0(i) = this
      override def toString = name.value
    }
    def lookup(i: Int) = lookup0(i)
  }

  class Phi(var control: Ctrl, var incoming: Set[(SSA.Ctrl, SSA.Val)], var typeSize: Int) extends Val(typeSize){
    override def upstream: Seq[SSA.Node] = Seq(control) ++ incoming.flatMap(x => Seq(x._1, x._2)).toArray[SSA.Node]
    override def toString = s"Phi@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
  }

  class Region(var incoming: Set[Ctrl]) extends Ctrl(){
    def upstream = incoming.toSeq

    override def toString = s"Region@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
  }
  class True(var node: Ctrl) extends Ctrl(){
    def upstream = Seq(node)
  }
  class False(var node: Ctrl) extends Ctrl(){
    def upstream = Seq(node)
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

  case class UnaBranch(var control: Ctrl, var a: Val, var opcode: UnaBranch.Code) extends Ctrl(){
    def upstream = Seq(control, a)
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
  case class BinBranch(var control: Ctrl, var a: Val, var b: Val, var opcode: BinBranch.Code) extends Ctrl(){
    def upstream = Seq(control, a, b)
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
  case class ReturnVal(var control: Ctrl, var a: Val) extends Ctrl(){
    def upstream = Seq(control, a)
  }
  case class Return(var control: Ctrl) extends Ctrl(){
    def upstream = Seq(control)
  }
  case class AThrow(var src: Val) extends Ctrl(){
    def upstream = Seq(src)
  }
  case class TableSwitch(var src: Val, min: Int, max: Int) extends Ctrl(){
    def upstream = Seq(src)
  }
  case class LookupSwitch(var src: Val, var keys: Seq[Int]) extends Ctrl(){
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
