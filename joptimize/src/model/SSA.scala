package joptimize.model
import org.objectweb.asm.{Handle, Opcodes}

import scala.collection.immutable.SortedSet
import scala.collection.mutable


object SSA{
  class Swapper(self: Node, other: Node){
    def apply[T <: Node](value: T): T =
      if (value == self) other.asInstanceOf[T] else value
  }
  trait Node{
    def replaceUpstream(swap: Swapper): Unit
    def replaceUpstream(self: SSA.Node, other: SSA.Node): Unit = replaceUpstream(new Swapper(self, other))


    def checkLinks() = {
      val brokenUps = upstream.filter(!_.downstreamContains(this))
      val brokenDowns = downstream.keys.filter(!_.upstream.contains(this))
      assert(brokenUps.isEmpty, s"Unreciprocated upstream edges: $this <-> ${brokenUps.mkString(", ")}")
      assert(brokenDowns.isEmpty, s"Unreciprocated downstream edges: $this <-> ${brokenDowns.mkString(", ")}")
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
    override def toString(): String = s"${getClass.getSimpleName}@${Integer.toHexString(hashCode())}"
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
  class ChangedState(var parent: Node) extends Val(JType.Prim.V) with State{
    override def upstream = Seq(parent)
    def replaceUpstream(swap: Swapper): Unit = {
      parent = swap(parent)
    }
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

  class Phi(var block: Block, var incoming: Set[(SSA.Block, SSA.Val)], var tpe: JType) extends Val(tpe) with SSA.State{
    override def upstream: Seq[SSA.Node] = Seq(block) ++ incoming.flatMap(x => Seq(x._1, x._2)).toArray[SSA.Node]
    override def toString = s"Phi@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
    def replaceUpstream(swap: Swapper): Unit = {
      block = swap(block)
      incoming = incoming.map(x => (swap(x._1), swap(x._2)))
    }
    override def checkLinks() = {
      super.checkLinks()
//      val phiIncomingBlocks = incoming.map(_._1)
//      val blockIncomingBlocks = block.upstream.toSet
//      assert(
//        phiIncomingBlocks == blockIncomingBlocks,
//        s"Phi node $this incoming blocks doesn't match block $block incoming blocks, $phiIncomingBlocks != $blockIncomingBlocks"
//      )
    }
  }

  class Merge(var insnIndex: Int, var incoming: Set[Block]) extends Block() {
    def controls = upstream
    def upstream = incoming.toSeq

    override def toString = s"Merge@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
    def replaceUpstream(swap: Swapper): Unit = {
      incoming = incoming.map(swap(_))
    }
  }
  case class True(var branch: Jump) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
    def replaceUpstream(swap: Swapper): Unit = {
      branch = swap(branch)
    }
  }
  case class False(var branch: Jump) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
    def replaceUpstream(swap: Swapper): Unit = {
      branch = swap(branch)
    }
  }
  case class Arg(var index: Int, var tpe: JType) extends Val(tpe){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  case class BinOp(var stateOpt: Option[State], var a: Val, var b: Val, var opcode: BinOp.Code) extends Val(opcode.tpe){
    def upstream = stateOpt.toSeq ++ Seq(a, b)
    def replaceUpstream(swap: Swapper): Unit = {
      stateOpt = stateOpt.map(swap(_))
      a = swap(a)
      b = swap(b)
    }
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
    def replaceUpstream(swap: Swapper): Unit = {
      a = swap(a)
    }
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
    def replaceUpstream(swap: Swapper): Unit = {
      block = swap(block)
      a = swap(a)
    }
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
    def replaceUpstream(swap: Swapper): Unit = {
      block = swap(block)
      a = swap(a)
      b = swap(b)
    }
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
  case class ReturnVal(var state: Val, var block: Block, var src: Val) extends Jump(){
    def upstream = Seq(state, block, src)
    override def upstreamVals = Seq(src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
      src = swap(src)
    }
  }
  case class Return(var state: Val, var block: Block) extends Jump(){
    def upstream = Seq(state, block)
    override def upstreamVals = Seq()
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
    }
  }
  case class AThrow(var state: Val, var block: Block, var src: Val) extends Jump(){
    def upstream = Seq(state, block, src)
    override def upstreamVals = Seq(src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
      src = swap(src)
    }
  }
  case class TableSwitch(var block: Block, var src: Val, min: Int, max: Int) extends Jump(){
    def upstream = Seq(block, src)
    override def upstreamVals = Seq(src)
    def replaceUpstream(swap: Swapper): Unit = {
      block = swap(block)
      src = swap(src)
    }
  }
  case class LookupSwitch(var block: Block, var src: Val, var keys: Seq[Int]) extends Jump(){
    def upstream = Seq(block, src)
    override def upstreamVals = Seq(src)
    def replaceUpstream(swap: Swapper): Unit = {
      block = swap(block)
      src = swap(src)
    }
  }

  case class Case(var branch: Jump, var n: Int) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
    def replaceUpstream(swap: Swapper): Unit = {
      branch = swap(branch)
    }
  }
  case class Default(var branch: Jump) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
    def replaceUpstream(swap: Swapper): Unit = {
      branch = swap(branch)
    }
  }
  case class Copy(var src: Val) extends Val(src.jtype){
    def upstream = Seq(src)
    def replaceUpstream(swap: Swapper): Unit = {
      src = swap(src)
    }
  }
  case class CheckCast(var state: State, var src: Val, var desc: JType) extends Val(desc){
    def upstream = Seq(state, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      src = swap(src)
    }
  }
  case class ArrayLength(var state: State, var src: Val) extends Val(JType.Prim.I){
    def upstream = Seq(state, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      src = swap(src)
    }
  }
  case class InstanceOf(var src: Val, var desc: JType) extends Val(JType.Prim.Z){
    def upstream = Seq(src)
    def replaceUpstream(swap: Swapper): Unit = {
      src = swap(src)
    }
  }

  case class ConstI(var value: Int) extends Val(JType.Prim.I){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  case class ConstJ(var value: Long) extends Val(JType.Prim.J){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  case class ConstF(var value: Float) extends Val(JType.Prim.F){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  case class ConstD(var value: Double) extends Val(JType.Prim.D){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  case class ConstStr(var value: String) extends Val(JType.Cls("java/lang/String")){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  case class ConstNull() extends Val(JType.Cls("java/lang/Object")){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  case class ConstCls(var value: JType.Cls) extends Val(value){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }

  trait Invoke extends Val{
    def srcs: Seq[Val]
    def srcs_=(v: Seq[Val]): Unit
    def cls: JType.Cls
    def cls_=(v: JType.Cls): Unit
    def name: String
    def name_=(v: String): Unit
    def desc: Desc
    def desc_=(v: Desc): Unit

    def sig = MethodSig(cls, name, desc, this.isInstanceOf[InvokeStatic])
  }
  case class InvokeStatic(var state: State,
                          var srcs: Seq[Val],
                          var cls: JType.Cls,
                          var name: String,
                          var desc: Desc) extends Val(desc.ret) with Invoke{
    def upstream = state +: srcs
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      srcs = srcs.map(swap(_))
    }
  }

  case class InvokeSpecial(var state: State,
                           var srcs: Seq[Val],
                           var cls: JType.Cls,
                           var name: String,
                           var desc: Desc) extends Val(desc.ret) with Invoke{
    def upstream = state +: srcs
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      srcs = srcs.map(swap(_))
    }
  }

  case class InvokeVirtual(var state: State,
                           var srcs: Seq[Val],
                           var cls: JType.Cls,
                           var name: String,
                           var desc: Desc) extends Val(desc.ret) with Invoke{
    def upstream = state +: srcs
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      srcs = srcs.map(swap(_))
    }
  }


  case class InvokeInterface(var state: State,
                             var srcs: Seq[Val],
                             var cls: JType.Cls,
                             var name: String,
                             var desc: Desc) extends Val(desc.ret) with Invoke{
    def upstream = state +: srcs
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      srcs = srcs.map(swap(_))
    }
  }

  case class InvokeDynamic(var name: String,
                           var desc: Desc,
                           var bootstrap: InvokeDynamic.Bootstrap,
                           var bootstrapArgs: Seq[InvokeDynamic.Arg],
                           var srcs: Seq[Val]) extends Val(desc.ret) {
    def upstream = srcs
    def replaceUpstream(swap: Swapper): Unit = {
      srcs = srcs.map(swap(_))
    }
  }
  object InvokeDynamic{
    case class Bootstrap(var tag: Int,
                         var owner: JType.Cls,
                         var name: String,
                         var desc: Desc)
    def bootstrapFromHandle(bsm: Handle) = {
      SSA.InvokeDynamic.Bootstrap(
        bsm.getTag, JType.Cls(bsm.getOwner),
        bsm.getName, Desc.read(bsm.getDesc)
      )
    }
    sealed trait Arg
    case class StringArg(s: String) extends Arg
    case class IntArg(i: Int) extends Arg
    case class LongArg(i: Long) extends Arg
    case class FloatArg(i: Float) extends Arg
    case class DoubleArg(i: Double) extends Arg
    case class ClsArg(i: JType.Cls) extends Arg
    case class HandleArg(cls: JType.Cls, name: String, desc: Desc, tag: Int) extends Arg
    case class MethodArg(i: Desc) extends Arg
    def anyToArg(any: Any): Arg = any match{
      case i: java.lang.String => SSA.InvokeDynamic.StringArg(i)
      case i: java.lang.Integer => SSA.InvokeDynamic.IntArg(i)
      case i: java.lang.Float => SSA.InvokeDynamic.FloatArg(i)
      case i: java.lang.Long => SSA.InvokeDynamic.LongArg(i)
      case i: java.lang.Double => SSA.InvokeDynamic.DoubleArg(i)
      case i: org.objectweb.asm.Type =>
        if (i.getSort == org.objectweb.asm.Type.METHOD){
          SSA.InvokeDynamic.MethodArg(Desc.read(i.getDescriptor))
        }else{
          SSA.InvokeDynamic.ClsArg(JType.Cls(i.getClassName))
        }
      case i: org.objectweb.asm.Handle =>
        SSA.InvokeDynamic.HandleArg(JType.Cls(i.getOwner), i.getName, Desc.read(i.getDesc), i.getTag)
    }
    def argToAny(arg: Arg): AnyRef = arg match{
      case SSA.InvokeDynamic.StringArg(i) => i
      case SSA.InvokeDynamic.IntArg(i) => i.asInstanceOf[AnyRef]
      case SSA.InvokeDynamic.FloatArg(i) => i.asInstanceOf[AnyRef]
      case SSA.InvokeDynamic.LongArg(i) => i.asInstanceOf[AnyRef]
      case SSA.InvokeDynamic.DoubleArg(i) => i.asInstanceOf[AnyRef]
      case SSA.InvokeDynamic.MethodArg(desc) => org.objectweb.asm.Type.getMethodType(desc.unparse)
      case SSA.InvokeDynamic.ClsArg(cls) => org.objectweb.asm.Type.getObjectType(cls.name)
      case SSA.InvokeDynamic.HandleArg(cls, name, desc, tag) => new Handle(tag, cls.name, name, desc.unparse)
    }
  }
  case class New(var cls: JType.Cls) extends Val(cls){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  case class NewArray(var state: State, var size: Val, var typeRef: JType) extends Val(JType.Arr(typeRef)){
    def upstream = Seq(state, size)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      size = swap(size)
    }
  }
  case class MultiANewArray(var state: State, var desc: JType, var dims: Seq[Val]) extends Val(desc){
    def upstream = state +: dims
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      dims = dims.map(swap(_))
    }
  }
  case class PutStatic(var state: State, var src: Val, var cls: JType.Cls, var name: String, var desc: JType) extends Val(JType.Prim.V){
    def upstream = Seq(state, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      src = swap(src)
    }
  }
  case class GetStatic(var state: State, var cls: JType.Cls, var name: String, var desc: JType) extends Val(desc){
    def upstream = Seq(state)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
    }
  }
  case class PutField(var state: State, var src: Val, var obj: Val, var owner: JType.Cls, var name: String, var desc: JType) extends Val(JType.Prim.V){
    def upstream = Seq(state, src, obj)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      src = swap(src)
      obj = swap(obj)
    }
  }
  case class GetField(var state: State, var obj: Val, var owner: JType.Cls, var name: String, var desc: JType) extends Val(desc){
    def upstream = Seq(state, obj)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      obj = swap(obj)
    }
  }
  case class PutArray(var state: State, var array: Val, var indexSrc: Val, var src: Val) extends Val(JType.Prim.V) {
    def upstream = Seq(state, array, indexSrc, src)
    override def upstreamVals = Seq(array, indexSrc, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      array = swap(array)
      indexSrc = swap(indexSrc)
      src = swap(src)
    }
  }
  case class GetArray(var state: State, var array: Val, var indexSrc: Val, var tpe: JType) extends Val(tpe) {
    def upstream = Seq(state, array, indexSrc)
    override def upstreamVals = Seq(array, indexSrc)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      array = swap(array)
      indexSrc = swap(indexSrc)
    }
  }

  case class MonitorEnter(var indexSrc: Val) extends Val(JType.Prim.V){
    def upstream = Seq(indexSrc)
    def replaceUpstream(swap: Swapper): Unit = {
      indexSrc = swap(indexSrc)
    }
  }
  case class MonitorExit(var indexSrc: Val) extends Val(JType.Prim.V){
    def upstream = Seq(indexSrc)
    def replaceUpstream(swap: Swapper): Unit = {
      indexSrc = swap(indexSrc)
    }
  }
}
