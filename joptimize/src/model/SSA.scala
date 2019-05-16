package joptimize.model
import org.objectweb.asm.{Handle, Opcodes}

import scala.collection.immutable.SortedSet
import scala.collection.mutable

object SSA{
  class Swapper(self: Node, other: Node){
    def apply[T <: Node](value: T): T =
      if (value == self) other.asInstanceOf[T] else value
  }
  trait Node extends java.io.Serializable{
    def replaceUpstream(swap: Swapper): Unit
    def replaceUpstream(self: SSA.Node, other: SSA.Node): Unit = replaceUpstream(new Swapper(self, other))


    def checkLinks() = {
      val brokenUps = upstream.filter(!_.downstreamContains(this))
      val brokenDowns = downstreamList.filter(!_.upstream.contains(this))
      assert(brokenUps.isEmpty, s"Unreciprocated upstream edges: $this <-> ${brokenUps.mkString(", ")}")
      assert(brokenDowns.isEmpty, s"Unreciprocated downstream edges: $this <-> ${brokenDowns.mkString(", ")}")
    }
    def upstream: Seq[Node]
    def upstreamVals: Seq[Val] = upstream.collect{case v: Val => v}

    def downstreamContains(n: Node): Boolean = downstreamList.contains(n)

    def downstreamList: Seq[SSA.Node]
    def downstreamSize: Int

    def update(): Node = {
      upstream.filter(_ != null).collect{
        case c: SSA.Val => c.downstreamAdd(this)
        case s: SSA.State => s.next = this
      }
      this
    }

    override def toString(): String = s"${getClass.getSimpleName}@${Integer.toHexString(hashCode())}"
    update()
  }

  sealed trait ValOrState extends Node
  sealed abstract class Val(val jtype: JType) extends org.objectweb.asm.tree.analysis.Value with ValOrState {
    def getSize = jtype.size
    def internalName = toString
    override def update(): Val = {
      super.update()
      this
    }
    private[this] val downstream = mutable.LinkedHashMap.empty[Node, Int]
    override def downstreamContains(n: Node) = downstream.contains(n)
    def downstreamList: Seq[SSA.Node] = downstream.flatMap{case (k, n) => Array.fill(n)(k)}.toArray[SSA.Node]
    def downstreamSize: Int = downstream.valuesIterator.sum
    def downstreamAdd(n: Node) = downstream(n) = downstream.getOrElse(n, 0) + 1
    def downstreamRemove(n: Node) = downstream.get(n) match{
      case None => // do nothing
      case Some(1) => downstream.remove(n)
      case Some(x) if x > 1 => downstream(n) = x - 1
    }
    def downstreamRemoveAll(n: Node) = downstream.get(n) match{
      case None => // do nothing
      case Some(_) => downstream.remove(n)
    }

  }
  sealed trait Stateful extends Node{
    def state: State
    def state_=(s: State): Unit
  }

  class State(var parent: Node) extends ValOrState {
    override def upstream = Seq(parent)
    def replaceUpstream(swap: Swapper): Unit = {
      parent = swap(parent)
    }
    override def update() = {
      parent match{
        case s: SSA.Block => s.nextState = this
        case _ =>
      }
      super.update()
    }

    var next: SSA.Node = null
    def downstreamList = Option(next).toList

    def downstreamSize = downstreamList.size
  }

  sealed abstract class Control() extends Node{
    def controls: Seq[Control]
    override def update(): Control = {
      super.update()
      this
    }
  }
  sealed abstract class Block() extends Control(){
    var nextPhis: Seq[Phi] = Nil
    var nextState: SSA.State = null
    def downstreamList: Seq[Node] = nextPhis ++ Option(nextState)
    def downstreamSize = nextPhis.length + (if (nextState == null) 0 else 1)
    def next: SSA.Control
    def next_=(v: SSA.Control)
  }
  sealed abstract class Jump() extends Control() with Stateful{
    def controls = Seq(block)
    def block: SSA.Block
    override def update(): Control = {
      block.next = this
      super.update()
      this
    }
  }
  sealed abstract class SimpleBlock() extends Block(){

    def next: SSA.Control
    def next_=(v: SSA.Control)
    def block: SSA.Block
    override def downstreamList = super.downstreamList ++ Option(next).toSeq
    override def downstreamSize = super.downstreamSize + (if (next == null) 0 else 1)
  }

  case class Code[T] (i: Int, tpe: JType = JType.Prim.V)
                     (implicit val name: sourcecode.Name) extends java.io.Serializable{

    override def toString = name.value
  }

  trait Codes[T]{
    private[SSA] val lookup0 = mutable.LinkedHashMap.empty[Int, Code]
    type Code = SSA.Code[T]
    def code(i: Int, tpe: JType = JType.Prim.V)(implicit name: sourcecode.Name) = {
      val code = new Code(i, tpe)
      lookup0(i) = code
      code
    }
    def lookup(i: Int) = lookup0(i)
  }

  class Phi(var block: Merge, val incoming: mutable.LinkedHashMap[SSA.Block, SSA.Val], var tpe: JType) extends Val(tpe) {
    override def upstream: Seq[SSA.Node] =
      Seq(block) ++ incoming.toArray[(SSA.Node, SSA.Node)].flatMap(x => Seq(x._1, x._2))
    override def toString = s"Phi@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
    def replaceUpstream(swap: Swapper): Unit = {
      block = swap(block)

      val arr = incoming.toArray
      incoming.clear()
      for((k, v) <- arr) incoming(swap(k)) = swap(v)
    }
    override def checkLinks() = {
      super.checkLinks()
      val phiIncomingBlocks = incoming.map(_._1).toSet
      val blockIncomingBlocks = block.incoming.map(_._1).toSet
      assert(
        phiIncomingBlocks == blockIncomingBlocks,
        s"$this incoming blocks doesn't match block $block incoming blocks, $phiIncomingBlocks != $blockIncomingBlocks"
      )
    }
  }

  class Start(next: SSA.Control) extends Merge(mutable.LinkedHashMap.empty, next, Nil){
    override def toString = s"Start@${Integer.toHexString(System.identityHashCode(this))}"
  }
  class Merge(val incoming: mutable.LinkedHashMap[Block, State],
              var next: SSA.Control,
              var phis: Seq[SSA.Phi]) extends Block() {
    def controls = incoming.toSeq.map(_._1)
    def upstream = incoming.toSeq.flatMap(x => Seq(x._1, x._2))

    override def toString = s"Merge@${Integer.toHexString(System.identityHashCode(this))}(${incoming.size})"
    def replaceUpstream(swap: Swapper): Unit = {
      val arr = incoming.toArray
      incoming.clear()
      for((k, v) <- arr) incoming(swap(k)) = swap(v)
    }
    override def downstreamList = Option(next).toSeq ++ phis ++ super.downstreamList
    override def downstreamSize = 1 + phis.length + super.downstreamSize
  }
  class True(var branch: Jump, var next: SSA.Control) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
    override def update(): Control = {
      branch match{
        case u: UnaBranch => u.trueBranch = this
        case u: BinBranch => u.trueBranch = this
      }
      super.update()
      this
    }
    def replaceUpstream(swap: Swapper): Unit = {
      branch = swap(branch)
    }
  }
  class False(var branch: Jump, var next: SSA.Control) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
    override def update(): Control = {
      branch match{
        case u: UnaBranch => u.falseBranch = this
        case u: BinBranch => u.falseBranch = this
      }
      super.update()
      this
    }
    def replaceUpstream(swap: Swapper): Unit = {
      branch = swap(branch)
    }
  }
  class Arg(var index: Int, var tpe: JType) extends Val(tpe){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
    override def toString = s"${super.toString()}($index, ${tpe.name})"
  }
  class BinOp(var state: State, var a: Val, var b: Val, var opcode: BinOp.Code)
    extends Val(opcode.tpe) with Stateful{
    def upstream = Option(state).toSeq ++ Seq(a, b)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      a = swap(a)
      b = swap(b)
    }
    override def toString = s"${super.toString()}($opcode)"
  }
  object BinOp extends Codes[BinOp]{
    val IADD = code(Opcodes.IADD, JType.Prim.I)
    val ISUB = code(Opcodes.ISUB, JType.Prim.I)
    val IMUL = code(Opcodes.IMUL, JType.Prim.I)
    val IDIV = code(Opcodes.IDIV, JType.Prim.I)
    val IREM = code(Opcodes.IREM, JType.Prim.I)
    val ISHL = code(Opcodes.ISHL, JType.Prim.I)
    val ISHR = code(Opcodes.ISHR, JType.Prim.I)
    val IUSHR = code(Opcodes.IUSHR, JType.Prim.I)
    val IAND = code(Opcodes.IAND, JType.Prim.I)
    val IOR = code(Opcodes.IOR, JType.Prim.I)
    val IXOR = code(Opcodes.IXOR, JType.Prim.I)
    val FADD = code(Opcodes.FADD, JType.Prim.F)
    val FSUB = code(Opcodes.FSUB, JType.Prim.F)
    val FMUL = code(Opcodes.FMUL, JType.Prim.F)
    val FDIV = code(Opcodes.FDIV, JType.Prim.F)
    val FREM = code(Opcodes.FREM, JType.Prim.F)
    val LCMP = code(Opcodes.LCMP, JType.Prim.I)
    val FCMPL = code(Opcodes.FCMPL, JType.Prim.I)
    val FCMPG = code(Opcodes.FCMPG, JType.Prim.I)
    val DCMPL = code(Opcodes.DCMPL, JType.Prim.I)
    val DCMPG = code(Opcodes.DCMPG, JType.Prim.I)
    val LADD = code(Opcodes.LADD, JType.Prim.J)
    val LSUB = code(Opcodes.LSUB, JType.Prim.J)
    val LMUL = code(Opcodes.LMUL, JType.Prim.J)
    val LDIV = code(Opcodes.LDIV, JType.Prim.J)
    val LREM = code(Opcodes.LREM, JType.Prim.J)
    val LSHL = code(Opcodes.LSHL, JType.Prim.J)
    val LSHR = code(Opcodes.LSHR, JType.Prim.J)
    val LUSHR = code(Opcodes.LUSHR, JType.Prim.J)
    val LAND = code(Opcodes.LAND, JType.Prim.J)
    val LOR = code(Opcodes.LOR, JType.Prim.J)
    val LXOR = code(Opcodes.LXOR, JType.Prim.J)
    val DADD = code(Opcodes.DADD, JType.Prim.D)
    val DSUB = code(Opcodes.DSUB, JType.Prim.D)
    val DMUL = code(Opcodes.DMUL, JType.Prim.D)
    val DDIV = code(Opcodes.DDIV, JType.Prim.D)
    val DREM = code(Opcodes.DREM, JType.Prim.D)
  }
  class UnaOp(var a: Val, var opcode: UnaOp.Code) extends Val(opcode.tpe){
    def upstream = Seq(a)
    def replaceUpstream(swap: Swapper): Unit = {
      a = swap(a)
    }
    override def toString = s"${super.toString()}($opcode)"
  }
  object UnaOp extends Codes[UnaOp]{
    val INEG = code(Opcodes.INEG, JType.Prim.I)
    val L2I = code(Opcodes.L2I, JType.Prim.I)
    val F2I = code(Opcodes.F2I, JType.Prim.I)
    val D2I = code(Opcodes.D2I, JType.Prim.I)
    val I2B = code(Opcodes.I2B, JType.Prim.B)
    val I2C = code(Opcodes.I2C, JType.Prim.C)
    val I2S = code(Opcodes.I2S, JType.Prim.S)
    val FNEG = code(Opcodes.FNEG, JType.Prim.F)
    val I2F = code(Opcodes.I2F, JType.Prim.F)
    val L2F = code(Opcodes.L2F, JType.Prim.F)
    val D2F = code(Opcodes.D2F, JType.Prim.F)
    val LNEG = code(Opcodes.LNEG, JType.Prim.J)
    val I2L = code(Opcodes.I2L, JType.Prim.J)
    val F2L = code(Opcodes.F2L, JType.Prim.J)
    val D2L = code(Opcodes.D2L, JType.Prim.J)
    val DNEG = code(Opcodes.DNEG, JType.Prim.D)
    val I2D = code(Opcodes.I2D, JType.Prim.D)
    val L2D = code(Opcodes.L2D, JType.Prim.D)
    val F2D = code(Opcodes.F2D, JType.Prim.D)
  }
  trait Branch extends Jump{
    def trueBranch: SSA.True
    def falseBranch: SSA.False
    def opcode: SSA.Code[_]
  }
  class UnaBranch(var state: State,
                  var block: Block,
                  var a: Val,
                  var opcode: UnaBranch.Code,
                  var trueBranch: SSA.True,
                  var falseBranch: SSA.False) extends Branch{
    def upstream = Seq(state, block, a)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
      a = swap(a)
    }
    def downstreamList = Seq(trueBranch, falseBranch)
    def downstreamSize = 2
    override def toString = s"${super.toString()}($opcode)"
  }
  object UnaBranch  extends Codes[UnaBranch]{
    val IFEQ = code(Opcodes.IFEQ)
    val IFNE = code(Opcodes.IFNE)
    val IFLT = code(Opcodes.IFLT)
    val IFGE = code(Opcodes.IFGE)
    val IFGT = code(Opcodes.IFGT)
    val IFLE = code(Opcodes.IFLE)
    val IFNULL = code(Opcodes.IFNULL)
    val IFNONNULL = code(Opcodes.IFNONNULL)
  }
  class BinBranch(var state: State,
                  var block: Block,
                  var a: Val,
                  var b: Val,
                  var opcode: BinBranch.Code,
                  var trueBranch: SSA.True,
                  var falseBranch: SSA.False) extends Branch{
    def upstream = Seq(state, block, a, b)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
      a = swap(a)
      b = swap(b)
    }
    def downstreamList = Seq(trueBranch, falseBranch)
    def downstreamSize = 2
    override def toString = s"${super.toString()}($opcode)"
  }

  object BinBranch  extends Codes[BinBranch]{
    val IF_ICMPEQ = code(Opcodes.IF_ICMPEQ)
    val IF_ICMPNE = code(Opcodes.IF_ICMPNE)
    val IF_ICMPLT = code(Opcodes.IF_ICMPLT)
    val IF_ICMPGE = code(Opcodes.IF_ICMPGE)
    val IF_ICMPGT = code(Opcodes.IF_ICMPGT)
    val IF_ICMPLE = code(Opcodes.IF_ICMPLE)
    val IF_ACMPEQ = code(Opcodes.IF_ACMPEQ)
    val IF_ACMPNE = code(Opcodes.IF_ACMPNE)
  }
  class ReturnVal(var state: State, var block: Block, var src: Val) extends Jump() with Stateful{
    def upstream = Seq(state, block, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
      src = swap(src)
    }
    def downstreamList = Nil
    def downstreamSize = 0
  }
  class Return(var state: State, var block: Block) extends Jump() with Stateful{
    def upstream = Seq(state, block)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
    }
    def downstreamList = Nil
    def downstreamSize = 0
  }
  class ThrowBlock(var next: Control) extends Block{
    def controls = Nil

    def replaceUpstream(swap: Swapper): Unit = {
      next = swap(next)
    }

    override def upstream = Nil
    override def downstreamList = Seq(next) ++ super.downstreamList
    override def downstreamSize = 1 + super.downstreamSize
  }

  class AThrow(var state: State,
               var block: Block,
               var src: Val,
               var next: Option[SSA.Block]) extends Jump() with Stateful{
    def upstream = Seq(state, block, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
      src = swap(src)
    }
    def downstreamList = next.toList
    def downstreamSize = downstreamList.size
  }
  trait Switch extends Jump{
    def src: SSA.Val
    def default: SSA.Default
    def cases: mutable.LinkedHashMap[Int, SSA.Case]
  }
  class TableSwitch(var state: State,
                    var block: Block,
                    var src: Val,
                    var min: Int,
                    var max: Int,
                    var cases: mutable.LinkedHashMap[Int, SSA.Case],
                    var default: SSA.Default) extends Switch {
    def upstream = Seq(state, block, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
      src = swap(src)
    }
    def downstreamList = cases.values.toSeq ++ Seq(default)
    def downstreamSize = cases.size + 1
  }
  class LookupSwitch(var state: State,
                     var block: Block,
                     var src: Val,
                     var keys: Seq[Int],
                     var cases: mutable.LinkedHashMap[Int, SSA.Case],
                     var default: SSA.Default) extends Switch {
    def upstream = Seq(state, block, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      block = swap(block)
      src = swap(src)
    }
    def downstreamList = cases.values.toSeq ++ Seq(default)
    def downstreamSize = cases.size + 1
  }

  class Case(var branch: Jump, var n: Int, var next: SSA.Control) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
    def replaceUpstream(swap: Swapper): Unit = {
      branch = swap(branch)
    }
    override def toString = s"${super.toString()}($n)"
  }
  class Default(var branch: Jump, var next: SSA.Control) extends SimpleBlock(){
    def controls = Seq(branch)
    def block = branch.block
    def upstream = Seq(branch)
    def replaceUpstream(swap: Swapper): Unit = {
      branch = swap(branch)
    }
  }
  class Copy(var src: Val) extends Val(src.jtype){
    def upstream = Seq(src)
    def replaceUpstream(swap: Swapper): Unit = {
      src = swap(src)
    }
  }
  class CheckCast(var state: State, var src: Val, var desc: JType) extends Val(desc) with Stateful{
    def upstream = Seq(state, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      src = swap(src)
    }
    override def toString = s"${super.toString()}(${desc.name})"
  }
  class ArrayLength(var state: State, var src: Val) extends Val(JType.Prim.I) with Stateful{
    def upstream = Seq(state, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      src = swap(src)
    }
  }
  class InstanceOf(var src: Val, var desc: JType) extends Val(JType.Prim.Z){
    def upstream = Seq(src)
    def replaceUpstream(swap: Swapper): Unit = {
      src = swap(src)
    }
    override def toString = s"${super.toString()}(${desc.name})"
  }

  class ConstI(var value: Int) extends Val(JType.Prim.I){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
    override def toString = s"${super.toString()}($value)"
  }
  class ConstJ(var value: Long) extends Val(JType.Prim.J){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
    override def toString = s"${super.toString()}($value)"
  }
  class ConstF(var value: Float) extends Val(JType.Prim.F){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
    override def toString = s"${super.toString()}($value)"
  }
  class ConstD(var value: Double) extends Val(JType.Prim.D){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
    override def toString = s"${super.toString()}($value)"
  }
  class ConstStr(var value: String) extends Val(JType.Cls("java/lang/String")){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
    override def toString = s"${super.toString()}($value)"
  }
  class ConstNull() extends Val(JType.Cls("java/lang/Object")){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
  }
  class ConstCls(var value: JType.Cls) extends Val(value){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
    override def toString = s"${super.toString()}(${value.name})"
  }

  trait Invoke extends Val with Stateful{
    def srcs: Seq[Val]
    def srcs_=(v: Seq[Val]): Unit
    def cls: JType.Cls
    def cls_=(v: JType.Cls): Unit
    def name: String
    def name_=(v: String): Unit
    def desc: Desc
    def desc_=(v: Desc): Unit

    def sig = MethodSig(cls, name, desc, this.isInstanceOf[InvokeStatic])
    def inferredSig(inferred: SSA.Val => IType) = {
      InferredSig(sig, srcs.drop(if (sig.static) 0 else 1).map(inferred))
    }
  }
  class InvokeStatic(var state: State,
                     var srcs: Seq[Val],
                     var cls: JType.Cls,
                     var name: String,
                     var desc: Desc) extends Val(desc.ret) with Invoke {
    def upstream = Option(state).toSeq ++ srcs
    def replaceUpstream(swap: Swapper): Unit = {
      if (state != null) state = swap(state)
      srcs = srcs.map(swap(_))
    }
    override def toString = s"${super.toString()}(${cls.name}, $name, $desc)"
  }

  class InvokeSpecial(var state: State,
                      var srcs: Seq[Val],
                      var cls: JType.Cls,
                      var name: String,
                      var desc: Desc) extends Val(desc.ret) with Invoke{
    def upstream = Option(state).toSeq ++ srcs
    def replaceUpstream(swap: Swapper): Unit = {
      if (state != null) state = swap(state)
      srcs = srcs.map(swap(_))
    }
    override def toString = s"${super.toString()}(${cls.name}, $name, $desc)"
  }

  class InvokeVirtual(var state: State,
                      var srcs: Seq[Val],
                      var cls: JType.Cls,
                      var name: String,
                      var desc: Desc,
                      var interface: Boolean) extends Val(desc.ret) with Invoke{
    def upstream = Option(state).toSeq ++ srcs
    def replaceUpstream(swap: Swapper): Unit = {
      if (state != null) state = swap(state)
      srcs = srcs.map(swap(_))
    }
    override def toString = s"${super.toString()}(${cls.name}, $name, $desc, $interface)"
  }

  class InvokeDynamic(var state: State,
                      var name: String,
                      var desc: Desc,
                      var bootstrap: InvokeDynamic.Bootstrap,
                      var bootstrapArgs: Seq[InvokeDynamic.Arg],
                      var srcs: Seq[Val]) extends Val(desc.ret) {
    def upstream = state +: srcs
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
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
  class New(var cls: JType.Cls) extends Val(cls){
    def upstream = Nil
    def replaceUpstream(swap: Swapper): Unit = {}
    override def toString = s"${super.toString()}(${cls.name})"
  }
  class NewArray(var state: State, var size: Val, var typeRef: JType)
    extends Val(JType.Arr(typeRef)) with Stateful{
    def upstream = Seq(state, size)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      size = swap(size)
    }
    override def toString = s"${super.toString()}(${typeRef.name})"
  }
  class MultiANewArray(var state: State, var desc: JType, var dims: Seq[Val])
    extends Val(desc) with Stateful{
    def upstream = state +: dims
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      dims = dims.map(swap(_))
    }
    override def toString = s"${super.toString()}(${desc.name})"
  }
  class PutStatic(var state: State, var src: Val, var cls: JType.Cls, var name: String, var desc: JType)
    extends Val(JType.Prim.V) with Stateful{
    def upstream = Seq(state, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      src = swap(src)
    }
    override def toString = s"${super.toString()}(${cls.name}, $name, ${desc.name})"
  }
  class GetStatic(var state: State, var cls: JType.Cls, var name: String, var desc: JType)
    extends Val(desc) with Stateful{
    def upstream = Seq(state)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
    }
    override def toString = s"${super.toString()}(${cls.name}, $name, ${desc.name})"
  }
  class PutField(var state: State, var src: Val, var obj: Val, var owner: JType.Cls, var name: String, var desc: JType)
    extends Val(JType.Prim.V) with Stateful{
    def upstream = Seq(state, src, obj)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      src = swap(src)
      obj = swap(obj)
    }
    override def toString = s"${super.toString()}(${owner.name}, $name, ${desc.name})"
  }
  class GetField(var state: State, var obj: Val, var owner: JType.Cls, var name: String, var desc: JType)
    extends Val(desc) with Stateful{
    def upstream = Seq(state, obj)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      obj = swap(obj)
    }
    override def toString = s"${super.toString()}(${owner.name}, $name, ${desc.name})"
  }
  class PutArray(var state: State, var array: Val, var indexSrc: Val, var src: Val)
    extends Val(JType.Prim.V) with Stateful{
    def upstream = Seq(state, array, indexSrc, src)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      array = swap(array)
      indexSrc = swap(indexSrc)
      src = swap(src)
    }
  }
  class GetArray(var state: State, var array: Val, var indexSrc: Val, var tpe: JType)
    extends Val(tpe) with Stateful{
    def upstream = Seq(state, array, indexSrc)
    def replaceUpstream(swap: Swapper): Unit = {
      state = swap(state)
      array = swap(array)
      indexSrc = swap(indexSrc)
    }
  }

  class MonitorEnter(var indexSrc: Val) extends Val(JType.Prim.V){
    def upstream = Seq(indexSrc)
    def replaceUpstream(swap: Swapper): Unit = {
      indexSrc = swap(indexSrc)
    }
  }
  class MonitorExit(var indexSrc: Val) extends Val(JType.Prim.V){
    def upstream = Seq(indexSrc)
    def replaceUpstream(swap: Swapper): Unit = {
      indexSrc = swap(indexSrc)
    }
  }
}
