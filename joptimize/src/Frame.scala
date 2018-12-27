package joptimize


import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.analysis.{Interpreter, Value}


/**
  * An immutable wrapper around [[org.objectweb.asm.tree.analysis.Frame]],
  */
class Frame[T <: Value](protected val value: org.objectweb.asm.tree.analysis.Frame[T]){
  val locals = new IndexedSeq[T] {
    def length = value.getLocals
    def apply(idx: Int) = value.getLocal(idx)
  }
  val stack = new IndexedSeq[T] {

    def length = value.getStackSize
    def apply(idx: Int) = {
      value.getStack(idx) match{
        case null => throw new IndexOutOfBoundsException(s"$idx >= $length")
        case x => x
      }
    }
  }

//  assert(!locals.contains(null), locals.indexOf(null))
//  assert(!stack.contains(null), stack.indexOf(null))

  def map[V <: Value](func: T => V) = {
    val f0 = new org.objectweb.asm.tree.analysis.Frame[V](value.getLocals, value.getMaxStackSize)
    for(i <- 0 until stack.length) f0.push(func(stack(i)))
    for(i <- 0 until locals.length) value.getLocal(i) match{
      case null =>
      case x => f0.setLocal(i, func(x))
    }
    new Frame(f0)
  }

  def zipForeach[V <: Value](other: Frame[V])(func: (T, V) => Unit): Unit = {
    assert(stack.length == other.stack.length)
    assert(locals.length == other.locals.length)
    for(i <- 0 until stack.length) func(stack(i), other.stack(i))
    for(i <- 0 until locals.length) (locals(i), other.locals(i)) match{
      case (null, null) => // Skip null entries
      case (_, null) | (null, _) => ??? // We do not expect any asymmetric nullness
      case (l, r) => func(l, r)
    }
  }


  override def hashCode() = {
    (
      (0 until value.getLocals).map(value.getLocal),
      (0 until value.getStackSize).map(value.getStack)
    ).hashCode()
  }
  override def equals(obj: scala.Any) = obj match{
    case other: Frame[T] =>
      other.value.getLocals == value.getLocals &&
      other.value.getStackSize == value.getStackSize &&
      (0 until value.getLocals).forall(i => value.getLocal(i) == other.value.getLocal(i)) &&
      (0 until value.getStackSize).forall(i => value.getStack(i) == other.value.getStack(i))
    case _ => false
  }
  def execute(insn: AbstractInsnNode, interpreter: Interpreter[T]) = {
    if (insn.getOpcode == -1) this
    else{
      val newFrame = new org.objectweb.asm.tree.analysis.Frame[T](value)
      newFrame.execute(insn, interpreter)
      new Frame(newFrame)
    }
  }
  def handleException(ex: T) = {
    val newFrame = new org.objectweb.asm.tree.analysis.Frame[T](value)
    newFrame.clearStack()
    newFrame.push(ex)
    new Frame(newFrame)
  }
}
object Frame{
  def initial[T <: Value](maxLocals: Int, maxStack: Int, args: Seq[T], nilValue: T) = {
    val initialFrame0 = new org.objectweb.asm.tree.analysis.Frame[T](maxLocals, maxStack)
    var i = 0
    for(arg <- args){
      for(x <- 0 until arg.getSize){
        if (x == 0) initialFrame0.setLocal(i, arg)
        else initialFrame0.setLocal(i, nilValue)
        i += 1
      }
    }
    new Frame[T](initialFrame0)
  }
}

