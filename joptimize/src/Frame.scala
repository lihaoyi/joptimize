package joptimize


import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.analysis.Interpreter


/**
  * An immutable wrapper around [[org.objectweb.asm.tree.analysis.Frame]],
  */
class Frame(private val value: org.objectweb.asm.tree.analysis.Frame[JType]){
  val locals = new IndexedSeq[JType] {
    def length = value.getLocals
    def apply(idx: Int) = value.getLocal(idx)
  }
  val stack = new IndexedSeq[JType] {

    def length = value.getStackSize
    def apply(idx: Int) = {
      value.getStack(idx) match{
        case null => throw new IndexOutOfBoundsException(s"$idx >= $length")
        case x => x
      }
    }
  }
  override def hashCode() = {
    (
      (0 until value.getLocals).map(value.getLocal),
      (0 until value.getStackSize).map(value.getStack)
    ).hashCode()
  }
  override def equals(obj: scala.Any) = obj match{
    case other: Frame =>
      other.value.getLocals == value.getLocals &&
        other.value.getStackSize == value.getStackSize &&
        (0 until value.getLocals).forall(i => value.getLocal(i) == other.value.getLocal(i)) &&
        (0 until value.getStackSize).forall(i => value.getStack(i) == other.value.getStack(i))
    case _ => false
  }
  def execute(insn: AbstractInsnNode, interpreter: Interpreter[JType]) = {
    if (insn.getOpcode == -1) this
    else{
      val newFrame = new org.objectweb.asm.tree.analysis.Frame[JType](value)
      newFrame.execute(insn, interpreter)
      new Frame(newFrame)
    }
  }

  override def toString = {
    def stringify(values: IndexedSeq[JType]) = {
      values
        .map{case null => " " case x => if (x == JType.Null) "_" else x.internalName}
        .mkString
    }
    s"Frame(${stringify(locals)}, ${stringify(stack)})"
  }
}

object Frame{
  def initial(maxLocals: Int, maxStack: Int, args: Seq[JType]) = {
    val initialFrame0 = new org.objectweb.asm.tree.analysis.Frame[JType](maxLocals, maxStack)
    var i = 0
    for(arg <- args){
      for(x <- 0 until arg.getSize){
        if (x == 0) initialFrame0.setLocal(i, arg)
        else initialFrame0.setLocal(i, JType.Null)
        i += 1
      }
    }
    new Frame(initialFrame0)
  }
}

