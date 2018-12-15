package joptimize

import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.analysis.Interpreter


/**
  * An immutable wrapper around `org.objectweb.asm.tree.analysis.Frame[Inferred]`,
  */
class Frame(private val value: org.objectweb.asm.tree.analysis.Frame[Inferred]){
  val locals = new IndexedSeq[Inferred] {
    def length = value.getLocals
    def apply(idx: Int) = value.getLocal(idx)
  }
  val stack = new IndexedSeq[Inferred] {
    def length = value.getStackSize
    def apply(idx: Int) = value.getStack(idx)
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
  def execute(insn: AbstractInsnNode, interpreter: Interpreter[Inferred]) = {
    if (insn.getOpcode == -1) this
    else{
      val newFrame = new org.objectweb.asm.tree.analysis.Frame[Inferred](value)
      newFrame.execute(insn, interpreter)
      new Frame(newFrame)
    }
  }
}

object Frame{
  def initial(maxLocals: Int, maxStack: Int, args: List[Inferred]) = {
    val initialFrame0 = new org.objectweb.asm.tree.analysis.Frame[Inferred](maxLocals, maxStack)
    var i = 0
    for(arg <- args){
      for(_ <- 0 until arg.getSize){
        initialFrame0.setLocal(i, arg)
        i += 1
      }
    }
    new Frame(initialFrame0)
  }
}

