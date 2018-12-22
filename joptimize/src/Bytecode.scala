package joptimize
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.{AbstractInsnNode, MethodInsnNode, MultiANewArrayInsnNode}
/**
  * Table of stack effects of each bytecode instruction. Note that ASM treats
  * doubles and longs as one slot in the stack, rather than two.
  */
object Bytecode {
  sealed trait StackChange{
    def push(node: AbstractInsnNode): Int
    def pop(node: AbstractInsnNode): Int
  }
  case class Fixed(pop: Int, push: Int) extends StackChange{
    def push(node: AbstractInsnNode): Int = push
    def pop(node: AbstractInsnNode): Int = pop
  }
  case object MultiANewArray extends StackChange{
    def push(node: AbstractInsnNode): Int = 1
    def pop(node: AbstractInsnNode): Int = node.asInstanceOf[MultiANewArrayInsnNode].dims
  }
  case class Invoke(static: Boolean) extends StackChange{
    def push(node: AbstractInsnNode): Int = 1
    def pop(node: AbstractInsnNode): Int = Desc.read(node.asInstanceOf[MethodInsnNode].desc).args.size
  }
  case class InvokeDynamic() extends StackChange{
    def push(node: AbstractInsnNode): Int = ???
    def pop(node: AbstractInsnNode): Int = ???
  }
  val stackEffect = Map[Int, StackChange](
    (NOP, Fixed(0, 0)),// visitInsn
    (ACONST_NULL, Fixed(0, 1)),// -
    (ICONST_M1, Fixed(0, 1)),
    (ICONST_0, Fixed(0, 1)),
    (ICONST_1, Fixed(0, 1)),
    (ICONST_2, Fixed(0, 1)),
    (ICONST_3, Fixed(0, 1)),
    (ICONST_4, Fixed(0, 1)),
    (ICONST_5, Fixed(0, 1)),
    (LCONST_0, Fixed(0, 1)),
    (LCONST_1, Fixed(0, 1)),
    (FCONST_0, Fixed(0, 1)),
    (FCONST_1, Fixed(0, 1)),
    (FCONST_2, Fixed(0, 1)),
    (DCONST_0, Fixed(0, 1)),
    (DCONST_1, Fixed(0, 1)),
    (BIPUSH, Fixed(0, 1)),// visitIntInsn
    (SIPUSH, Fixed(0, 1)),
    (LDC, Fixed(0, 1)),// visitLdcInsn
    (ILOAD, Fixed(0, 1)),// visitVarInsn
    (LLOAD, Fixed(0, 1)),
    (FLOAD, Fixed(0, 1)),
    (DLOAD, Fixed(0, 1)),
    (ALOAD, Fixed(0, 1)),
    (IALOAD, Fixed(2, 1)),
    (LALOAD, Fixed(2, 1)),
    (FALOAD, Fixed(2, 1)),
    (DALOAD, Fixed(2, 1)),
    (AALOAD, Fixed(2, 1)),
    (BALOAD, Fixed(2, 1)),
    (CALOAD, Fixed(2, 1)),
    (SALOAD, Fixed(2, 1)),
    (ISTORE, Fixed(1, 0)),
    (LSTORE, Fixed(1, 0)),
    (FSTORE, Fixed(1, 0)),
    (DSTORE, Fixed(1, 0)),
    (ASTORE, Fixed(1, 0)),
    (IASTORE, Fixed(3, 0)),
    (LASTORE, Fixed(3, 0)),
    (FASTORE, Fixed(3, 0)),
    (DASTORE, Fixed(3, 0)),
    (AASTORE, Fixed(3, 0)),
    (BASTORE, Fixed(3, 0)),
    (CASTORE, Fixed(3, 0)),
    (SASTORE, Fixed(3, 0)),
    (POP, Fixed(1, 0)),
    (POP2, Fixed(2, 0)),
    (DUP, Fixed(1, 2)),
    (DUP_X1, Fixed(2, 3)),
    (DUP_X2, Fixed(3, 4)),
    (DUP2, Fixed(2, 4)),
    (DUP2_X1, Fixed(3, 5)),
    (DUP2_X2, Fixed(4, 6)),
    (SWAP, Fixed(2, 2)),
    (IADD, Fixed(2, 1)),
    (LADD, Fixed(2, 1)),
    (FADD, Fixed(2, 1)),
    (DADD, Fixed(2, 1)),
    (ISUB, Fixed(2, 1)),
    (LSUB, Fixed(2, 1)),
    (FSUB, Fixed(2, 1)),
    (DSUB, Fixed(2, 1)),
    (IMUL, Fixed(2, 1)),
    (LMUL, Fixed(2, 1)),
    (FMUL, Fixed(2, 1)),
    (DMUL, Fixed(2, 1)),
    (IDIV, Fixed(2, 1)),
    (LDIV, Fixed(2, 1)),
    (FDIV, Fixed(2, 1)),
    (DDIV, Fixed(2, 1)),
    (IREM, Fixed(2, 1)),
    (LREM, Fixed(2, 1)),
    (FREM, Fixed(2, 1)),
    (DREM, Fixed(2, 1)),
    (INEG, Fixed(1, 1)),
    (LNEG, Fixed(1, 1)),
    (FNEG, Fixed(1, 1)),
    (DNEG, Fixed(1, 1)),
    (ISHL, Fixed(2, 1)),
    (LSHL, Fixed(2, 1)),
    (ISHR, Fixed(2, 1)),
    (LSHR, Fixed(2, 1)),
    (IUSHR, Fixed(2, 1)),
    (LUSHR, Fixed(2, 1)),
    (IAND, Fixed(2, 1)),
    (LAND, Fixed(2, 1)),
    (IOR, Fixed(2, 1)),
    (LOR, Fixed(2, 1)),
    (IXOR, Fixed(2, 1)),
    (LXOR, Fixed(2, 1)),
    (IINC, Fixed(0, 0)),// visitIincInsn
    (I2L, Fixed(1, 1)),
    (I2F, Fixed(1, 1)),
    (I2D, Fixed(1, 1)),
    (L2I, Fixed(1, 1)),
    (L2F, Fixed(1, 1)),
    (L2D, Fixed(1, 1)),
    (F2I, Fixed(1, 1)),
    (F2L, Fixed(1, 1)),
    (F2D, Fixed(1, 1)),
    (D2I, Fixed(1, 1)),
    (D2L, Fixed(1, 1)),
    (D2F, Fixed(1, 1)),
    (I2B, Fixed(1, 1)),
    (I2C, Fixed(1, 1)),
    (I2S, Fixed(1, 1)),
    (LCMP, Fixed(2, 1)),
    (FCMPL, Fixed(2, 1)),
    (FCMPG, Fixed(2, 1)),
    (DCMPL, Fixed(2, 1)),
    (DCMPG, Fixed(2, 1)),
    (IFEQ, Fixed(1, 0)),// visitJumpInsn
    (IFNE, Fixed(1, 0)),
    (IFLT, Fixed(1, 0)),
    (IFGE, Fixed(1, 0)),
    (IFGT, Fixed(1, 0)),
    (IFLE, Fixed(1, 0)),
    (IF_ICMPEQ, Fixed(2, 0)),
    (IF_ICMPNE, Fixed(2, 0)),
    (IF_ICMPLT, Fixed(2, 0)),
    (IF_ICMPGE, Fixed(2, 0)),
    (IF_ICMPGT, Fixed(2, 0)),
    (IF_ICMPLE, Fixed(2, 0)),
    (IF_ACMPEQ, Fixed(2, 0)),
    (IF_ACMPNE, Fixed(2, 0)),
    (GOTO, Fixed(0, 0)),
    (JSR, Fixed(0, 1)),
    (RET, Fixed(0, 0)),
    (TABLESWITCH, Fixed(1, 0)),// visiTableSwitchInsn
    (LOOKUPSWITCH, Fixed(1, 0)),// visitLookupSwitch
    (IRETURN, Fixed(1, 0)),
    (LRETURN, Fixed(1, 0)),
    (FRETURN, Fixed(1, 0)),
    (DRETURN, Fixed(1, 0)),
    (ARETURN, Fixed(1, 0)),
    (RETURN, Fixed(0, 0)),
    (GETSTATIC, Fixed(0, 1)),// visitFieldInsn
    (PUTSTATIC, Fixed(1, 0)),
    (GETFIELD, Fixed(1, 1)),
    (PUTFIELD, Fixed(2, 0)),
    (INVOKEVIRTUAL, Invoke(static = false)),// visitMethodInsn
    (INVOKESPECIAL, Invoke(static = false)),
    (INVOKESTATIC, Invoke(static = true)),
    (INVOKEINTERFACE, Invoke(static = false)),
    (INVOKEDYNAMIC, InvokeDynamic()),// visitInvokeDynamicInsn
    (NEW, Fixed(0, 1)),// visitTypeInsn
    (NEWARRAY, Fixed(1, 1)),
    (ANEWARRAY, Fixed(1, 1)),
    (ARRAYLENGTH, Fixed(1, 1)),
    (ATHROW, Fixed(1, 1)),
    (CHECKCAST, Fixed(1, 1)),
    (INSTANCEOF, Fixed(1, 1)),
    (MONITORENTER, Fixed(1, 0)),
    (MONITOREXIT, Fixed(1, 0)),
    (MULTIANEWARRAY, MultiANewArray),// visitMultiANewArrayInsn
    (IFNULL, Fixed(1, 0)),
    (IFNONNULL, Fixed(1, 0)),
  )
}
