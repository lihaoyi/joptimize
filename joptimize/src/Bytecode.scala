package joptimize
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.{AbstractInsnNode, LdcInsnNode, MethodInsnNode, MultiANewArrayInsnNode}
/**
  * Table of stack effects of each bytecode instruction. Note that ASM treats
  * doubles and longs as one slot in the stack, rather than two.
  */
object Bytecode {
  sealed trait StackChange{
    def push(node: AbstractInsnNode): Int
    def pop(node: AbstractInsnNode): Int
    def nullType(node: AbstractInsnNode): Option[JType]
  }
  case class Fixed(pop: Int, push: Int, pushType: JType = null) extends StackChange{
    def push(node: AbstractInsnNode): Int = push
    def pop(node: AbstractInsnNode): Int = pop
    def nullType(node: AbstractInsnNode) = Option(pushType)
  }
  case class Ldc() extends StackChange{
    def push(node: AbstractInsnNode): Int = 1
    def pop(node: AbstractInsnNode): Int = 0
    def nullType(node: AbstractInsnNode) = Some(
      node.asInstanceOf[LdcInsnNode].cst match{
        case _: java.lang.Integer => JType.Prim.I
        case _: java.lang.Float => JType.Prim.F
        case _: java.lang.Long => JType.Prim.J
        case _: java.lang.Double => JType.Prim.D
        case _: java.lang.String => JType.Null
        case _: org.objectweb.asm.Type => JType.Null
      }
    )
  }
  case object MultiANewArray extends StackChange{
    def push(node: AbstractInsnNode): Int = 1
    def pop(node: AbstractInsnNode): Int = node.asInstanceOf[MultiANewArrayInsnNode].dims
    def nullType(node: AbstractInsnNode) = Some(JType.read(node.asInstanceOf[MultiANewArrayInsnNode].desc))
  }
  case class Invoke(static: Boolean) extends StackChange{
    def push(node: AbstractInsnNode): Int = 1
    def pop(node: AbstractInsnNode): Int = Desc.read(node.asInstanceOf[MethodInsnNode].desc).args.size
    def nullType(node: AbstractInsnNode) = Some(Desc.read(node.asInstanceOf[MethodInsnNode].desc).ret)
  }
  case class InvokeDynamic() extends StackChange{
    def push(node: AbstractInsnNode): Int = ???
    def pop(node: AbstractInsnNode): Int = ???
    def nullType(node: AbstractInsnNode) = ???
  }
  val stackEffect = Map[Int, StackChange](
    NOP             -> Fixed(0, 0),// visitInsn
    ACONST_NULL     -> Fixed(0, 1, JType.Null),// -
    ICONST_M1       -> Fixed(0, 1, JType.Prim.I),
    ICONST_0        -> Fixed(0, 1, JType.Prim.I),
    ICONST_1        -> Fixed(0, 1, JType.Prim.I),
    ICONST_2        -> Fixed(0, 1, JType.Prim.I),
    ICONST_3        -> Fixed(0, 1, JType.Prim.I),
    ICONST_4        -> Fixed(0, 1, JType.Prim.I),
    ICONST_5        -> Fixed(0, 1, JType.Prim.I),
    LCONST_0        -> Fixed(0, 1, JType.Prim.J),
    LCONST_1        -> Fixed(0, 1, JType.Prim.J),
    FCONST_0        -> Fixed(0, 1, JType.Prim.F),
    FCONST_1        -> Fixed(0, 1, JType.Prim.F),
    FCONST_2        -> Fixed(0, 1, JType.Prim.F),
    DCONST_0        -> Fixed(0, 1, JType.Prim.D),
    DCONST_1        -> Fixed(0, 1, JType.Prim.D),
    BIPUSH          -> Fixed(0, 1, JType.Prim.I),// visitIntInsn
    SIPUSH          -> Fixed(0, 1, JType.Prim.I),
    LDC             -> Ldc(),// visitLdcInsn
    ILOAD           -> Fixed(0, 1, JType.Prim.I),// visitVarInsn
    LLOAD           -> Fixed(0, 1, JType.Prim.J),
    FLOAD           -> Fixed(0, 1, JType.Prim.F),
    DLOAD           -> Fixed(0, 1, JType.Prim.D),
    ALOAD           -> Fixed(0, 1, JType.Null),
    IALOAD          -> Fixed(2, 1, JType.Prim.I),
    LALOAD          -> Fixed(2, 1, JType.Prim.J),
    FALOAD          -> Fixed(2, 1, JType.Prim.F),
    DALOAD          -> Fixed(2, 1, JType.Prim.D),
    AALOAD          -> Fixed(2, 1, JType.Null),
    BALOAD          -> Fixed(2, 1, JType.Prim.I),
    CALOAD          -> Fixed(2, 1, JType.Prim.I),
    SALOAD          -> Fixed(2, 1, JType.Prim.I),
    ISTORE          -> Fixed(1, 0),
    LSTORE          -> Fixed(1, 0),
    FSTORE          -> Fixed(1, 0),
    DSTORE          -> Fixed(1, 0),
    ASTORE          -> Fixed(1, 0),
    IASTORE         -> Fixed(3, 0),
    LASTORE         -> Fixed(3, 0),
    FASTORE         -> Fixed(3, 0),
    DASTORE         -> Fixed(3, 0),
    AASTORE         -> Fixed(3, 0),
    BASTORE         -> Fixed(3, 0),
    CASTORE         -> Fixed(3, 0),
    SASTORE         -> Fixed(3, 0),
    POP             -> Fixed(1, 0),
    POP2            -> Fixed(2, 0),
    DUP             -> Fixed(1, 2),
    DUP_X1          -> Fixed(2, 3),
    DUP_X2          -> Fixed(3, 4),
    DUP2            -> Fixed(2, 4),
    DUP2_X1         -> Fixed(3, 5),
    DUP2_X2         -> Fixed(4, 6),
    SWAP            -> Fixed(2, 2),
    IADD            -> Fixed(2, 1, JType.Prim.I),
    LADD            -> Fixed(2, 1, JType.Prim.J),
    FADD            -> Fixed(2, 1, JType.Prim.F),
    DADD            -> Fixed(2, 1, JType.Prim.D),
    ISUB            -> Fixed(2, 1, JType.Prim.I),
    LSUB            -> Fixed(2, 1, JType.Prim.J),
    FSUB            -> Fixed(2, 1, JType.Prim.F),
    DSUB            -> Fixed(2, 1, JType.Prim.D),
    IMUL            -> Fixed(2, 1, JType.Prim.I),
    LMUL            -> Fixed(2, 1, JType.Prim.J),
    FMUL            -> Fixed(2, 1, JType.Prim.F),
    DMUL            -> Fixed(2, 1, JType.Prim.D),
    IDIV            -> Fixed(2, 1, JType.Prim.I),
    LDIV            -> Fixed(2, 1, JType.Prim.J),
    FDIV            -> Fixed(2, 1, JType.Prim.F),
    DDIV            -> Fixed(2, 1, JType.Prim.D),
    IREM            -> Fixed(2, 1, JType.Prim.I),
    LREM            -> Fixed(2, 1, JType.Prim.J),
    FREM            -> Fixed(2, 1, JType.Prim.F),
    DREM            -> Fixed(2, 1, JType.Prim.D),
    INEG            -> Fixed(1, 1, JType.Prim.I),
    LNEG            -> Fixed(1, 1, JType.Prim.J),
    FNEG            -> Fixed(1, 1, JType.Prim.F),
    DNEG            -> Fixed(1, 1, JType.Prim.D),
    ISHL            -> Fixed(2, 1, JType.Prim.I),
    LSHL            -> Fixed(2, 1, JType.Prim.J),
    ISHR            -> Fixed(2, 1, JType.Prim.I),
    LSHR            -> Fixed(2, 1, JType.Prim.J),
    IUSHR           -> Fixed(2, 1, JType.Prim.I),
    LUSHR           -> Fixed(2, 1, JType.Prim.J),
    IAND            -> Fixed(2, 1, JType.Prim.I),
    LAND            -> Fixed(2, 1, JType.Prim.J),
    IOR             -> Fixed(2, 1, JType.Prim.I),
    LOR             -> Fixed(2, 1, JType.Prim.J),
    IXOR            -> Fixed(2, 1, JType.Prim.I),
    LXOR            -> Fixed(2, 1, JType.Prim.J),
    IINC            -> Fixed(0, 0),// visitIincInsn
    I2L             -> Fixed(1, 1, JType.Prim.J),
    I2F             -> Fixed(1, 1, JType.Prim.F),
    I2D             -> Fixed(1, 1, JType.Prim.D),
    L2I             -> Fixed(1, 1, JType.Prim.I),
    L2F             -> Fixed(1, 1, JType.Prim.F),
    L2D             -> Fixed(1, 1, JType.Prim.D),
    F2I             -> Fixed(1, 1, JType.Prim.I),
    F2L             -> Fixed(1, 1, JType.Prim.J),
    F2D             -> Fixed(1, 1, JType.Prim.D),
    D2I             -> Fixed(1, 1, JType.Prim.I),
    D2L             -> Fixed(1, 1, JType.Prim.J),
    D2F             -> Fixed(1, 1, JType.Prim.F),
    I2B             -> Fixed(1, 1, JType.Prim.I),
    I2C             -> Fixed(1, 1, JType.Prim.I),
    I2S             -> Fixed(1, 1, JType.Prim.I),
    LCMP            -> Fixed(2, 1, JType.Prim.I),
    FCMPL           -> Fixed(2, 1, JType.Prim.I),
    FCMPG           -> Fixed(2, 1, JType.Prim.I),
    DCMPL           -> Fixed(2, 1, JType.Prim.I),
    DCMPG           -> Fixed(2, 1, JType.Prim.I),
    IFEQ            -> Fixed(1, 0),// visitJumpInsn
    IFNE            -> Fixed(1, 0),
    IFLT            -> Fixed(1, 0),
    IFGE            -> Fixed(1, 0),
    IFGT            -> Fixed(1, 0),
    IFLE            -> Fixed(1, 0),
    IF_ICMPEQ       -> Fixed(2, 0),
    IF_ICMPNE       -> Fixed(2, 0),
    IF_ICMPLT       -> Fixed(2, 0),
    IF_ICMPGE       -> Fixed(2, 0),
    IF_ICMPGT       -> Fixed(2, 0),
    IF_ICMPLE       -> Fixed(2, 0),
    IF_ACMPEQ       -> Fixed(2, 0),
    IF_ACMPNE       -> Fixed(2, 0),
    GOTO            -> Fixed(0, 0),
    JSR             -> Fixed(0, 1),
    RET             -> Fixed(0, 0),
    TABLESWITCH     -> Fixed(1, 0),// visiTableSwitchInsn
    LOOKUPSWITCH    -> Fixed(1, 0),// visitLookupSwitch
    IRETURN         -> Fixed(1, 0),
    LRETURN         -> Fixed(1, 0),
    FRETURN         -> Fixed(1, 0),
    DRETURN         -> Fixed(1, 0),
    ARETURN         -> Fixed(1, 0),
    RETURN          -> Fixed(0, 0),
    GETSTATIC       -> Fixed(0, 1),// visitFieldInsn
    PUTSTATIC       -> Fixed(1, 0),
    GETFIELD        -> Fixed(1, 1),
    PUTFIELD        -> Fixed(2, 0),
    INVOKEVIRTUAL   -> Invoke(static = false),// visitMethodInsn
    INVOKESPECIAL   -> Invoke(static = false),
    INVOKESTATIC    -> Invoke(static = true),
    INVOKEINTERFACE -> Invoke(static = false),
    INVOKEDYNAMIC   -> InvokeDynamic(),// visitInvokeDynamicInsn
    NEW             -> Fixed(0, 1, JType.Null),// visitTypeInsn
    NEWARRAY        -> Fixed(1, 1, JType.Null),
    ANEWARRAY       -> Fixed(1, 1, JType.Null),
    ARRAYLENGTH     -> Fixed(1, 1, JType.Prim.I),
    ATHROW          -> Fixed(1, 0),
    CHECKCAST       -> Fixed(1, 1, JType.Null),
    INSTANCEOF      -> Fixed(1, 1, JType.Prim.I),
    MONITORENTER    -> Fixed(1, 0),
    MONITOREXIT     -> Fixed(1, 0),
    MULTIANEWARRAY  -> MultiANewArray,// visitMultiANewArrayInsn
    IFNULL          -> Fixed(1, 0),
    IFNONNULL       -> Fixed(1, 0),
  )
}
