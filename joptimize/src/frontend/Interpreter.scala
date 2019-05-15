package joptimize.frontend

import org.objectweb.asm.Type
import org.objectweb.asm.tree.{AbstractInsnNode, TryCatchBlockNode}
import org.objectweb.asm.tree.analysis.Value


/**
  * A semantic bytecode interpreter. More precisely, this interpreter only manages the computation of
  * values from other values: it does not manage the transfer of values to or from the stack, and to
  * or from the local variables. This separation allows a generic bytecode {@link Analyzer} to work
  * with various semantic interpreters, without needing to duplicate the code to simulate the
  * transfer of values.
  *
  * @param < V> type of the Value used for the analysis.
  * @author Eric Bruneton
  */
abstract class Interpreter[V <: Value, S] {
  /**
    * Creates a new value that represents the given parameter type. This method is called to
    * initialize the value of a local corresponding to a method parameter in a frame.
    */
  def newParameterValue(local: Int, `type`: Type): V

  /**
    * Creates a new uninitialized value for a local variable. This method is called to initialize the
    * value of a local that does not correspond to a method parameter, and to reset one half of a
    * size-2 value when the other half is assigned a size-1 value.
    */
  def newEmptyValue(local: Int): V

  /**
    * Creates a new value that represents the given exception type. This method is called to
    * initialize the exception value on the call stack at the entry of an exception handler.
    */
  def newExceptionValue(tryCatchBlockNode: TryCatchBlockNode,
                        exceptionType: Type): V

  /**
    * ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5,
    * LCONST_0, LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0, DCONST_1, BIPUSH, SIPUSH, LDC, JSR,
    * NEW
    */
  def constOperation(insn: AbstractInsnNode): V
  /**
    * GETSTATIC
    */
  def getStaticOperation(insn: AbstractInsnNode, state: S): (V, S)

  /**
    * ILOAD, LLOAD, FLOAD, DLOAD, ALOAD, ISTORE, LSTORE, FSTORE, DSTORE, ASTORE, DUP, DUP_X1,
    * DUP_X2, DUP2, DUP2_X1, DUP2_X2, SWAP
    */
  def copyOperation(insn: AbstractInsnNode, value: V): V

  /**
    * INEG, LNEG, FNEG, DNEG, IINC, I2L, I2F, I2D, L2I, L2F, L2D, F2I, F2L, F2D, D2I, D2L, D2F,
    * I2B, I2C, I2S, INSTANCEOF
    */

  def unaryOp(insn: AbstractInsnNode, value: V): V
  /**
    * CHECKCAST, NEWARRAY, ANEWARRAY, ARRAYLENGTH
    */

  def unaryOpUnsafe(insn: AbstractInsnNode, value: V, state: S): (V, S)
  /**
    * GETFIELD
    */
  def getFieldOp(insn: AbstractInsnNode, value: V, state: S): (V, S)

  /**
    * IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE, TABLESWITCH, LOOKUPSWITCH, IRETURN, LRETURN,
    * * FRETURN, DRETURN, ARETURN, PUTSTATIC, ATHROW,
    * * MONITORENTER, MONITOREXIT, IFNULL, IFNONNULL
    */
  def unaryCommand(insn: AbstractInsnNode, value: V): Unit

  /**
    * PUTSTATIC
    */
  def putStaticCommand(insn: AbstractInsnNode, value: V, state: S): S

  /**
    * IADD, LADD, FADD, DADD, ISUB, LSUB, FSUB, DSUB, IMUL, LMUL, FMUL, DMUL,
    * ISHL, LSHL, ISHR, LSHR, IUSHR, LUSHR, IAND, LAND, IOR, LOR, IXOR, LXOR,
    * LCMP, FCMPL, FCMPG, DCMPL, DCMPG
    */
  def binaryOp(insn: AbstractInsnNode, value1: V, value2: V): V

  /**
    * IALOAD, LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD,
    * IDIV, LDIV, FDIV, DDIV, IREM, LREM, FREM, DREM,
    */
  def binaryOpUnsafe(insn: AbstractInsnNode, value1: V, value2: V, state: S): (V, S)

  /**
    * IF_ICMPEQ, IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ACMPEQ,
    * IF_ACMPNE
    */
  def binaryCommand(insn: AbstractInsnNode, value1: V, value2: V): Unit
  /**
    * PUTFIELD
    */
  def putFieldOp(insn: AbstractInsnNode, value1: V, value2: V, state: S): S

  /**
    * IASTORE, LASTORE, FASTORE, DASTORE, AASTORE, BASTORE, CASTORE, SASTORE
    */
  def ternaryOperation(insn: AbstractInsnNode, value1: V, value2: V, value3: V, state: S): S

  /**
    * INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC, INVOKEINTERFACE, MULTIANEWARRAY and
    * INVOKEDYNAMIC
    */
  def naryOperation(insn: AbstractInsnNode, values: Seq[V], state: S): (V, S)

  /**
    * Merges two values. The merge operation must return a value that represents both values (for
    * instance, if the two values are two types, the merged value must be a common super type of the
    * two types. If the two values are integer intervals, the merged value must be an interval that
    * contains the previous ones. Likewise for other types of values).
    *
    * @param value1 a value.
    * @param value2 another value.
    * @return the merged value. If the merged value is equal to { @code value1}, this method
    *                                                                   <i>must</i> return { @code value1}.
    */
  def merge[N <: V](value1: N, value2: N, insnIndex: Int, targetInsnIndex: Int): N

  def merge0[N <: V](value1: N, insnIndex: Int, targetInsnIndex: Int): N
}
