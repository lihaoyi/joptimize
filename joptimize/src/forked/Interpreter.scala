package forked


import java.util
import org.objectweb.asm.Type
import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.TryCatchBlockNode
import org.objectweb.asm.tree.analysis.AnalyzerException
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
abstract class Interpreter[V <: Value] {
  /**
    * Creates a new value that represents the given parameter type. This method is called to
    * initialize the value of a local corresponding to a method parameter in a frame.
    *
    * <p>By default, calls <code>newValue(type)</code>.
    *
    * @param local the local variable index.
    * @param type  a primitive or reference type.
    * @return a value that represents the given type. The size of the returned value must be equal to
    *         the size of the given type.
    */
  def newParameterValue(local: Int, `type`: Type): V

  /**
    * Creates a new uninitialized value for a local variable. This method is called to initialize the
    * value of a local that does not correspond to a method parameter, and to reset one half of a
    * size-2 value when the other half is assigned a size-1 value.
    *
    * <p>By default, calls <code>newValue(null)</code>.
    *
    * @param local the local variable index.
    * @return a value representing an uninitialized value. The size of the returned value must be
    *         equal to 1.
    */
  def newEmptyValue(local: Int): V

  /**
    * Creates a new value that represents the given exception type. This method is called to
    * initialize the exception value on the call stack at the entry of an exception handler.
    *
    * <p>By default, calls <code>newValue(exceptionType)</code>.
    *
    * @param tryCatchBlockNode the exception handler.
    * @param handlerFrame      the exception handler frame.
    * @param exceptionType     the exception type handled by this handler.
    * @return a value that represents the given { @code exceptionType}. The size of the returned value
    *                                                   must be equal to 1.
    */
  def newExceptionValue(tryCatchBlockNode: TryCatchBlockNode,
                        handlerFrame: Frame[V],
                        exceptionType: Type): V

  /**
    * Interprets a bytecode instruction without arguments. This method is called for the following
    * opcodes:
    *
    * <p>ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5,
    * LCONST_0, LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0, DCONST_1, BIPUSH, SIPUSH, LDC, JSR,
    * GETSTATIC, NEW
    *
    * @param insn the bytecode instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException if an error occurred during the interpretation.
    */
  def newOperation(insn: AbstractInsnNode): V

  /**
    * Interprets a bytecode instruction that moves a value on the stack or to or from local
    * variables. This method is called for the following opcodes:
    *
    * <p>ILOAD, LLOAD, FLOAD, DLOAD, ALOAD, ISTORE, LSTORE, FSTORE, DSTORE, ASTORE, DUP, DUP_X1,
    * DUP_X2, DUP2, DUP2_X1, DUP2_X2, SWAP
    *
    * @param insn  the bytecode instruction to be interpreted.
    * @param value the value that must be moved by the inst ruction.
    * @return the result of the interpretation of the given instruction. The returned value must be
    *         { @code equal} to the given value.
    * @throws AnalyzerException if an error occurred during the interpretation.
    */
  def copyOperation(insn: AbstractInsnNode, value: V): V

  /**
    * Interprets a bytecode instruction with a single argument. This method is called for the
    * following opcodes:
    *
    * <p>INEG, LNEG, FNEG, DNEG, IINC, I2L, I2F, I2D, L2I, L2F, L2D, F2I, F2L, F2D, D2I, D2L, D2F,
    * I2B, I2C, I2S, IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE, TABLESWITCH, LOOKUPSWITCH, IRETURN, LRETURN,
    * FRETURN, DRETURN, ARETURN, PUTSTATIC, GETFIELD, NEWARRAY, ANEWARRAY, ARRAYLENGTH, ATHROW,
    * CHECKCAST, INSTANCEOF, MONITORENTER, MONITOREXIT, IFNULL, IFNONNULL
    *
    * @param insn  the bytecode instruction to be interpreted.
    * @param value the argument of the instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException if an error occurred during the interpretation.
    */
  def unaryOperation(insn: AbstractInsnNode, value: V): V

  /**
    * Interprets a bytecode instruction with two arguments. This method is called for the following
    * opcodes:
    *
    * <p>IALOAD, LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD, IADD, LADD, FADD, DADD,
    * ISUB, LSUB, FSUB, DSUB, IMUL, LMUL, FMUL, DMUL, IDIV, LDIV, FDIV, DDIV, IREM, LREM, FREM, DREM,
    * ISHL, LSHL, ISHR, LSHR, IUSHR, LUSHR, IAND, LAND, IOR, LOR, IXOR, LXOR, LCMP, FCMPL, FCMPG,
    * DCMPL, DCMPG, IF_ICMPEQ, IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ACMPEQ,
    * IF_ACMPNE, PUTFIELD
    *
    * @param insn   the bytecode instruction to be interpreted.
    * @param value1 the first argument of the instruction to be interpreted.
    * @param value2 the second argument of the instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException if an error occurred during the interpretation.
    */
  @throws[AnalyzerException]
  def binaryOperation(insn: AbstractInsnNode, value1: V, value2: V): V

  /**
    * Interprets a bytecode instruction with a variable number of arguments. This method is called
    * for the following opcodes:
    *
    * <p>INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC, INVOKEINTERFACE, MULTIANEWARRAY and
    * INVOKEDYNAMIC
    *
    * @param insn   the bytecode instruction to be interpreted.
    * @param values the arguments of the instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException if an error occurred during the interpretation.
    */
  @throws[AnalyzerException]
  def naryOperation(insn: AbstractInsnNode, values: Seq[V]): V

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
  def merge(value1: V, value2: V, insnIndex: Int): V

  def merge0(value1: V, insnIndex: Int): V
}
