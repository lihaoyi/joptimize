package forked

import java.util
import java.util.{ArrayList, List}

import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.IincInsnNode
import org.objectweb.asm.tree.InvokeDynamicInsnNode
import org.objectweb.asm.tree.LabelNode
import org.objectweb.asm.tree.MethodInsnNode
import org.objectweb.asm.tree.MultiANewArrayInsnNode
import org.objectweb.asm.tree.VarInsnNode
import org.objectweb.asm.tree.analysis.AnalyzerException
import org.objectweb.asm.tree.analysis.Value

import scala.collection.mutable


/**
  * A symbolic execution stack frame. A stack frame contains a set of local variable slots, and an
  * operand stack. Warning: long and double values are represented with <i>two</i> slots in local
  * variables, and with <i>one</i> slot in the operand stack.
  *
  * @param < V> type of the Value used for the analysis.
  * @author Eric Bruneton
  */
class Frame[V <: Value] @SuppressWarnings(Array("unchecked"))(/** The number of local variables of this frame. */
                                                              var numLocals: Int, val numStack0: Int){

/**
  * Constructs a new frame with the given size.
  *
  * @param numLocals the maximum number of local variables of the frame.
  * @param numStack  the maximum stack size of the frame.
  */
  /**
    * The local variables and the operand stack of this frame. The first {@link #numLocals} elements
    * correspond to the local variables. The following {@link #numStack} elements correspond to the
    * operand stack.
    */
  private val values = new Array[Value](numLocals + numStack0).asInstanceOf[Array[V]]
  /** The number of elements in the operand stack. */
  private var numStack = 0

  /**
    * Constructs a copy of the given Frame.
    *
    * @param frame a frame.
    */
  def this(frame: Frame[_ <: V]) {
    this(frame.numLocals, frame.values.length - frame.numLocals)
    init(frame) // NOPMD(ConstructorCallsOverridableMethod): can't fix for backward compatibility.

  }

  /**
    * Copies the state of the given frame into this frame.
    *
    * @param frame a frame.
    * @return this frame.
    */
  def init(frame: Frame[_ <: V]) = {
    System.arraycopy(frame.values, 0, values, 0, values.length)
    numStack = frame.numStack
    this
  }

  /**
    * Returns the maximum number of local variables of this frame.
    *
    * @return the maximum number of local variables of this frame.
    */
  def getLocals = numLocals

  /**
    * Returns the maximum stack size of this frame.
    *
    * @return the maximum stack size of this frame.
    */
  def getMaxStackSize = values.length - numLocals

  /**
    * Returns the value of the given local variable.
    *
    * @param index a local variable index.
    * @return the value of the given local variable.
    * @throws IndexOutOfBoundsException if the variable does not exist.
    */
  def getLocal(index: Int) = {
    if (index >= numLocals) throw new IndexOutOfBoundsException("Trying to access an inexistant local variable")
    values(index)
  }

  /**
    * Sets the value of the given local variable.
    *
    * @param index a local variable index.
    * @param value the new value of this local variable.
    * @throws IndexOutOfBoundsException if the variable does not exist.
    */
  def setLocal(index: Int, value: V): Unit = {
    if (index >= numLocals) throw new IndexOutOfBoundsException("Trying to access an inexistant local variable " + index)
    values(index) = value
  }

  /**
    * Returns the number of values in the operand stack of this frame. Long and double values are
    * treated as single values.
    *
    * @return the number of values in the operand stack of this frame.
    */
  def getStackSize = numStack

  /**
    * Returns the value of the given operand stack slot.
    *
    * @param index the index of an operand stack slot.
    * @return the value of the given operand stack slot.
    * @throws IndexOutOfBoundsException if the operand stack slot does not exist.
    */
  def getStack(index: Int) = values(numLocals + index)

  /**
    * Sets the value of the given stack slot.
    *
    * @param index the index of an operand stack slot.
    * @param value the new value of the stack slot.
    * @throws IndexOutOfBoundsException if the stack slot does not exist.
    */
  @throws[IndexOutOfBoundsException]
  def setStack(index: Int, value: V): Unit = {
    values(numLocals + index) = value
  }

  /** Clears the operand stack of this frame. */
  def clearStack(): Unit = {
    numStack = 0
  }

  /**
    * Pops a value from the operand stack of this frame.
    *
    * @return the value that has been popped from the stack.
    * @throws IndexOutOfBoundsException if the operand stack is empty.
    */
  def pop() = {
    if (numStack == 0) throw new IndexOutOfBoundsException("Cannot pop operand off an empty stack.")
    values(numLocals + {
      numStack -= 1; numStack
    })
  }

  /**
    * Pushes a value into the operand stack of this frame.
    *
    * @param value the value that must be pushed into the stack.
    * @throws IndexOutOfBoundsException if the operand stack is full.
    */
  def push(value: V): Unit = {
    if (numLocals + numStack >= values.length) throw new IndexOutOfBoundsException("Insufficient maximum stack size.")
    values(numLocals + {
      numStack += 1; numStack - 1
    }) = value
  }

  /**
    * Simulates the execution of the given instruction on this execution stack frame.
    *
    * @param insn        the instruction to execute.
    * @param interpreter the interpreter to use to compute values from other values.
    * @throws AnalyzerException if the instruction cannot be executed on this execution frame (e.g. a
    *                           POP on an empty operand stack).
    */
  @throws[AnalyzerException]
  def execute(insn: AbstractInsnNode, interpreter: Interpreter[V]): Unit = {
    var value1: V = null.asInstanceOf[V]
    var value2: V = null.asInstanceOf[V]
    var value3: V = null.asInstanceOf[V]
    var value4: V = null.asInstanceOf[V]
    var `var` = 0
    import Opcodes._
    insn.getOpcode match {
      case NOP =>
      case ACONST_NULL | ICONST_M1 | ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 |
           ICONST_4 | ICONST_5 | LCONST_0 | LCONST_1 | FCONST_0 | FCONST_1 | FCONST_2 |
           DCONST_0 | DCONST_1 | BIPUSH | SIPUSH | LDC =>
        push(interpreter.newOperation(insn))
      case ILOAD | LLOAD | FLOAD | DLOAD | ALOAD =>
        push(interpreter.copyOperation(insn, getLocal(insn.asInstanceOf[VarInsnNode].`var`)))
      case ISTORE | LSTORE | FSTORE | DSTORE | ASTORE =>
        value1 = interpreter.copyOperation(insn, pop())
        `var` = insn.asInstanceOf[VarInsnNode].`var`
        setLocal(`var`, value1)
        if (value1.getSize == 2) setLocal(`var` + 1, interpreter.newEmptyValue(`var` + 1))
        if (`var` > 0) {
          val local = getLocal(`var` - 1)
          if (local != null && local.getSize == 2) setLocal(`var` - 1, interpreter.newEmptyValue(`var` - 1))
        }
      case Opcodes.IASTORE | LASTORE | FASTORE | DASTORE | AASTORE | BASTORE | CASTORE | SASTORE =>
        value3 = pop()
        value2 = pop()
        value1 = pop()
      case Opcodes.POP =>
        if (pop.getSize == 2) throw new AnalyzerException(insn, "Illegal use of POP")
      case Opcodes.POP2 =>
        if (pop.getSize == 1 && pop.getSize != 1) throw new AnalyzerException(insn, "Illegal use of POP2")
      case Opcodes.DUP =>
        value1 = pop()
        if (value1.getSize != 1) throw new AnalyzerException(insn, "Illegal use of DUP")
        push(value1)
        push(interpreter.copyOperation(insn, value1))
      case Opcodes.DUP_X1 =>
        value1 = pop()
        value2 = pop()
        if (value1.getSize != 1 || value2.getSize != 1) throw new AnalyzerException(insn, "Illegal use of DUP_X1")
        push(interpreter.copyOperation(insn, value1))
        push(value2)
        push(value1)
      case Opcodes.DUP_X2 =>
        value1 = pop()
        if (value1.getSize == 1) {
          value2 = pop()
          if (value2.getSize == 1) {
            value3 = pop()
            if (value3.getSize == 1) {
              push(interpreter.copyOperation(insn, value1))
              push(value3)
              push(value2)
              push(value1)
            }
          }
          else {
            push(interpreter.copyOperation(insn, value1))
            push(value2)
            push(value1)
          }
        }
        throw new AnalyzerException(insn, "Illegal use of DUP_X2")
      case Opcodes.DUP2 =>
        value1 = pop()
        if (value1.getSize == 1) {
          value2 = pop()
          if (value2.getSize == 1) {
            push(value2)
            push(value1)
            push(interpreter.copyOperation(insn, value2))
            push(interpreter.copyOperation(insn, value1))
          }
        }
        else {
          push(value1)
          push(interpreter.copyOperation(insn, value1))
        }
        throw new AnalyzerException(insn, "Illegal use of DUP2")
      case Opcodes.DUP2_X1 =>
        value1 = pop()
        if (value1.getSize == 1) {
          value2 = pop()
          if (value2.getSize == 1) {
            value3 = pop()
            if (value3.getSize == 1) {
              push(interpreter.copyOperation(insn, value2))
              push(interpreter.copyOperation(insn, value1))
              push(value3)
              push(value2)
              push(value1)
            }
          }
        }
        else {
          value2 = pop()
          if (value2.getSize == 1) {
            push(interpreter.copyOperation(insn, value1))
            push(value2)
            push(value1)
          }
        }
        throw new AnalyzerException(insn, "Illegal use of DUP2_X1")
      case Opcodes.DUP2_X2 =>
        value1 = pop()
        if (value1.getSize == 1) {
          value2 = pop()
          if (value2.getSize == 1) {
            value3 = pop()
            if (value3.getSize == 1) {
              value4 = pop()
              if (value4.getSize == 1) {
                push(interpreter.copyOperation(insn, value2))
                push(interpreter.copyOperation(insn, value1))
                push(value4)
                push(value3)
                push(value2)
                push(value1)
              }
            }
            else {
              push(interpreter.copyOperation(insn, value2))
              push(interpreter.copyOperation(insn, value1))
              push(value3)
              push(value2)
              push(value1)
            }
          }
        }
        else {
          value2 = pop()
          if (value2.getSize == 1) {
            value3 = pop()
            if (value3.getSize == 1) {
              push(interpreter.copyOperation(insn, value1))
              push(value3)
              push(value2)
              push(value1)
            }
          }
          else {
            push(interpreter.copyOperation(insn, value1))
            push(value2)
            push(value1)
          }
        }
        throw new AnalyzerException(insn, "Illegal use of DUP2_X2")
      case Opcodes.SWAP =>
        value2 = pop()
        value1 = pop()
        if (value1.getSize != 1 || value2.getSize != 1) throw new AnalyzerException(insn, "Illegal use of SWAP")
        push(interpreter.copyOperation(insn, value2))
        push(interpreter.copyOperation(insn, value1))
      case IALOAD | LALOAD | FALOAD | DALOAD | AALOAD | BALOAD | CALOAD | SALOAD |
           IADD | LADD | FADD | DADD | ISUB | LSUB | FSUB | DSUB | IMUL | LMUL | FMUL |
           DMUL | IDIV | LDIV | FDIV | DDIV | IREM | LREM | FREM | DREM | ISHL | LSHL |
           ISHR | LSHR | IUSHR | LUSHR | IAND | LAND | IOR | LOR | IXOR | LXOR | LCMP |
           FCMPL | FCMPG | DCMPL | DCMPG =>
        value2 = pop()
        value1 = pop()
        push(interpreter.binaryOperation(insn, value1, value2))
      case INEG | LNEG | FNEG | DNEG =>
        push(interpreter.unaryOperation(insn, pop()))
      case IINC =>
        `var` = insn.asInstanceOf[IincInsnNode].`var`
        setLocal(`var`, interpreter.unaryOperation(insn, getLocal(`var`)))
      case I2L | I2F | I2D | L2I | L2F | L2D | F2I | F2L | F2D | D2I | D2L |
           D2F | I2B | I2C | I2S =>
        push(interpreter.unaryOperation(insn, pop()))
      case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE =>
        interpreter.unaryOperation(insn, pop())
      case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE |
           IF_ACMPEQ | IF_ACMPNE | PUTFIELD =>
        value2 = pop()
        value1 = pop()
        interpreter.binaryOperation(insn, value1, value2)
      case GOTO =>
      case JSR =>push(interpreter.newOperation(insn))
      case RET | TABLESWITCH | LOOKUPSWITCH =>
        interpreter.unaryOperation(insn, pop())
      case IRETURN | LRETURN | FRETURN | DRETURN | ARETURN =>
        value1 = pop()
        interpreter.unaryOperation(insn, value1)
      case RETURN =>
      case GETSTATIC => push(interpreter.newOperation(insn))
      case PUTSTATIC => interpreter.unaryOperation(insn, pop())
      case GETFIELD => push(interpreter.unaryOperation(insn, pop()))
      case INVOKEVIRTUAL | INVOKESPECIAL | INVOKESTATIC | INVOKEINTERFACE =>
        val valueList = new mutable.ArrayBuffer[V]
        val methodDescriptor = insn.asInstanceOf[MethodInsnNode].desc
        var i = Type.getArgumentTypes(methodDescriptor).length
        while ( {
          i > 0
        }) {
          valueList.insert(0, pop())

          {
            i -= 1; i
          }
        }
        if (insn.getOpcode != Opcodes.INVOKESTATIC) valueList.insert(0, pop())
        if (Type.getReturnType(methodDescriptor) eq Type.VOID_TYPE) interpreter.naryOperation(insn, valueList)
        else push(interpreter.naryOperation(insn, valueList))

      case INVOKEDYNAMIC =>
        val valueList = new mutable.ArrayBuffer[V]
        val methodDesccriptor = insn.asInstanceOf[InvokeDynamicInsnNode].desc
        var i = Type.getArgumentTypes(methodDesccriptor).length
        while ( {
          i > 0
        }) {
          valueList.insert(0, pop())

          {
            i -= 1; i
          }
        }
        if (Type.getReturnType(methodDesccriptor) eq Type.VOID_TYPE) interpreter.naryOperation(insn, valueList)
        else push(interpreter.naryOperation(insn, valueList))

      case NEW =>
        push(interpreter.newOperation(insn))
      case NEWARRAY | ANEWARRAY | ARRAYLENGTH =>
        push(interpreter.unaryOperation(insn, pop()))
      case ATHROW =>
        interpreter.unaryOperation(insn, pop())
      case CHECKCAST | INSTANCEOF =>
        push(interpreter.unaryOperation(insn, pop()))
      case MONITORENTER | MONITOREXIT =>
        interpreter.unaryOperation(insn, pop())
      case MULTIANEWARRAY =>
        val valueList = new mutable.ArrayBuffer[V]
        var i = insn.asInstanceOf[MultiANewArrayInsnNode].dims
        while ( {
          i > 0
        }) {
          valueList.insert(0, pop())

          {
            i -= 1; i
          }
        }
        push(interpreter.naryOperation(insn, valueList))
      case IFNULL | IFNONNULL =>
        interpreter.unaryOperation(insn, pop())
      case _ =>
        throw new AnalyzerException(insn, "Illegal opcode " + insn.getOpcode)
    }
  }

  /**
    * Merges the given frame into this frame.
    *
    * @param frame       a frame. This frame is left unchanged by this method.
    * @param interpreter the interpreter used to merge values.
    * @return { @literal true} if this frame has been changed as a result of the merge operation, or
    *                    { @literal false} otherwise.
    * @throws AnalyzerException if the frames have incompatible sizes.
    */
  @throws[AnalyzerException]
  def merge(insnIndex: Int, frame: Frame[_ <: V], interpreter: Interpreter[V]) = {
    if (numStack != frame.numStack) throw new AnalyzerException(null, "Incompatible stack heights")
    var changed = false
    var i = 0
    while ( {
      i < numLocals + numStack
    }) {
      val v = interpreter.merge(values(i), frame.values(i), insnIndex)
      if (!(v == values(i))) {
        values(i) = v
        changed = true
      }

      {
        i += 1; i
      }
    }
    changed
  }

  /**
    * Merges this frame into itself
    *
    * @param interpreter the interpreter used to merge values.
    * @return { @literal true} if this frame has been changed as a result of the merge operation, or
    *                    { @literal false} otherwise.
    * @throws AnalyzerException if the frames have incompatible sizes.
    */
  @throws[AnalyzerException]
  def merge0(insnIndex: Int, interpreter: Interpreter[V]) = {
    var changed = false
    var i = 0
    while ( {
      i < numLocals + numStack
    }) {
      val v = interpreter.merge0(values(i), insnIndex)
      if (!(v == values(i))) {
        values(i) = v
        changed = true
      }

      {
        i += 1; i
      }
    }
    changed
  }

  /**
    * Returns a string representation of this frame.
    *
    * @return a string representation of this frame.
    */
  override def toString = {
    val stringBuilder = new StringBuilder
    var i = 0
    while ( {
      i < getLocals
    }) {
      stringBuilder.append(getLocal(i))

      {
        i += 1; i
      }
    }
    stringBuilder.append(' ')
    var i2 = 0
    while ( {
      i2 < getStackSize
    }) {
      stringBuilder.append(getStack(i2).toString)

      {
        i2 += 1; i2
      }
    }
    stringBuilder.toString
  }
}