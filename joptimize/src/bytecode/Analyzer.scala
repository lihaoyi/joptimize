package joptimize.bytecode
import collection.JavaConverters._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.InsnList
import org.objectweb.asm.tree.JumpInsnNode
import org.objectweb.asm.tree.LookupSwitchInsnNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.TableSwitchInsnNode
import org.objectweb.asm.tree.TryCatchBlockNode
import org.objectweb.asm.tree.analysis._

import scala.collection.mutable


/**
  * Fork of {@link org.objectweb.asm.tree.analysis.Analyzer},
  * to give us a bit more flexibility in keeping track of the control flow
  * graph's incoming edges during the {@link Interpreter#merge}
  */
object Analyzer{
  def analyze[V <: Value](owner: String,
                          method: MethodNode,
                          interpreter: Interpreter[V]): Array[Frame[V]] = {
    /** The instructions of the currently analyzed method. */
    val insnList = method.instructions
    /** The size of {@link #insnList}. */
    val insnListSize = insnList.size()
    /** The exception handlers of the currently analyzed method (one list per instruction index). */
    val handlers = new Array[mutable.Buffer[TryCatchBlockNode]](insnListSize)
    /** The execution stack frames of the currently analyzed method (one per instruction index). */
    val frames = new Array[Frame[V]](insnListSize)
    /** The instructions that remain to process (one boolean per instruction index). */
    val inInstructionsToProcess = new Array[Boolean](insnListSize)
    /** The indices of the instructions that remain to process in the currently analyzed method. */
    val instructionsToProcess = new Array[Int](insnListSize)
    /** The number of instructions that remain to process in the currently analyzed method. */
    var numInstructionsToProcess = 0

    def merge(insnIndex: Int, targetInsnIndex: Int, frame: Frame[V]) = {
      val oldFrame = frames(targetInsnIndex)
      if (oldFrame != null) oldFrame.merge(insnIndex, targetInsnIndex, frame, interpreter)
      else {
        frames(targetInsnIndex) = new Frame[V](frame)
        frames(targetInsnIndex).merge0(insnIndex, targetInsnIndex, interpreter)
        inInstructionsToProcess(targetInsnIndex) = true

        instructionsToProcess(numInstructionsToProcess) = targetInsnIndex
        numInstructionsToProcess = numInstructionsToProcess + 1
      }
    }

    if ((method.access & (Opcodes.ACC_ABSTRACT | Opcodes.ACC_NATIVE)) != 0) {
      return new Array[Frame[_]](0).asInstanceOf[Array[Frame[V]]]
    }

    // For each exception handler, and each instruction within its range, record in 'handlers' the
    // fact that execution can flow from this instruction to the exception handler.

    for(tryCatchBlock <- method.tryCatchBlocks.asScala){
      val startIndex = insnList.indexOf(tryCatchBlock.start)
      val endIndex = insnList.indexOf(tryCatchBlock.end)
      for(j <- startIndex until endIndex){
        var insnHandlers = handlers(j)
        if (insnHandlers == null) {
          insnHandlers = new mutable.ArrayBuffer[TryCatchBlockNode]()
          handlers(j) = insnHandlers
        }
        insnHandlers.append(tryCatchBlock)
      }
    }
    // Initializes the data structures for the control flow analysis.
    val currentFrame = computeInitialFrame(owner, method, interpreter)
    merge(0, 0, currentFrame)
    // Control flow analysis.

    while (numInstructionsToProcess > 0) {
      // Get and remove one instruction from the list of instructions to process.
      numInstructionsToProcess -= 1
      val insnIndex = instructionsToProcess(numInstructionsToProcess)
      val oldFrame = frames(insnIndex)
      inInstructionsToProcess(insnIndex) = false
      // Simulate the execution of this instruction.
      var insnNode: AbstractInsnNode = null
      try {
        insnNode = method.instructions.get(insnIndex)
        val insnOpcode = insnNode.getOpcode
        val insnType = insnNode.getType
        insnType match{
          case AbstractInsnNode.LABEL | AbstractInsnNode.LINE | AbstractInsnNode.FRAME =>
            merge(insnIndex, insnIndex + 1, oldFrame)
          case _ =>
            currentFrame.init(oldFrame).execute(insnNode, interpreter)
            insnNode match {
              case jumpInsn: JumpInsnNode =>
                if (insnOpcode != Opcodes.GOTO) merge(insnIndex, insnIndex + 1, currentFrame)
                val jumpInsnIndex = insnList.indexOf(jumpInsn.label)
                merge(insnIndex, jumpInsnIndex, currentFrame)

              case lookupSwitchInsn: LookupSwitchInsnNode =>
                var targetInsnIndex = insnList.indexOf(lookupSwitchInsn.dflt)
                merge(insnIndex, targetInsnIndex, currentFrame)
                for(label <- lookupSwitchInsn.labels.iterator().asScala){
                  targetInsnIndex = insnList.indexOf(label)
                  merge(insnIndex, targetInsnIndex, currentFrame)
                }

              case tableSwitchInsn: TableSwitchInsnNode =>
                var targetInsnIndex = insnList.indexOf(tableSwitchInsn.dflt)
                merge(insnIndex, targetInsnIndex, currentFrame)
                for(label <- tableSwitchInsn.labels.iterator().asScala){
                  targetInsnIndex = insnList.indexOf(label)
                  merge(insnIndex, targetInsnIndex, currentFrame)
                }

              case _ =>
                if (insnOpcode != Opcodes.ATHROW &&
                    (insnOpcode < Opcodes.IRETURN || insnOpcode > Opcodes.RETURN)) {
                  merge(insnIndex, insnIndex + 1, currentFrame)
                }
            }
        }
        val insnHandlers = handlers(insnIndex)
        if (insnHandlers != null) {
          for (tryCatchBlock <- insnHandlers) {
            val catchType =
              if (tryCatchBlock.`type` == null) Type.getObjectType("java/lang/Throwable")
              else Type.getObjectType(tryCatchBlock.`type`)

            val handler = new Frame[V](oldFrame)
            handler.clearStack()
            handler.push(interpreter.newExceptionValue(tryCatchBlock, catchType))
            merge(insnIndex, insnList.indexOf(tryCatchBlock.handler), handler)
          }
        }
      } catch {
        case e: AnalyzerException =>
          throw new AnalyzerException(e.node, "Error at instruction " + insnIndex + ": " + e.getMessage, e)
      }
    }

    frames
  }

  /**
    * Computes the initial execution stack frame of the given method.
    *
    * @param owner  the internal name of the class to which 'method' belongs.
    * @param method the method to be analyzed.
    * @return the initial execution stack frame of the 'method'.
    */
  private def computeInitialFrame[V <: Value](owner: String,
                                              method: MethodNode,
                                              interpreter: Interpreter[V]) = {
    val frame = new Frame[V](method.maxLocals, method.maxStack)
    var currentLocal = 0
    val isInstanceMethod = (method.access & Opcodes.ACC_STATIC) == 0
    if (isInstanceMethod) {
      val ownerType = Type.getObjectType(owner)
      frame.setLocal(currentLocal, interpreter.newParameterValue(currentLocal, ownerType))
      currentLocal += 1
    }
    val argumentTypes = Type.getArgumentTypes(method.desc)
    for (argumentType <- argumentTypes) {
      frame.setLocal(currentLocal, interpreter.newParameterValue(currentLocal, argumentType))
      currentLocal += 1
      if (argumentType.getSize == 2) {
        frame.setLocal(currentLocal, interpreter.newEmptyValue(currentLocal))
        currentLocal += 1
      }
    }
    while (currentLocal < method.maxLocals) {
      frame.setLocal(currentLocal, interpreter.newEmptyValue(currentLocal))
      currentLocal += 1
    }
    frame
  }

}
