package joptimize.frontend

import joptimize.analyzer.Renderer
import org.objectweb.asm.{Opcodes, Type}
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Frame => _, Interpreter => _, _}

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Fork of {@link org.objectweb.asm.tree.analysis.Analyzer},
  * to give us a bit more flexibility in keeping track of the control flow
  * graph's incoming edges during the {@link Interpreter#merge}
  */
object DataflowExecutor {
  def analyze[V <: Value, S, B, M](
    owner: String,
    method: MethodNode,
    blockStartStates: IndexedSeq[Option[S]],
    interpreter: Interpreter[V, S, B, M],
    start: B,
    firstMerge: M
  ): (Frame[V, S, B, M], Array[Frame[V, S, B, M]]) = {

    /** The instructions of the currently analyzed method. */
    val insnList = method.instructions

    /** The size of {@link #insnList}. */
    val insnListSize = insnList.size()

    /** The exception handlers of the currently analyzed method (one list per instruction index). */
    val handlers = new Array[mutable.Buffer[TryCatchBlockNode]](insnListSize)

    /** The execution stack frames of the currently analyzed method (one per instruction index). */
    val frames = new Array[Frame[V, S, B, M]](insnListSize)

    /** The instructions that remain to process (one boolean per instruction index). */
    val inInstructionsToProcess = new Array[Boolean](insnListSize)

    /** The indices of the instructions that remain to process in the currently analyzed method. */
    val instructionsToProcess = new Array[Int](insnListSize)

    /** The number of instructions that remain to process in the currently analyzed method. */
    var numInstructionsToProcess = 0

    def merge(insnIndex: Int, targetInsnIndex: Int, frame: Frame[V, S, B, M]) = {
      val oldFrame = frames(targetInsnIndex)
      if (oldFrame != null) oldFrame.merge(insnIndex, targetInsnIndex, frame, interpreter)
      else {
        frames(targetInsnIndex) = new Frame[V, S, B, M](frame)
        frames(targetInsnIndex).merge0(insnIndex, targetInsnIndex, interpreter)
        inInstructionsToProcess(targetInsnIndex) = true
        blockStartStates(targetInsnIndex).foreach { s =>
          frames(targetInsnIndex).state = s
        }
        instructionsToProcess(numInstructionsToProcess) = targetInsnIndex
        numInstructionsToProcess = numInstructionsToProcess + 1
      }
    }
    def forceMerge(targetInsnIndex: Int, frame: Frame[V, S, B, M], src: B, dest: M) = {
      frames(targetInsnIndex) = new Frame[V, S, B, M](frame)
      frames(targetInsnIndex).forceMerge0(targetInsnIndex, interpreter, src, dest)
      inInstructionsToProcess(targetInsnIndex) = true
      blockStartStates(targetInsnIndex).foreach { s =>
        frames(targetInsnIndex).state = s
      }
      instructionsToProcess(numInstructionsToProcess) = targetInsnIndex
      numInstructionsToProcess = numInstructionsToProcess + 1
    }


    // For each exception handler, and each instruction within its range, record in 'handlers' the
    // fact that execution can flow from this instruction to the exception handler.

    for (tryCatchBlock <- method.tryCatchBlocks.asScala) {
      val startIndex = insnList.indexOf(tryCatchBlock.start)
      val endIndex = insnList.indexOf(tryCatchBlock.end)
      for (j <- startIndex until endIndex) {
        var insnHandlers = handlers(j)
        if (insnHandlers == null) {
          insnHandlers = new mutable.ArrayBuffer[TryCatchBlockNode]()
          handlers(j) = insnHandlers
        }
        insnHandlers.append(tryCatchBlock)
      }
    }
    // Initializes the data structures for the control flow analysis.
    val startFrame = computeInitialFrame(owner, method, interpreter)
    if ((method.access & (Opcodes.ACC_ABSTRACT | Opcodes.ACC_NATIVE)) != 0) {
      return (startFrame, new Array[Frame[V, S, B, M]](0))
    }

    forceMerge(0, startFrame, start, firstMerge)
    // Control flow analysis.

    while (numInstructionsToProcess > 0) {
      // Get and remove one instruction from the list of instructions to process.
      numInstructionsToProcess -= 1
      val insnIndex = instructionsToProcess(numInstructionsToProcess)
      val currentFrame = frames(insnIndex)
      inInstructionsToProcess(insnIndex) = false
      // Simulate the execution of this instruction.
      var insnNode: AbstractInsnNode = null
      try {
        insnNode = method.instructions.get(insnIndex)

        val insnOpcode = insnNode.getOpcode
        val insnType = insnNode.getType

        insnType match {
          case AbstractInsnNode.LABEL | AbstractInsnNode.LINE | AbstractInsnNode.FRAME =>
            merge(insnIndex, insnIndex + 1, currentFrame)

          case _ =>
            currentFrame.execute(insnNode, interpreter, blockStartStates(insnIndex))

            insnNode match {
              case jumpInsn: JumpInsnNode =>
                if (insnOpcode != Opcodes.GOTO) merge(insnIndex, insnIndex + 1, currentFrame)
                val jumpInsnIndex = insnList.indexOf(jumpInsn.label)
                merge(insnIndex, jumpInsnIndex, currentFrame)

              case lookupSwitchInsn: LookupSwitchInsnNode =>
                merge(insnIndex, insnList.indexOf(lookupSwitchInsn.dflt), currentFrame)
                for (label <- lookupSwitchInsn.labels.iterator().asScala) {
                  merge(insnIndex, insnList.indexOf(label), currentFrame)
                }

              case tableSwitchInsn: TableSwitchInsnNode =>
                merge(insnIndex, insnList.indexOf(tableSwitchInsn.dflt), currentFrame)
                for (label <- tableSwitchInsn.labels.iterator().asScala) {
                  merge(insnIndex, insnList.indexOf(label), currentFrame)
                }

              case _ =>
                if (insnOpcode != Opcodes.ATHROW &&
                  (insnOpcode < Opcodes.IRETURN || insnOpcode > Opcodes.RETURN)) {
                  merge(insnIndex, insnIndex + 1, currentFrame)
                }
            }
        }
//        val insnHandlers = handlers(insnIndex)
//        if (insnHandlers != null) {
//          for (tryCatchBlock <- insnHandlers) {
//            val catchType =
//              if (tryCatchBlock.`type` == null) Type.getObjectType("java/lang/Throwable")
//              else Type.getObjectType(tryCatchBlock.`type`)
//
//            val handler = new Frame[V, S](currentFrame)
//            handler.clearStack()
//            handler.push(interpreter.newExceptionValue(tryCatchBlock, catchType))
//            merge(insnIndex, insnList.indexOf(tryCatchBlock.handler), handler)
//          }
//        }
      } catch {
        case e: AnalyzerException =>
          throw new AnalyzerException(
            e.node,
            "Error at instruction " + insnIndex + ": " + e.getMessage,
            e
          )
      }
    }

    (startFrame, frames)
  }

  /**
    * Computes the initial execution stack frame of the given method.
    *
    * @param owner  the internal name of the class to which 'method' belongs.
    * @param method the method to be analyzed.
    * @return the initial execution stack frame of the 'method'.
    */
  private def computeInitialFrame[V <: Value, S, B, M](
    owner: String,
    method: MethodNode,
    interpreter: Interpreter[V, S, B, M]
  ) = {
    val frame = new Frame[V, S, B, M](method.maxLocals, method.maxStack)
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
