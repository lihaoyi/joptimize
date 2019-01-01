package forked;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.IincInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LookupSwitchInsnNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.TableSwitchInsnNode;
import org.objectweb.asm.tree.TryCatchBlockNode;
import org.objectweb.asm.tree.VarInsnNode;
import org.objectweb.asm.tree.analysis.*;

/**
 * Fork of {@link org.objectweb.asm.tree.analysis.Analyzer},
 * to give us a bit more flexibility in keeping track of the control flow
 * graph's incoming edges during the {@link Interpreter#merge}
 */
public class Analyzer<V extends Value> implements Opcodes {

    /** The interpreter to use to symbolically interpret the bytecode instructions. */
    private final Interpreter<V> interpreter;

    /** The instructions of the currently analyzed method. */
    private InsnList insnList;

    /** The size of {@link #insnList}. */
    private int insnListSize;

    /** The exception handlers of the currently analyzed method (one list per instruction index). */
    private List<TryCatchBlockNode>[] handlers;

    /** The execution stack frames of the currently analyzed method (one per instruction index). */
    private Frame<V>[] frames;

    /** The instructions that remain to process (one boolean per instruction index). */
    private boolean[] inInstructionsToProcess;

    /** The indices of the instructions that remain to process in the currently analyzed method. */
    private int[] instructionsToProcess;

    /** The number of instructions that remain to process in the currently analyzed method. */
    private int numInstructionsToProcess;

    /**
     * Constructs a new {@link org.objectweb.asm.tree.analysis.Analyzer}.
     *
     * @param interpreter the interpreter to use to symbolically interpret the bytecode instructions.
     */
    public Analyzer(final Interpreter<V> interpreter) {
        this.interpreter = interpreter;
    }

    /**
     * Analyzes the given method.
     *
     * @param owner the internal name of the class to which 'method' belongs.
     * @param method the method to be analyzed.
     * @return the symbolic state of the execution stack frame at each bytecode instruction of the
     *     method. The size of the returned array is equal to the number of instructions (and labels)
     *     of the method. A given frame is {@literal null} if and only if the corresponding
     *     instruction cannot be reached (dead code).
     * @throws AnalyzerException if a problem occurs during the analysis.
     */
    @SuppressWarnings("unchecked")
    public Frame<V>[] analyze(final String owner, final MethodNode method) throws AnalyzerException {
        if ((method.access & (ACC_ABSTRACT | ACC_NATIVE)) != 0) {
            frames = (Frame<V>[]) new Frame<?>[0];
            return frames;
        }
        insnList = method.instructions;
        insnListSize = insnList.size();
        handlers = (List<TryCatchBlockNode>[]) new List<?>[insnListSize];
        frames = (Frame<V>[]) new Frame<?>[insnListSize];
        inInstructionsToProcess = new boolean[insnListSize];
        instructionsToProcess = new int[insnListSize];
        numInstructionsToProcess = 0;

        // For each exception handler, and each instruction within its range, record in 'handlers' the
        // fact that execution can flow from this instruction to the exception handler.
        for (int i = 0; i < method.tryCatchBlocks.size(); ++i) {
            TryCatchBlockNode tryCatchBlock = method.tryCatchBlocks.get(i);
            int startIndex = insnList.indexOf(tryCatchBlock.start);
            int endIndex = insnList.indexOf(tryCatchBlock.end);
            for (int j = startIndex; j < endIndex; ++j) {
                List<TryCatchBlockNode> insnHandlers = handlers[j];
                if (insnHandlers == null) {
                    insnHandlers = new ArrayList<TryCatchBlockNode>();
                    handlers[j] = insnHandlers;
                }
                insnHandlers.add(tryCatchBlock);
            }
        }

        // Initializes the data structures for the control flow analysis.
        Frame<V> currentFrame = computeInitialFrame(owner, method);
        merge(0, 0, currentFrame);
        init(owner, method);

        // Control flow analysis.
        while (numInstructionsToProcess > 0) {
            // Get and remove one instruction from the list of instructions to process.
            int insnIndex = instructionsToProcess[--numInstructionsToProcess];
            Frame<V> oldFrame = frames[insnIndex];
            inInstructionsToProcess[insnIndex] = false;

            // Simulate the execution of this instruction.
            AbstractInsnNode insnNode = null;
            try {
                insnNode = method.instructions.get(insnIndex);
                int insnOpcode = insnNode.getOpcode();
                int insnType = insnNode.getType();

                if (insnType == AbstractInsnNode.LABEL
                        || insnType == AbstractInsnNode.LINE
                        || insnType == AbstractInsnNode.FRAME) {
                    merge(insnIndex, insnIndex + 1, oldFrame);
                } else {
                    currentFrame.init(oldFrame).execute(insnNode, interpreter);

                    if (insnNode instanceof JumpInsnNode) {
                        JumpInsnNode jumpInsn = (JumpInsnNode) insnNode;
                        if (insnOpcode != GOTO) {
                            currentFrame.initJumpTarget(insnOpcode, /* target = */ null);
                            merge(insnIndex, insnIndex + 1, currentFrame);
                        }
                        int jumpInsnIndex = insnList.indexOf(jumpInsn.label);
                        currentFrame.initJumpTarget(insnOpcode, jumpInsn.label);
                        merge(insnIndex, jumpInsnIndex, currentFrame);
                    } else if (insnNode instanceof LookupSwitchInsnNode) {
                        LookupSwitchInsnNode lookupSwitchInsn = (LookupSwitchInsnNode) insnNode;
                        int targetInsnIndex = insnList.indexOf(lookupSwitchInsn.dflt);
                        currentFrame.initJumpTarget(insnOpcode, lookupSwitchInsn.dflt);
                        merge(insnIndex, targetInsnIndex, currentFrame);
                        for (int i = 0; i < lookupSwitchInsn.labels.size(); ++i) {
                            LabelNode label = lookupSwitchInsn.labels.get(i);
                            targetInsnIndex = insnList.indexOf(label);
                            currentFrame.initJumpTarget(insnOpcode, label);
                            merge(insnIndex, targetInsnIndex, currentFrame);
                        }
                    } else if (insnNode instanceof TableSwitchInsnNode) {
                        TableSwitchInsnNode tableSwitchInsn = (TableSwitchInsnNode) insnNode;
                        int targetInsnIndex = insnList.indexOf(tableSwitchInsn.dflt);
                        currentFrame.initJumpTarget(insnOpcode, tableSwitchInsn.dflt);
                        merge(insnIndex, targetInsnIndex, currentFrame);
                        for (int i = 0; i < tableSwitchInsn.labels.size(); ++i) {
                            LabelNode label = tableSwitchInsn.labels.get(i);
                            currentFrame.initJumpTarget(insnOpcode, label);
                            targetInsnIndex = insnList.indexOf(label);
                            merge(insnIndex, targetInsnIndex, currentFrame);
                        }
                    } else if (insnOpcode != ATHROW && (insnOpcode < IRETURN || insnOpcode > RETURN)) {
                        merge(insnIndex, insnIndex + 1, currentFrame);
                    }
                }

                List<TryCatchBlockNode> insnHandlers = handlers[insnIndex];
                if (insnHandlers != null) {
                    for (TryCatchBlockNode tryCatchBlock : insnHandlers) {
                        Type catchType;
                        if (tryCatchBlock.type == null) {
                            catchType = Type.getObjectType("java/lang/Throwable");
                        } else {
                            catchType = Type.getObjectType(tryCatchBlock.type);
                        }
                        if (newControlFlowExceptionEdge(insnIndex, tryCatchBlock)) {
                            Frame<V> handler = newFrame(oldFrame);
                            handler.clearStack();
                            handler.push(interpreter.newExceptionValue(tryCatchBlock, handler, catchType));
                            merge(insnIndex, insnList.indexOf(tryCatchBlock.handler), handler);
                        }
                    }
                }
            } catch (AnalyzerException e) {
                throw new AnalyzerException(
                        e.node, "Error at instruction " + insnIndex + ": " + e.getMessage(), e);
            } catch (RuntimeException e) {
                // DontCheck(IllegalCatch): can't be fixed, for backward compatibility.
                throw new AnalyzerException(
                        insnNode, "Error at instruction " + insnIndex + ": " + e.getMessage(), e);
            }
        }

        return frames;
    }

    /**
     * Computes the initial execution stack frame of the given method.
     *
     * @param owner the internal name of the class to which 'method' belongs.
     * @param method the method to be analyzed.
     * @return the initial execution stack frame of the 'method'.
     */
    private Frame<V> computeInitialFrame(final String owner, final MethodNode method) {
        Frame<V> frame = newFrame(method.maxLocals, method.maxStack);
        int currentLocal = 0;
        boolean isInstanceMethod = (method.access & ACC_STATIC) == 0;
        if (isInstanceMethod) {
            Type ownerType = Type.getObjectType(owner);
            frame.setLocal(
                    currentLocal, interpreter.newParameterValue(isInstanceMethod, currentLocal, ownerType));
            currentLocal++;
        }
        Type[] argumentTypes = Type.getArgumentTypes(method.desc);
        for (Type argumentType : argumentTypes) {
            frame.setLocal(
                    currentLocal,
                    interpreter.newParameterValue(isInstanceMethod, currentLocal, argumentType));
            currentLocal++;
            if (argumentType.getSize() == 2) {
                frame.setLocal(currentLocal, interpreter.newEmptyValue(currentLocal));
                currentLocal++;
            }
        }
        while (currentLocal < method.maxLocals) {
            frame.setLocal(currentLocal, interpreter.newEmptyValue(currentLocal));
            currentLocal++;
        }
        frame.setReturn(interpreter.newReturnTypeValue(Type.getReturnType(method.desc)));
        return frame;
    }

    /**
     * Returns the symbolic execution stack frame for each instruction of the last analyzed method.
     *
     * @return the symbolic state of the execution stack frame at each bytecode instruction of the
     *     method. The size of the returned array is equal to the number of instructions (and labels)
     *     of the method. A given frame is {@literal null} if the corresponding instruction cannot be
     *     reached, or if an error occurred during the analysis of the method.
     */
    public Frame<V>[] getFrames() {
        return frames;
    }

    /**
     * Returns the exception handlers for the given instruction.
     *
     * @param insnIndex the index of an instruction of the last analyzed method.
     * @return a list of {@link TryCatchBlockNode} objects.
     */
    public List<TryCatchBlockNode> getHandlers(final int insnIndex) {
        return handlers[insnIndex];
    }

    /**
     * Initializes this analyzer. This method is called just before the execution of control flow
     * analysis loop in #analyze. The default implementation of this method does nothing.
     *
     * @param owner the internal name of the class to which the method belongs.
     * @param method the method to be analyzed.
     * @throws AnalyzerException if a problem occurs.
     */
    protected void init(final String owner, final MethodNode method) throws AnalyzerException {
        // Nothing to do.
    }

    /**
     * Constructs a new frame with the given size.
     *
     * @param numLocals the maximum number of local variables of the frame.
     * @param numStack the maximum stack size of the frame.
     * @return the created frame.
     */
    protected Frame<V> newFrame(final int numLocals, final int numStack) {
        return new Frame<V>(numLocals, numStack);
    }

    /**
     * Constructs a copy of the given frame.
     *
     * @param frame a frame.
     * @return the created frame.
     */
    protected Frame<V> newFrame(final Frame<? extends V> frame) {
        return new Frame<V>(frame);
    }

    /**
     * Creates a control flow graph edge corresponding to an exception handler. The default
     * implementation of this method does nothing. It can be overridden in order to construct the
     * control flow graph of a method (this method is called by the {@link #analyze} method during its
     * visit of the method's code).
     *
     * @param insnIndex an instruction index.
     * @param successorIndex index of a successor instruction.
     * @return true if this edge must be considered in the data flow analysis performed by this
     *     analyzer, or false otherwise. The default implementation of this method always returns
     *     true.
     */
    protected boolean newControlFlowExceptionEdge(final int insnIndex, final int successorIndex) {
        return true;
    }

    /**
     * Creates a control flow graph edge corresponding to an exception handler. The default
     * implementation of this method delegates to {@link #newControlFlowExceptionEdge(int, int)}. It
     * can be overridden in order to construct the control flow graph of a method (this method is
     * called by the {@link #analyze} method during its visit of the method's code).
     *
     * @param insnIndex an instruction index.
     * @param tryCatchBlock TryCatchBlockNode corresponding to this edge.
     * @return true if this edge must be considered in the data flow analysis performed by this
     *     analyzer, or false otherwise. The default implementation of this method delegates to {@link
     *     #newControlFlowExceptionEdge(int, int)}.
     */
    protected boolean newControlFlowExceptionEdge(
            final int insnIndex, final TryCatchBlockNode tryCatchBlock) {
        return newControlFlowExceptionEdge(insnIndex, insnList.indexOf(tryCatchBlock.handler));
    }

    // -----------------------------------------------------------------------------------------------

    /**
     * Merges the given frame and subroutine into the frame and subroutines at the given instruction
     * index. If the frame or the subroutine at the given instruction index changes as a result of
     * this merge, the instruction index is added to the list of instructions to process (if it is not
     * already the case).
     *
     * @param targetInsnIndex an instruction index.
     * @param frame a frame. This frame is left unchanged by this method.
     * @throws AnalyzerException if the frames have incompatible sizes.
     */
    private void merge(final int insnIndex, final int targetInsnIndex, final Frame<V> frame)
            throws AnalyzerException {
        boolean changed;
        Frame<V> oldFrame = frames[targetInsnIndex];
        if (oldFrame == null) {
            frames[targetInsnIndex] = newFrame(frame);
            frames[targetInsnIndex].merge0(insnIndex, interpreter);
            changed = true;
        } else {
            changed = oldFrame.merge(insnIndex, frame, interpreter);
        }
        if (changed && !inInstructionsToProcess[targetInsnIndex]) {
            inInstructionsToProcess[targetInsnIndex] = true;
            instructionsToProcess[numInstructionsToProcess++] = targetInsnIndex;
        }
    }
}
