# Primary Tasks

- Dead code elimination

- Optimization

- Obfuscation (Mostly just renaming things)

# Optimization Plan

- Specialize methods by duplicating them based on narrow callsite-types of
  arguments

  - Including lambdas, whose concrete implementation types can be fished out
    from the invokedynamic method-handle and contains their concrete
    implementation

  - Some method calls will not have concrete callsite-types, e.g. if an argument
    comes from a variable field whose concrete type can vary at runtime. That's
    ok, we just pick the narrowest type possible

  - Return types to methods are also specialized; if the narrowed argument types
    to a duplicated method mean the return type is also narrowed, this is done
    and made available to downstream code making use of the method return value
    (requires dead code elimination)

- Specialization is recursive; if a specialized method calls downstream methods
  and forwards along arguments, the downstream methods are also duplicated based
  on the narrowed types of the arguments

- Optimize based on the now duplicated code
  - Inlining
  - Constant folding
  - Allocation Sinking
  - Partial Evaluation
  - Dead Code Elimination

- Possible to optimize:
  - Chains of Options
  - Chains of Eithers
  - Chains of futures???

- Probably not possible to optimize:
  - Fusing collection traversal (changes semantics in presence of exceptions)
    but probably can fuse iterator traversal.

# Optimization internals

- Single pass dataflow-ordered abstract interpretation
  - Dead code elimination

  - Partial evaluation

  - Specialization

  - Constant folding (replace instructions with pops + const)

  - Purity analysis

- Post-single-pass method-level instruction-level liveness cleanup (backwards
  dataflow order)
  - Walk backwards from method terminals (returns + impure method calls) to find
    all live values/instructions

  - Remove all other instructions!

  - Pure methods do not count as terminals; if their return value is not used,
    they can be eliminated

  - Liveness cleanup doesn't feed back into DCE/specialization: instructions
    cleaned up here can only be dataflow-upstream of other liveness-cleaned-up
    instructions, which will get also eliminated automatically

  - Unused arguments can be eliminated from the method signature, and the
    eliminated arguments can then be returned to the caller: they will be
    eliminated from the callsite, and used in the caller's analysis to
    participate in it's liveness cleanup

- Post-liveness method-level DCE
  - Delete any methods which were reachable but failed during liveness analysis

  - Doesn't feed back into DCE/specialization or instruction-level liveness
    cleanup

# Liveness cleanup

- Core considerations:
    - Returned values must be computed with the same input
    - Side effects (both reads & writes!) must happen in the same order

Stack/Local bytecode -> Dataflow graph -> Stack/Local bytecode

- Capture dataflow graph of `LValue`s depending on each other, in
  `Interpreter[LValue]`

- Convert flat list of instructions into controlflow graph of basic blocks

- Within each basic block, each side effecting instruction must be called
  and in the same original order with the same arguments

- Traverse the dataflow graph upstream from all terminal instructions to
  find the set of live `LValue`s:

    - *RETURN VALUE
    - Method calls to non-pure methods (ARGS...)
    - PUTSTATIC VALUE
    - PUTFIELD VALUE, *ASTORE VALUE for escaping objects only
    - RETURN

- Re-generate the basic block, in order of terminal instructions, in order
  to ensure each terminal instruction is called with the correct LValues,
  and the necessary LValues are left on the stack/locals for downstream
  basic blocks.
    - Walk dataflow graph upstream from that terminal instruction and find its
      transitive closure
        - For terminal instructions which do not require input, they can simply
          be emitted immediately!

        - We treat conditional jumps as a transformation on all values in the
          frame that passes through it, in order to force them to be included
          within the dataflow graph. Unconditional jumps are discarded.

    - Find expressions: segments of the subgraph which are tree-shaped:
      all nodes in the graph have only one downstream use case, except the
      terminal node which may have one or more.

    - Every expression can be evaluated purely on the stack, loading and
      evaluating sub-expressions smallest-to-largest starting from the left-most
      smallest expression

    - Evaluation between expressions cannot be evaluated purely on the stack,
      and must use either local variables, or DUP bytecodes

- Any un-used input arguments are removed from the method signature, and that
  change propagated to the caller method's callsite.