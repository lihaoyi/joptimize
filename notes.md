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
