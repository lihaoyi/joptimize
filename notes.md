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

- Optimize based on the now duplicated code to improve perf further, bring down
  the size-penalty of duplication

- Possible to optimize:
  - Chains of Options
  - Chains of Eithers
  - Chains of futures???

- Probably not possible to optimize:
  - Fusing collection traversal (changes semantics in presence of exceptions)
    but probably can fuse iterator traversal.

# Optimization internals

Four primary passes:

First pass: control-flow-ordered abstract interpretation & specialization
Second pass: method-level liveness analysis interleaved with first pass
Third pass: whole-program DCE for things left behind in second pass
Fourth pass: method-level bytecode cleanup

## First pass: control-flow-ordered abstract interpretation & specialization

- Dead code elimination: anything (bytecodes, methods, classes) that's not
  reachable is ignored.

- Partial evaluation: we perform an abstract interpretation using the most
  specific types available. Often these are concrete: e.g. ICONST_* instructions,
  ISINSTANCE for known types, etc. and we propagate the concrete constant-type
  instead of the abstract type

- Specialization: duplicate methods according to the narrower inferred types of
  their arguments.

- Constant folding: stub out instructions with constant results, using POPs +
  CONST. Chains of instructions with constant results become long chains of
  redundant POPs + CONSTs

- Purity analysis: if a method has no side effects (field read, field write,
  exceptions, IO, calls to impure methods) they are marked as pure for further
  analysis

- Flow-sensitive inference: called methods have their return types narrowed
  according to their input types, and the narrowed return types are then fed
  downstream and become the narrowed input types of downstream functions

## Second pass: method-level liveness analysis interleaved with first pass

- Walk backwards from method terminals (returns + impure method calls)

- Any instructions that do not turn up in that traversal are not live, and
  can be stubbed out with POPs + CONST

- Pure methods do not count as terminals; if their return value is not used,
  they can be eliminated

- Liveness cleanup doesn't feed back into DCE/specialization: instructions
  cleaned up here can only be dataflow-upstream of other liveness-cleaned-up
  instructions, which will get also eliminated automatically

- Unused arguments can be eliminated from the method signature, and the
  eliminated arguments can then be returned to the caller: they will be
  eliminated from the callsite, and used in the caller's analysis to
  participate in it's liveness cleanup

- Does not feed back into first pass, but does need to take place in a post
  order traversal similar to the first-pass's pre-order traversal, so it's
  natural to place it interleaved with the first pass running on every processed
  method body

## Third pass: whole-program DCE for things left behind in second pass

- Delete any methods which were reachable but failed during liveness analysis.
  We cannot perform this during the first or second passes, because we need all
  liveness analysis to be finished before we have this information

- Doesn't feed back into DCE/specialization or instruction-level liveness
  cleanup: anything deleted here is already known to be

## Fourth pass: method-level bytecode cleanup

- Clean up messy bytecodes we left behind in the First and Second passes

- We performed no significant transformation of the bytecode during those two
  passes: mostly just omission (e.g. ignoring dead code in the first pass, dead
  methods in the third), duplication (method-level and block-level specialization
  in the first pass) or stubbing (constant folding in the first pass, un-live
  code during the second pass).

- Earlier passes do their job fine without bytecode-level cleanup: specialization,
  constant folding & method-level liveness analysis. This lets us leave bytecode
  cleanup to its own phase that specializes in bytecode wrangling

- Can take place on every method independently, in parallel if necessary