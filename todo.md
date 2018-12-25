# Easy

- NULLness inference for reference types
    - Needed if we want to mark instance methods as elidable, because we need to
      be sure they do not throw NPEs.

    - Otherwise, we are forced to leave null-check-throw stubs in the bytecode,
      which forces enclosing methods to be non-elidable

- Stub Cleanup
    - Clean up all the CONST-POP pairs left behind by Walker and Liveness

    - Unclear if a peephole optimizer would be sufficient, or if we would need
      a full bytecode rewrite based on the LValue graph (doable but more work)

- Unused parameter elimination
    - Remove parameters from method signatures when they are unused within the
      method

    - All the bytecode leading to the callsite and from the parameter in the
      method body are already stubbed out, so just need to reshuffle the stack
      and remove the parameter itself

- Fine-grained purity tracking
    - Currently purity is a binary PURE (only affects locals or stack) or not
      (writing fields, statics, calling external methods, ...)

    - Separate out varying levels of purity: write static (cannot be elided),
      write to instance fields (may be elided if the locals do not escape),
      writes to parameters or return value fields (cannot be elided in this
      method body, but this call may be elided in enclosing method body if the
      parameter/return-value does not escape)

- Bytecode Assembler
    - Let us easily build ad-hoc bytecode sequences, so we can perform our
      transformations against fixed bytecode (unaffected by the java compiler)
      and also assert against the exact output we expect

    - Will be important once Stub Cleanup is in place, to ensure we are cleaning
      up all the stubs we expect despite the evalation output being the same

- Execution Logging
    - See what's happened in a run without going and adding printlns everywhere

    - Should be thorough enough to satisfy 90% of debugging needs just by
      browsing the output file

# Hard

- Recursive function property inference
    - Rather than inferring TOP when encountering recursive functions (no
      return type narrowing, no unused params), instead infer BOTTOM and let
      LUBing find the real inference

    - If there are no non-BOTTOM branches, LUBing infers bottom for the entire
      function, which is correct.

    - Need some way of performing the inference twice: first with the BOTTOM
      returned by the recursive call, then again with the proper inferred type

    - Will likely have to throw away some work, since we do code-generation &
      specialization at the same time as inference. Hopefully won't need to
      throw away too much

    - What about mutually recursive functions?

- Stb Lib property inference
    - Rather than treating the entire Std Lib conservatively as a black box,
      walk the code to analyze its properties

    - This would let us infer many common APIs as pure.