# JOptimize: the missing Java optimizer

JOptimize is an aggressive whole-program optimizer for the Java Virtual
Machine. It takes advantage of the fact that most Java programs make minimal use
of runtime classloading, meaning large portions of the program can be analyzed
and optimized ahead of time before the program starts running. JOptimize focuses
heavily on interprocedural optimizations that the JVM does not do well, which
are increasingly important in modern Java and other JVM languages such as Scala
or Kotlin.

JOptimize's optimization pipeline looks roughly as follows:

1. Construction of SSA IR from Java Bytecode 
2. Optimization Passes on the SSA IR 
3. Java Bytecode Generation from the SSA IR

## Construction of SSA IR

Construction of the SSA IR from Java Bytecode is straightforward: we do a
one-pass abstract interpretation of the stack-based bytecode, emulating the
operand stack and local variables, and generating `SSA.Node`s instead of
performing actual operations on the abstract values. 

The `SSA.Node`s that comprise the IR form a double linked graph, as a simple
tree-based or DAG-based IR does not easily capture the circular nature of the
program dependence graph caused by loops, and simple Tree/DAG transformations
lose most of their elegance and convenience when performed on cyclic graphs.

In place of simple recursive transformation of Trees/DAGs, the doubly-linked
graph is primarily operated on via fixed-point graph traversal: the callback
processing each node returns a list of subsequent nodes to process, which are
then processed in turn, until there are no more nodes to process. The
flexibility each callback has to decide which nodes it wants to return for
further processing gives us the flexiblity to traverse the graph in different
ways:

- In forwards dataflow order, by returning downstream nodes 
- In backwards dataflow order, by returning upstream nodes 
- Bidirectionally, by returning both upstream and downstream nodes

## Optimization Passes

JOptimize performs three main sets of optimizations:

1. "Traditional" optimizations: constant folding, dead/unreachable code
   elimination, loop invariant code hoisting, copy coalescing, inlining, etc.

2. Tracing code specialization: this aims to find "traces" throughout your
   program where a single dynamic check up-front allows us to elide a whole
   series of dynamic checks further down the line, allowing the specialized code
   to be much more aggressively optimized.

3. Aggressive partial evaluation: we maintain a detailed and complete model of
   the JVM runtime at optimization time, allowing us to extend basic constant
   folding to encompass class instances, array, and their contents.

### Traditional Optimizations

JOptimize is forced to duplicate many of the existing JVM HotSpot optimizations
for two reasons:

- Optimizations are mutually reinforcing, and having the traditional
  optimizations happen concurrently with tracing specialization and partial
  evaluation results in better results than having them only occur later

- The JVM has limit on how much optimization it can perform: inlining recursion
  limits, bytecode size limits, etc.. Messy bytecode can harm HotSpot's ability
  to optimize even if it "should" be able to perform those optimizations itself

For these reasons, JOptimize as much as possible does the JVM's optimization
work during its own optimization passes.

### Tracing Specialization

Tracing specialization is the first way in which JOptimize aims to go beyond
that HotSpot optimizer. HotSpot is a method-ad-a-time optimizer, which means
that each source method corresponds to a single optimized method body. This
limits how much a method body can be optimized, as although each callsite may
only use a subset of the called method's functionality, the method itself has to
assume it is being used in its full genericity and cannot adapt itself to each
callsite.

The JVM does perform some specialization when it inlines methods, but inlining
has its own issues (e.g. increased code size) that prevent it from being used
aggressively.

The key to tracing specialization is that often multiple dynamic checks (whether
a virtual dispatch, conditional jump, etc.) can be replaced by a single dynamic
check and then a sequence of straight-line code. For example,

- A method callsite with constant arguments may already statically determine the
  result of multiple conditionals in the method body

- A method that is defined to take an abstract interface may be called with a
  concrete type, and all virtual/interface method calls to that argument in the
  method body would already be statically determined.

- Higher-order functions taking functions as arguments often know what function
  is passed at each callsite, and could be specialized to avoid having a slow
  interface method dispatch that is often an optimization boundary.

- A method called with narrower argument types may return a value of a narrower
  result type, which may then be passed into other methods and narrow their
  callsites.

Often this tracing spans multiple methods, which is a style of optimization the
current HotSpot JIT isn't well suited to support. Unlike inlining, tracing
specialization can share specialized copies of a method across multiple
callsites, as long as they are specialized in the same way. This helps limit the
increase in code size that inlining typically causes, and allows JOptimize to
use tracing specialization much more aggressively.

### Partial Evaluation

The last major pillar of JOptimize is a very aggressive partial evaluator.
HotSpot already does trivial constant folding for primitives values in local
variables, and branch elimination when those primitives can predict the jump
target. What it does not do is model loads and stores from instance fields or
arrays, thus forcing it to treat such instructions pessimistically as
optimization barriers. This is especially problematic in modern Java or other
JVM languages like Kotlin or Scala, which tend to make greater use of simple
data structures and wrapper types as opposed to just primitive types.

To properly partially evaluate code dealing with object instances and arrays,
JOptimize has a much richer model of constant-fold-able values than most
compilers. JOptimize is able to model arbitrarily deep object graphs with a
mixture of known/concrete and unknown/abstract objects and arrays.

This allows JOptimize to:

- Keep track of known values as they get passed in and out of simple data
  structures

- Partially evaluate non-trivial code at optimization time, even if it includes
  method calls, objects or arrays, leaving only code necessary to efficiently
  materialize the finished output at runtime

- Analyze the properties of non-trivial methods, e.g. marking them for purity to
  make them eligible for dead code elimination or loop-invariant code hoisting.

JOptimize's partial evaluation benefits greatly from tracing specialization,
which tends to produce code with many more known concrete values and types. It
also helps mitigate the code-size increase that limits aggressive use of tracing
specialization.

## Java Bytecode Generation

JOptimize does the bulk of its optimizations on the SSA IR, which allows it to
ignore many irrelevant details during the complex code transformations. These
concerns are left to the bytecode generation phase, which performs:

- Copy insertion, to correctly (but conservatively) convert Phi nodes into
  copies

- Copy Coalescing, to reduce the number of copies necessary

- Register allocation, to decide exactly where the so-far unnamed variables will
  be stored in the method's local variable table

Finally, the last step is to emit the Java Bytecode. This is done in two layers:

- The control-flow graph between basic blocks is traversed via a depth-first
  spanning tree. In addition to serializing the graph into a linear sequence,
  the depth-first order also automatically collapses empty basic blocks and
  removes a number of trivial jumps

- The named values within each basic block are traversed, and each one's
  dependent values is walked via a post-order traversal to generate the bytecode
  necessary to perform the computations on the operand stack. Values which are
  used more than once are stored in a local variable and loaded where necessary.

This final step gives us an executable Java program that when run should produce
the same output.
