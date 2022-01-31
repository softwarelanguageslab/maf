# MAF Incremental Benchmark Suite

This directory contains the MAF Incremental Benchmark suite.
This suite contains programs annotated with change expressions.

To execute these programs in a standard Scheme interpreter, macros can be used:

* To execute the original program: 
```scheme 
(define-syntax <change>
  (syntax-rules ()
    ((<change> x y) x)))
```
* To execute the modified program:
```scheme 
(define-syntax <change>
  (syntax-rules ()
    ((<change> x y) y)))
```

The incremental benchmark suite contains programs for a [R5RS Scheme](../../test/changes/scheme) (adapted to work with our own R5RS Scheme semantics which [deviate](../../docs/INCOMPATIBILITIES.md) at certain points from the standard).
Overall, there are four subparts of the benchmarking suite:
* Benchmarking programs for a parallel Scheme language with threads can be found [here](../../test/changes/cscheme).
* Benchmarking programs for a Scheme language supporting assertions can be found [here](../../test/changes/scheme/assertions).
* [This directory](../../test/changes/scheme) contains a set of curated benchmarking programs with changes corresponding to real program changes.
* A set of 950 generated benchmarking programs is located [here](../../test/changes/scheme/generated). During the generation of benchmarks, changes to programs were made with a certain probability. For every initial program, five mutations have been conceived.