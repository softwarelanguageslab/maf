# Incremental Modular Analysis

The `maf.modular.incremental` package contains support for the incremental modular analysis of programs. This
implementation supports the following publications, which we also refer to for more information regarding our approach
and implementation:

* _Incremental Flow Analysis through Computational Dependency Reification_. SCAM
  2020. [pdf](http://soft.vub.ac.be/Publications/2020/vub-tr-soft-20-12.pdf). _Corresponding code release: `SCAM 2020`_

The implementation of the incremental modular analyses follows the structure of the 'standard', non-incremental modular
analyses provided by MAF. Hence, their implementation is modular and an analysis can be obtained by mixing in a number
of traits into the base class `IncrementalModAnalysis`.

## Configurations

An incremental analysis can be run with or without several precision-improving techniques that can be enabled or disabled
using a config parameter. The techniques can be enabled and disabled independely of one another. When all techniques are
disabled, changes to the program results upon an incremental update are fully monotonous.

## Provided instances

We already provide a number of instances of incremental modular analyses for Scheme. These instances can be found
in `maf.modular.incremental.scheme.SchemeAnalyses`. The following instances are available for ModF (non-exhaustive):

* `IncrementalSchemeModFAnalysisTypeLattice`: an incremental ModF analysis using a type lattice, a LIFO worklist
  algorithm, and a big-step semantics for Scheme,
* `IncrementalSchemeModFAnalysisCPLattice`: an incremental ModF analysis using a constant-propagation lattice, a LIFO
  worklist algorithm, and a big-step semantics for Scheme.

The following instances are available for ModConc (non-exhaustive):

* `IncrementalModConcAnalysisTypeLattice`: an incremental ModConc analysis using a type lattice, a LIFO worklist
  algorithm, and a small-step semantics for a concurrent Scheme.
* `IncrementalModConcAnalysisCPLattice`: an incremental ModConc analysis using a constant-propagation lattice, a LIFO
  worklist algorithm, and a small-step semantics for a concurrent Scheme.

For ModF, versions of the given instances that can perform an assertion analysis are also provided. The full details of
all instances is provided in JavaDoc.

## Tests

Several unit tests have been written for the incremental analysis. A part of these tests are run upon every commit to the code base.
All tests are run every night.

Overview of the incremental test suite:
* `IncrementalModXComparisonTests` compare the result of the incremental analysis/update to the result of a full reanalysis of the program by a non-incremental analysis.
* `IncrementalModXMachineryTests` test whether some small functionalities of the incremental analysis work correctly using small example programs.
* `IncrementalModXRestartTests` verify whether restarting an analysis after completion does not alter the result.
* `IncrementalModXSoundnessTests` verify whether the analysis result is sound when compared to the result of a concrete interpreter (or potentially multiple runs of this interpreter in case of concurrent programs).
* `IncrementalModXWLIndependenceTests` test whether the result of the analysis is independent of the work list order. **These tests are currently disabled.**

Tests normally verify (1) the initial analysis result, (2) the incremental update, and (3) the full reanalysis result.
Note that tests may be split into multiple classes, to allow the CI system to execute them within the given time bound.

Current status:

[![Daily Incremental MAF tests](https://github.com/softwarelanguageslab/maf/actions/workflows/dailyTestsIncremental.yml/badge.svg)](https://github.com/softwarelanguageslab/maf/actions/workflows/dailyTestsIncremental.yml)
[![Incremental MAF tests on action](https://github.com/softwarelanguageslab/maf/actions/workflows/testsOnActionIncremental.yml/badge.svg)](https://github.com/softwarelanguageslab/maf/actions/workflows/testsOnActionIncremental.yml)


## Benchmarks

The incremental analyses can be run on a specific suit of benchmarks, which are programs that have been annotated with
change expressions.
More information on these benchmarks can be found [here](../../../../../../../../test/changes/incrementalBenchmarks.md).
