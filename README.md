Modular Analysis Framework (MAF): A Framework for Modular Analysis of Dynamic Languages

# Goal

The goal of this artefact is to experiment with abstract machines and language semantics.
Currently, the artefact's implementation is focused towards experiments with modular analyses of (R5RS) Scheme programs.
All analyses live in the package [`maf.analysis.scheme`](code/src/main/scala/maf/analysis/scheme):

* `SchemeModFLocal` (standard instance: `SchemeDSSAnalysis`): every component carries its own (garbage-collected) local store, and the result of a call is a store delta;
* `SchemeModFLocalFS`: a variant in which the store of a component is joined over all of its calls (garbage collection can be disabled using the `gc` flag);
* `SchemeModFADI` (standard instance: `SchemeModFADIAnalysis`): a variant that threads a counting store through the analysis.

They share their semantics (`SchemeSemantics`), context-sensitivity policies (`SchemeModFLocalSensitivity`: context-insensitive or k-CFA call-site sensitive) and allocators (`CustomAllocator`), which can be found in the package [`maf.semantics.scheme`](code/src/main/scala/maf/semantics/scheme).

Additionally, semantics for R5RS Scheme are present, including a concrete interpreter that is used as an oracle for the soundness tests.
Note that the semantics of our implementation may deviate from the R5RS specification at certain points; these have been described [here](docs/INCOMPATIBILITIES.md).

## Code organisation

The source code (in [`code/src/main/scala/maf`](code/src/main/scala/maf)) is organised in the following packages:

| Package | Contents |
|---|---|
| `maf.util` | Generic utilities, such as monads, monoids, timeouts and file I/O. |
| `maf.syntax` | Expressions (program points with a unique label) and source positions. `sexp` contains S-expressions and their parser; `scheme` contains the Scheme AST, parser, compiler and prelude, and `scheme.transformations` the source-to-source transformations (undefiner, mutable variable boxer, lexical addresser and renamer). |
| `maf.domain` | Semantic domains: addresses, environments, stores, errors and abstract domains. `lattice` contains the lattice interfaces and their implementations (concrete, constant propagation, type, bounded); `scheme` contains the Scheme lattice, abstract domains, addresses and primitives. |
| `maf.semantics` | `scheme` contains the (monadic) abstract semantics of Scheme with its context-sensitivity policies and allocators; `scheme.interpreter` contains the concrete Scheme interpreters. |
| `maf.fixpoint` | The generic modular fixpoint machinery: `ModAnalysis`, work lists and worklist algorithms. |
| `maf.analysis` | The analysis results interface. `scheme` contains the analyses (`SchemeModFLocal`, `SchemeModFLocalFS` and `SchemeModFADI`). |
| `maf.runners` | Ready-to-run programs and the catalogue of benchmark programs. `precision` and `performance` contain the experiments. |

The test suite (in [`code/src/test/scala/maf/test`](code/src/test/scala/maf/test)) follows the same structure.

# Usage

## Analysing a program

To run a modular analysis, you compose an analysis with an abstract domain, a context-sensitivity policy and a worklist algorithm.

The analyses do not support `set!` directly, so a program first needs to be parsed, extended with the required prelude definitions, rewritten to use explicit boxes for mutable variables, and undefined:
```scala
val parsed = SchemeParser.parseFile(path-to-file)
val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
val transf = SchemeMutableVarBoxer.transform(prelud)
val prog = SchemeParser.undefine(transf)
```

Then, for instance, to analyse `prog` using a 1-CFA analysis with a constant-propagation domain:
```scala
val analysis = new SchemeModFLocal(prog)
                 with SchemeConstantPropagationDomain
                 with SchemeModFLocalCallSiteSensitivity(1)
                 with FIFOWorklistAlgorithm[SchemeExp]
analysis.analyze()
```
Method `analyze` computes the full (and sound) analysis result for the program that the analysis is constructed with.
This method does not return the analysis results directly.
Rather, after calling `analyze`, the computed analysis results can be accessed through the properties of the `analysis` object (e.g., `analysis.results(analysis.MainComponent)`).
Mixing in `SchemeModFLocalAnalysisResults` (resp. `SchemeModFLocalFSAnalysisResults` and `SchemeModFADIAnalysisResults`) additionally collects the values per program location in `resultsPerIdn`.

Alternatively, one can use the `analyzeWithTimeout(<timeout>)` method to run the analysis with a given timeout. This timeout is obtained from a Java Duration
(using `Timeout.start(<duration>)`). 
The method returns when either the analysis has terminated or when the timeout has been reached (approximately, meaning in practice it may run a bit longer than the specified timeout). 
Extra care should be taken when using this method, as the (partial) analysis results are not guaranteed to be sound when the timeout is triggered. 
Therefore, when using this method, it is recommended to explicitly check afterwards if the analysis terminated using the `finished` method.

Some ready-to-run examples can be found in the package `maf.runners`, e.g.:
```sbtshell
maf/runMain maf.runners.AnalyzeProgram
```

## Running the experiments

The precision and performance experiments are found in the packages `maf.runners.precision` and `maf.runners.performance`.
For example, the following command compares the precision of the different analyses on the SAS benchmarks, writing the results as CSV files to the folder `benchOutput/precision`:
```sbtshell
maf/runMain maf.runners.precision.AllSASBenchmarks benchOutput/precision
```
The scripts in the folder `benchOutput` can be used to plot the results.

# Running the test suite
This repository is monitored by a CI-system. Upon every push and pull request to this repository, the test suite is run on a specific subset of benchmark programs (MAF tests on action). 
In addition, the full test suite is run over night (Daily MAF tests).

Current status:
<!-- https://github.com/badges/shields -->
![Latest build](https://github.com/softwarelanguageslab/maf/workflows/MAF%20tests%20on%20action/badge.svg)
![Nightly tests](https://github.com/softwarelanguageslab/maf/workflows/Daily%20MAF%20tests/badge.svg)

The full test suite of MAF can easily be run manually using sbt:
```sbtshell
maf/test
```

To allow specific tests to be run, tags have been added to the test suite. 
 * Following tags can be used to select the component of the framework that should (not) be tested: `ParserTest`, `LatticeTest`, `PrimitiveTest`, `InterpreterTest` and `SoundnessTest`.
 * Following tags can be used to select which benchmark programs (not) to run: `SlowTest`.

The `SlowTest` tag currently is only used for some of the soundness tests. When these tests are disabled, only a part of the available benchmark programs
will be used.

To run tests with a specific tag, the sbt command `maf/testOnly` should be used. The `-n` flag indicates test tags that should be
included from testing, whereas the `-l` flag indicates tags that should be excluded from testing.

For example, to run the parser tests, the following command can be used:
```sbt
maf/testOnly -- -n ParserTest
```
(Note the double -- before any possible flags.)<br>
To run all soundness tests, but only on a fast subset of benchmark programs, the command
```sbt
maf/testOnly -- -n SoundnessTest -l SlowTest
```
can be executed.

# References and Relevant publications
The original idea behind MAF comes from the following work on modular analysis: [Effect-Driven Flow Analysis](https://doi.org/10.1007/978-3-030-11245-5_12), and [A general method for rendering static analyses for diverse concurrency models modular](https://doi.org/10.1016/j.jss.2018.10.001).
The MAF framework is presented in the following publication:

* _MAF: A Framework for Modular Static Analysis of Higher-Order Languages_. SCAM
  2020. [pdf](http://soft.vub.ac.be/Publications/2020/vub-tr-soft-20-13.pdf). _See release: `SCAM 2020`_
 
MAF is a complete rework of the [Scala-AM framework](https://github.com/acieroid/scala-am), which was not focused on
modular static analysis but was primarily used to experiment with AAM-style analyses. Scala-AM is described in the
following publications:

* _Scala-AM: A Modular Static Analysis Framework_. SCAM
  2016. [pdf](http://soft.vub.ac.be/Publications/2016/vub-soft-tr-16-07.pdf)
  , [doi](https://zenodo.org/badge/latestdoi/23603/acieroid/scala-am).
* _Building a Modular Static Analysis Framework in Scala_. Scala@SPLASH
  2016. [pdf](http://soft.vub.ac.be/Publications/2016/vub-soft-tr-16-13.pdf)
        , [doi](http://doi.acm.org/10.1145/2998392.3001579).

**MAF has been used for evaluating modular static analysis approaches in the following publications:**

* _A Parallel Worklist Algorithm and Its Exploration Heuristics for Static Modular Analyses_. Journal of Systems and Software, Volume 181. 2021. 
[pdf](http://soft.vub.ac.be/Publications/2021/vub-tr-soft-21-06.pdf), [doi](https://doi.org/10.1016/j.jss.2021.111042).
* _A Parallel Worklist Algorithm for Modular Analyses_. SCAM
  2020. [pdf](http://soft.vub.ac.be/Publications/2020/vub-tr-soft-20-10.pdf). _See release: `SCAM 2020`_
* _Incremental Flow Analysis through Computational Dependency Reification_. SCAM
  2020. [pdf](http://soft.vub.ac.be/Publications/2020/vub-tr-soft-20-12.pdf). _See release: `SCAM 2020`
* _Summary-Based Compositional Analysis for Soft Contract Verification_. SCAM 2022. [preprint pdf](http://soft.vub.ac.be/Publications/2022/vub-tr-soft-22-12.pdf). [git](https://github.com/softwarelanguageslab/maf/tree/scam-2022-summaries). [artifact](https://doi.org/10.5281/zenodo.7024635)
.

**Scala-AM has been used for evaluating static analysis approaches in the following publications:**
  * _Garbage-Free Abstract Interpretation through Abstract Reference Counting_. ECOOP 2019. [pdf](http://drops.dagstuhl.de/opus/volltexte/2019/10784/).
  * _A general method for rendering static analyses for diverse concurrency models modular_. Journal of Systems and Software, Volume 149. 2019. [pdf](https://www.sciencedirect.com/science/article/pii/S0164121218302206), [doi](https://doi.org/10.1016/j.jss.2018.10.001). <!-- [pdf](https://soft.vub.ac.be/~qstieven/fwo-proposal-jss.pdf) -->
  * _Mailbox Abstractions for Static Analysis of Actor Programs_. ECOOP 2017. [pdf](http://soft.vub.ac.be/~qstieven/ecoop2017/ecoop2017actors-final.pdf), [doi](https://doi.org/10.4230/LIPIcs.ECOOP.2017.25).
  * _Employing Run-time Static Analysis to Improve Concolic Execution_. BENEVOL 2017. [pdf](http://ceur-ws.org/Vol-2047/BENEVOL_2017_paper_7.pdf).
  * _Incrementalizing Abstract Interpretation_. BENEVOL 2017. [pdf](http://ceur-ws.org/Vol-2047/BENEVOL_2017_paper_9.pdf).
  * _Static taint analysis of event-driven scheme programs_. ELS 2017. [pdf](http://soft.vub.ac.be/Publications/2017/vub-soft-tr-17-02.pdf).
  * _Improving trace-based JIT optimisation using whole-program information_. VMIL@SPLASH 2016. [pdf](http://soft.vub.ac.be/Publications/2016/vub-soft-tr-16-09.pdf), [doi](http://doi.acm.org/10.1145/2998415.2998418).
  * _STRAF: A Scala Framework for Experiments in Trace-Based JIT Compilation_. GTTSE 2015. [pdf](http://soft.vub.ac.be/Publications/2017/vub-soft-tr-17-09.pdf), [doi](https://doi.org/10.1007/978-3-319-60074-1\_10).

# Acknowledgements

![](https://www.ej-technologies.com/images/product_banners/jprofiler_medium.png)

Many thanks to [JProfiler](https://www.ej-technologies.com/products/jprofiler/overview.html) for supporting this open-source project, allowing us to easily identify and resolve performance bottlenecks in our analyses.
