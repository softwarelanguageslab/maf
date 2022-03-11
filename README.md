Modular Analysis Framework (MAF): A Framework for Modular Analysis of Dynamic Languages

# Goal

The goal of this artefact is to experiment with abstract machines and language semantics. 
Currently, the artefact's implementation is focused towards experiments with modular analyses. 
These are build according to the ModX technique.
A comparison on how our ModF analysis compares to a well-known AAM-style analysis is described [here](docs/AAM.md).

Additionally, semantics for R5RS Scheme are present. 
Note that the semantics of our implementation may deviate from the R5RS specification at certain points; these have been described [here](docs/INCOMPATIBILITIES.md).

For more information on the incremental analyses that are currently developed using MAF,
see [here](code/shared/src/main/scala/maf/modular/incremental/incrementalAnalysis.md).

# Usage

The MAF framework can be used in several ways.

## Using the JavaScript visual front-end

The framework includes a JavaScript front-end that can be used to visualise a modular analysis in the browser. Before
the visualisation can be run, you have to ensure the code is compiled, by executing the commans `fastOptJS` (
or `fullOptJS`) in your sbt repl.

The MAF framework currently provides a standard visualisation for modular analyses, as well as specific visualisations
that correspond to several different flavours of the analysis (e.g., adaptive or incremental analyses.)

To run the standard visualisation, open the file `standard.html` in the `web` folder with your browser. The
visualisation provides information with regard to the work list (coloured in light blue) and currently analysed
component (coloured in dark blue). Stepping through the visualisation can be done using the space bar. The other
visualisations are also accessible via the `web` folder.

## Analysing a program using command line

The MAF framework is built in a modular style. To run a modular analysis, you need to compose the implementation of a
specific machine and an abstract domain.

To analyze a specific program, an instance of the ModF analysis class must be created. The constructor of this class
takes a parsed version of the program to be analysed, which can be obtained as follows:
```scala
val text = io.Reader.loadFile(path-to-file)
val prog = language.scheme.Schemeparser.parse(text)
```
Additional preprocessing steps are performed by the modular analysis itself and hence must not be performed manually.

Now, the ModF instance can be created. For example, to analyze `prog` using a big-step ModF analysis
with full argument sensitivity and a type domain:
```scala
val analysis = new ModAnalysis(prog) with BigStepSemantics
                                     with StandardSchemeModFSemantics
                                     with FullArgumentSensitivity
                                     with TypePropagationDomain
analysis.analyze()
```
Method `analyze` computes the full (and sound) analysis result for the program that `ModAnalysis` is constructed with. 
This method does not return the analysis results directly.
Rather, after calling `analyze`, the computed analysis results (e.g., the final store and dependencies) can be accessed through the properties of the `analysis` object (e.g., through `analysis.store` and `analysis.deps`).

Alternatively, one can use the `analyzeWithTimeout(<timeout>)` method to run the analysis with a given timeout. This timeout is obtained from a Java Duration
(using `Timeout.start(<duration>)`). 
The method returns when either the analysis has terminated or when the timeout has been reached (approximately, meaning in practice it may run a bit longer than the specified timeout). 
Extra care should be taken when using this method, as the (partial) analysis results are not guaranteed to be sound when the timeout is triggered. 
Therefore, when using this method, it is recommended to explicitly check afterwards if the analysis terminated using the `finished` method.

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
 * Following tags can be used to select the component of the framework that should (not) be tested: `ParserTest`, `LatticeTest`, `PrimitiveTest` and `SoundnessTest`.
 * Following tags can be used to select which benchmark programs (not) to run: `SlowTest`.
 * Following tags can be used to test utility components of the framework: `UtilTest`.

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
modular static analysis but was primarily used to experiment with AAM-style analyses. An [AAM implementation](docs/AAM.md) (inspired by Scala-AM) is provided in MAF for benchmarks comparisons. Scala-AM is described in the
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
  2020. [pdf](http://soft.vub.ac.be/Publications/2020/vub-tr-soft-20-12.pdf). _See release: `SCAM 2020`_

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
