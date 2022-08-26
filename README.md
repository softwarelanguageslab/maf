Summary-Based Compositional Analysis for Soft Contract Verification - Artifact
===============================================================================

This repository contains the software artifact for the paper "Summary-Based Compositional Analysis for Soft Contract Verification".

## Obtaining the Artifact

The artifact is published as an archive on Zenodo at https://doi.org/10.5281/zenodo.7024635.

## Running the Artifact

The artifact can be executed in two ways: by building it from source or by executing our pre-built Docker image.
The commands listed below will create a Docker image in the local Docker installation named `scam-scv-2022-artifact`. All commands need to be executed on the host where Docker has been installed.

### Building from Source

To build the artifact from source, an included `Dockerfile` can be used. This `Dockerfile` contains all the commands necessary to set-up a suitable building environment.
It can be executed using the following command: 

```
./scripts/artifact/build_from_source.sh 
```

### Running the existing Docker image

In order to run the pre-built Docker image, it first needs to be imported in the Docker environment:

```
./scripts/artifact/import_image.sh
```

## Reproducing Tables

**Note:** All commands need to be executed on the host where Docker has been installed.

To reproduce the tables included in the paper, the benchmark programs can be executed, and their results analyzed. 

First make sure that the `results` directory is empty or non-existent. If you run the artifact for the first time, this should be the case.
Then run the benchmarks, this may take up to 2 hours.

``` 
./scripts/artifact/run_benchmarks.sh $PWD/results/
```

The second argument of the command above can be used to change the location of the CSV files that contain the benchmark results.

Then run the script for producing the tables and printing them in LaTeX format: 

```
./scripts/artifact/produce_tables.sh $PWD/results/
```

The command above will run the benchmarks and their Python processing scripts. All processing scripts can be found in the `scripts/Python/scv/` directory.

## Code Navigation

* Section 3A: syntax of the contract language is provided by the following files and packages:
   - `code/shared/src/main/scala/maf/language/scheme/SchemeExp.scala`: defines the AST elements as subklasses of `ContractSchemeExp`
   - `code/shared/src/main/scala/maf/language/ContractScheme/`: provides various preprocessing code for parsing the code ofprograms written in the contract language.
* Section 3B: concrete semantics, in big-step form can be found in the following files and packages:
   - `code/shared/src/main/scala/maf/language/ContractScheme/ContractValues.scala` contains definitions for all contract related values used by the interpreter 
   - `code/shared/src/main/scala/maf/language/ContractScheme/interpreter/` provides a concrete interpreter for soundness testing.
* Section 4: 
   - `code/shared/src/main/scala/maf/language/scheme/lattices/ModularSchemeLattice.scala` provides the abstract domain for the static analysis.
   - `code/shared/src/main/scala/maf/modular/scv/ScvBigStepSemantics.scala` describes the abstract semantics of the contract language in a big step style. It descends from a ModF analysis from the MAF framework, which implies the usage of a global store, and provides the notion of "components" and effects between them. 
   - `code/shared/src/main/scala/maf/modular/scv/FunctionSummary.scala` provides the implementation of function summaries as well as the two phase-analysis of collection and propagation.
* Section 6B: 
   - `maf.cli.experiments.aam.ScvPerformanceComparison` implements the performance (in terms of execution time) benchmarks described in the paper. 
   - `maf.cli.experiments.scv.CountFalsePositives` implements the precision (in terms of false positives) benchmarks described in the papers.
