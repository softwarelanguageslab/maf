Summary-Based Compositional Analysis for Soft Contract Verification - Artifact
===============================================================================

This repository contains the software artifact for the paper "Summary-Based Compositional Analysis for Soft Contract Verification".

## Running the Artifact

The artifact can be executed in two ways: by building it from source or by executing our pre-built Docker image.

### Building from Source

To build the artifact from source, an included `Dockerfile` can be used. This `Dockerfile` contains all the commands necessary to set-up a suitable building environment.
It can be executed as follows (the dollar sign must be ignored):

```
$ ./scripts/artifact/build_from_source.sh 
```

### Running the existing Docker image

In order to run the pre-built Docker image, it first need to be imported in the Docker environment:

```
$ ./scripts/artifact/import_image.sh
```

Followed by:

```
$ ./scripts/artifact/run_benchmarks.sh results/
```

The second argument of the command above can be used to change the location of the CSV files that contain the benchmark results.

## Reproducing Tables

To reproduce the tables included in the paper, the benchmark programs can be executed, and their results analyzed. 
For convience a script is included that generates the tables for the paper, and outputs them in LaTeX format:

```
$ ./scripts/artifact/run_and_build_tables.sh results/
```

The command above will run the benchmarks and their Python processing scripts. All processing scripts can be found in the `scripts/Python/scv/` directory.

## Re-using and extending the framework

## Badges

Our artifact satisfies the following criteria:

* Available: the artifact is available both in binary and source form. The binary form is available as a Docker image uploaded to TODO, while the source code is available as a Github repository at https://github.com/softwarelanguageslab/maf/tree/scam-2022-summaries.
* Functional: 
