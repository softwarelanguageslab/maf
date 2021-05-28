A Parallel Worklist Algorithm and Its Exploration Heuristics for Static Modular Analyses (Replication Package)

# Summary
This replication package enables reproducing the evaluation of the following paper:
> A Parallel Worklist Algorithm and Its Exploration Heuristics for Static Modular Analyses

# Setup
This replication package is self-contained and only requires a local installation of =sbt= to run.
All dependencies will be downloaded upon the first use of the artifact.

For reference, we have performed our evaluation on a server using an AMD Ryzen Threadripper 3990X processor with 64 cores at 2.9GHz, enabling running 128 threads simultaneously in total, with Java 14.0.2 (OpenJDK) and Scala 2.13.3. The JVM is configured with a fixed heap size of 64GB.
In the likely case you are running this on a less powerful machines, the results with higher levels of parallelism may not hold.

# Benchmark names
In the paper, the benchmarks names have been slightly changed compared to the file names used in this reproduction package.
Here is the mapping used:

| File name                                                  | Benchmark name    |
| ---------------------------------------------------------- | ----------------- |
| test/R5RS/WeiChenRompf2019/meta-circ.scm                   | meta-circ         |
| test/R5RS/WeiChenRompf2019/earley.sch                      | earley            |
| test/R5RS/WeiChenRompf2019/toplas98/graphs.scm             | graphs            |
| test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm            | dynamic           |
| test/R5RS/WeiChenRompf2019/toplas98/nbody-processed.scm    | nbody             |
| test/R5RS/WeiChenRompf2019/toplas98/boyer.scm              | boyer             |
| test/R5RS/gambit/peval.scm                                 | peval             |
| test/R5RS/gambit/scheme.scm                                | scheme            |
| test/R5RS/gambit/sboyer.scm                                | sboyer            |
| test/R5RS/gambit/nboyer.scm                                | nboyer            |
| test/R5RS/gambit/matrix.scm                                | matrix            |
| test/R5RS/gambit/browse.scm                                | browse            |
| test/R5RS/scp1-compressed/all.scm                          | scp               |
| test/R5RS/ad/all.scm                                       | ad                |
| test/R5RS/various/SICP-compiler.scm                        | SICP-compiler     |
| test/R5RS/icp/icp_1c_ambeval.scm                           | ambeval           |
| test/R5RS/icp/icp_1c_multiple-dwelling.scm                 | multiple-dwelling |
| test/R5RS/icp/icp_1c_ontleed.scm                           | decompose         |
| test/R5RS/icp/icp_1c_prime-sum-pair.scm                    | prime-sum-pair    |
| test/R5RS/icp/icp_7_eceval.scm                             | eceval            |
| test/R5RS/icp/icp_8_compiler.scm                           | compiler          |
| test/R5RS/icp/icp_5_regsim.scm                             | regsim            |
| test/R5RS/icp/icp_3_leval.scm                              | leval             |
| test/R5RS/icp/icp_2_aeval.scm                              | aeval             |

# Generating Tables 1 and 2
The experiments to produce Tables 1 and 2 are generated when running the script for RQ1 (see below).
If you want simply to reproduce these tables without running the full script, the following command can be used:

```
sbt 'maf/runMain maf.cli.experiments.parallel.BaseResultsModF'
```

A LaTex table is produced as the final output in the console.
Moreover, the resulting analysis times are produced in:
  - `data/modf-base-context-insensitive.csv` for the 0-CFA analysis
  - `data/modf-base-context-sensitive-1CFA.csv` for the 1-CFA analysis
  - `data/modf-base-context-sensitive-2CFA.csv` for the 2-CFA analysis
Each of these file is accompanied by a `....csv-stddev` file, also in CSV format, that lists the standard deviation measured across all execution of the benchmarks.

*Expected running time*: around 16 hours.
This time can be reduced by decreasing the number of runs used to perform the evaluation, by changing variable `analysisRuns` in `code/jvm/src/main/scala/maf/cli/experiments/parallel/Performance.scala` (in trait `BaseResultsModFSetup`)

# Generating Figures 3, 4, and 5 (RQ1)
The results for RQ1 can be produced by running the shell script `rq1.sh`.
This script outputs multiple `.csv` files in the `data/` directory:
  - `data/modf-context-insensitive.csv` for the 0-CFA analysis
  - `data/modf-context-sensitive-1CFA.csv` for the 1-CFA analysis
  - `data/modf-context-sensitive-2CFA.csv` for the 2-CFA analysis

Each of these `.csv` files need to be adapted before being able to generate the figures, by running the following commands:
  - `./reorder.sh data/modf-context-insensitive.csv` and `./reorder.sh data/modf-context-insensitive.csv-stddev` for Fig. 3
  - `./reorder.sh data/modf-context-sensitive-1CFA.csv` and `./reorder.sh data/modf-context-sensitive-1CFA.csv-stddev` for Fig. 3
  - `./reorder.sh data/modf-context-sensitive-2CFA.csv` and `./reorder.sh data/modf-context-sensitive-2CFA.csv-stddev` for Fig. 3

Each figure can then be produced by running:
  - `python modf-context-insensitive-plots.py` for Fig. 3
  - `python modf-context-sensitive-1CFA-plots.py` for Fig. 4
  - `python modf-context-sensitive-2CFA-plots.py` for Fig. 5

*Expected running time* around 40 hours.

# Generating Figure 6 (RQ2)
To reproduce Figure 6, first run the script `rq2.sh`.
Once the script has produced a table output, you can kill it (^C), as it can sometimes keep running even though all experiments are finished.
It outputs its results in `data/modconc.csv` file.
Remove any line containing timeouts (`TIMEOUT`) in this file, and then Figure 6 can be produced by running `python modconc_plot.py`

*Expected running time*: around 24 hours.

# Generating Tables 3, 4, and 5 (RQ3)
To reproduce these tables, first run the script `rq3.sh`.

The output `.csv` are:
  - `data/modf-context-insensitive-metrics.csv` for 0-CFA
  - `data/modf-context-sensitive-metrics-1CFA.csv` for 0-CFA
  - `data/modf-context-sensitive-metrics-2CFA.csv` for 0-CFA

In order to produce the tables, we have included the file `metrics-computation.xls`.
The "Raw results" tab can be filled with the data from one of these `.csv` files.
Then, the "Formatted results" tab will contain a Table with the same layout as Tables 3, 4, and 5 in the paper.


*Expected running time*: around 29 hours.
