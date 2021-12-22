# This file imports libraries and defines some global variables.

library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)

plotWidth <- 750
plotHeight <- 450

##### Default input files #####

performance_curated_source <- "scripts/R/data/performance curated.csv"
performance_generated_source <- "scripts/R/data/performance generated.csv"

precision_curated_vs_fullReanalysis_source <- "scripts/R/data/precision curated FULL.csv"
precision_generated_vs_fullReanalysis_source <- "scripts/R/data/precision generated FULL.csv"
precision_curated_vs_noOptimisations_source <- "scripts/R/data/precision curated NOOPT.csv"
precision_generated_vs_noOptimisations_source <- "scripts/R/data/precision generated NOOPT.csv"

properties_curated_source <- "scripts/R/data/properties curated.csv"
properties_generated_source <- "scripts/R/data/properties generated.csv"

##### Default output files #####

performance_curated_graph <- "scripts/R/graphs/perfCur.png"
performance_generated_graph <- "scripts/R/graphs/perfGen.png"

precision_curated_vs_fullReanalysis_graph <- "scripts/R/graphs/precCurFULL.png"
precision_generated_vs_fullReanalysis_graph <- "scripts/R/graphs/precGenFULL.png"
precision_curated_vs_noOptimisations_graph <- "scripts/R/graphs/precGenNOOPT.png"
precision_generated_vs_noOptimisations_graph <- "scripts/R/graphs/precCurNOOPT.png"

##### Saved data set locations #####

# Data that is transferred from performance benchmarks to precision benchmarks (for filtereing).
filtered_times_curated <- "scripts/R/data/filtered_times_curated.Rds"
filtered_times_generated <- "scripts/R/data/filtered_times_generated.Rds"