library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)

plotWidth <- 750
plotHeight <- 450

##### Data files #####

performance_curated_source <- "data/performance curated.txt"
performance_generated_source <- "data/performance generated.txt"

precision_curated_vs_fullReanalysis_source <- "data/precision curated FULL.txt"
precision_generated_vs_fullReanalysis_source <- "data/precision generated FULL.txt"
precision_curated_vs_noOptimisations_source <- "data/precision curated NOOPT.txt"
precision_generated_vs_noOptimisations_source <- "data/precision generated NOOPT.txt"

properties_curated_source <- "data/properties curated.txt"
properties_generated_source <- "data/properties generated.txt"

##### Performance #####

perf_cur <- read.csv(performance_curated_source)
perf_gen <- read.csv(performance_generated_source)

source("scripts/R/scripts/performance.R")

##### Precision #####

prec_cur_full <- read.csv(precision_curated_vs_fullReanalysis_source)
prec_gen_full <- read.csv(precision_generated_vs_fullReanalysis_source)
prec_cur_noopt <- read.csv(precision_curated_vs_noOptimisations_source)
prec_gen_noopt <- read.csv(precision_generated_vs_noOptimisations_source)

source("scripts/R/scripts/precision.R")

##### Properties #####

prop_cur <- read.csv(properties_curated_source)
prop_gen <- read.csv(properties_generated_source)