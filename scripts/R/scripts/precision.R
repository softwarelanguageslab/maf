source("scripts/R/scripts/incremental.R")

args <- commandArgs(TRUE)
if (length(args) == 0) {
  prec_cur_full <- read.csv(precision_curated_vs_fullReanalysis_source)
  prec_gen_full <- read.csv(precision_generated_vs_fullReanalysis_source)
  prec_cur_noopt <- read.csv(precision_curated_vs_noOptimisations_source)
  prec_gen_noopt <- read.csv(precision_generated_vs_noOptimisations_source)
} else if (length(args) == 4) {
  prec_cur_full <- read.csv(args[1])
  prec_gen_full <- read.csv(args[2])
  prec_cur_noopt <- read.csv(args[3])
  prec_gen_noopt <- read.csv(args[4])
} else {
  library(stringr)
  print(args)
  stop(str_interp("Wrong number of arguments provided (${length(args)})."))
}

# Extract data from file
getAbsValue <- function(string) {
  as.integer(gsub(" ", "", (strsplit(string, split="[(]"))[[1]][1]))
}

getRelValue <- function (string) {
  as.double(gsub(" ", "", (strsplit(string, split="[(%)]"))[[1]][2]))
}

applyToColumn <- function(column) {
  unname(unlist(Map(getRelValue, column)))
}

# Filtering
filter_precision <- function(prec_data, filtered_times, keyword = "Less") {
  # First, only keep the benchmarks for which we have retained timing information.
  drop <- c()
  for (row in seq_len(nrow(prec_data))) {
    find <- filtered_times %>% filter(startsWith(benchmark, prec_data[row, 1]))
    if (nrow(find) != 1) {
      drop <- c(row, drop)
    }
  }
  prec_data <- (prec_data %>% slice(-drop))
  # Second, keep the percentage of values that are less precise.
  prec_data <- prec_data %>% select(starts_with("benchmark") | starts_with(keyword))
  # Third, get the percentages.
  prec_data <- prec_data %>% mutate(across(starts_with(keyword), applyToColumn))
  return(prec_data)
}

# Load timing data to know what to measurements to keep.
performance_filtered_cur <- readRDS(filtered_times_curated)
performance_filtered_gen <- readRDS(filtered_times_generated)

#Ridgelineplot
plotRidgeLinePrec <- function(prec_data, out, bins = 15) {
  div <- prec_data[, 2:length(prec_data)]
  png(out, width = plotWidth, height = plotHeight)
  plot <- div %>% gather(key="Configuration", value="Comparison") %>%
    ggplot(aes(x = Comparison, y = Configuration, fill = Configuration)) +
    geom_density_ridges(alpha=0.6, stat="binline", bins=bins) +
    theme_ridges() +
    theme(legend.position = "none") +
    xlim(-10, 100)
  print(plot)
  while (!is.null(dev.list()))  dev.off()
}

plotRidgeLinePrec(filter_precision(prec_cur_full, performance_filtered_cur), precision_generated_vs_fullReanalysis_graph, 25)
plotRidgeLinePrec(filter_precision(prec_gen_full, performance_filtered_gen), precision_curated_vs_fullReanalysis_graph, 25)

plotRidgeLinePrec(filter_precision(prec_cur_noopt, performance_filtered_cur, "More"), precision_generated_vs_noOptimisations_graph, 25)
plotRidgeLinePrec(filter_precision(prec_gen_noopt, performance_filtered_gen, "More"), precision_curated_vs_noOptimisations_graph, 25)
