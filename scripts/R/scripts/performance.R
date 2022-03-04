source("scripts/R/scripts/incremental.R")

args <- commandArgs(TRUE)
if (length(args) == 0) {
  perf_cur_in <- read.csv(performance_curated_source)
  perf_gen_in <- read.csv(performance_generated_source)
  perf_cur_out <- performance_curated_graph
  perf_gen_out <- performance_generated_graph
} else if (length(args) == 2) {
  perf_cur_in <- read.csv(args[1])
  perf_gen_in <- read.csv(args[2])
  perf_cur_out <- performance_curated_graph
  perf_gen_out <- performance_generated_graph
} else if (length(args) == 4 && substr(args[3], nchar(args[3]) - 4, nchar(args[3])) == ".png" && substr(args[3], nchar(args[3]) - 4, nchar(args[3])) == ".png") {
  perf_cur_in <- read.csv(args[1])
  perf_gen_in <- read.csv(args[2])
  perf_cur_out <- args[3]
  perf_gen_out <- args[4]
} else {
  library(stringr)
  print(args)
  stop(str_interp("Wrong number of arguments provided (${length(args)})."))
}

min_init_time <- 100 # The initial analysis must run at least 100 seconds.
mutations_per_file <- 5

# Rename and reorder the columns
alter_columns <- function(data_in) {
  renamed <- data_in %>% rename("Initial analysis" = "ms..init.",
                                "Full reanalysis" = "ms..rean.",
                                "No optimisations" = "ms..NoOpt.",
                                "CI" = "ms..CI.",
                                "DI" = "ms..DI.",
                                "WI" = "ms..WI.",
                                "CI + DI" = "ms..CI.DI.",
                                "CI + WI" = "ms..CI.WI.",
                                "DI + WI" = "ms..DI.WI.",
                                "CI + DI + WI" = "ms..CI.DI.WI.")
  #order <- c("benchmark", "Initial analysis", "Full reanalysis", "No optimisations", "CI", "DI", "WI", "CI + DI", "CI + WI", "DI + WI", "CI + DI + WI")
  #return(renamed[, order])
  return(renamed)
}

# Filter data
filter_times <- function(perf_data, mutations) {
  # First, omit entries that are not complete.
  data_complete <- na.omit(perf_data)
  # Second, remove the standard deviations.
  times <- data_complete %>% select(starts_with("benchmark") | starts_with("ms"))
  # Third, remove benchmarks for which the initial time was under the min_init_time.
  times_slow <- times[(times$ms..init.>=min_init_time),]
  # Lastly, only keep entries for which all 5 variations are present if these are the generated benchmarks.
  if (mutations) {
    drop <- c()
    for (row in seq_len(nrow(times_slow))) {
      cut_name <- substr(times_slow[row, 1], 1, nchar(times_slow[row, 1])-6)
      filtered <- times_slow %>% filter(startsWith(benchmark, cut_name))
      if (nrow(filtered) != mutations_per_file) {
        drop <- c(row, drop)
      }
    }
    if (!is.null(drop)) {
       times_slow <- (times_slow %>% slice(-drop))
    }
  }
  return(times_slow)
}

# Filter data and rename the columns
perf_cur_filtered <- alter_columns(filter_times(perf_cur_in, FALSE))
perf_gen_filtered <- alter_columns(filter_times(perf_gen_in, TRUE))

# Save filtered datasets for reuse by precision/properties scripts.
saveRDS(perf_cur_filtered, filtered_times_curated)
saveRDS(perf_gen_filtered, filtered_times_generated)

# Normalises data w.r.t. a given column. Preserves first column with benchmark names.
normaliseData <- function(data, columnName) {
  colNr <- which(colnames(data) == columnName)
  norm <- data[, 2:length(data)] / data[,colNr]
  return(cbind(data[,1], norm) %>% rename("benchmark" = "data[, 1]")) # Keep original column names.
}

perf_cur_normalised_Rean <- normaliseData(perf_cur_filtered, "Full reanalysis")
perf_gen_normalised_Rean <- normaliseData(perf_gen_filtered, "Full reanalysis")

# Get an overview of the data per benchmark and per configuration.
dataOverview <- function(data) {
 return(pivot_longer(data, -c("benchmark")))
}

# Ridgelineplot

plotRidgeLine <- function(filtered_data, out, bins = 25, max = 5) {
  data <- filtered_data[, 2:length(filtered_data)] # Remove benchmark names.
  png(out, width = plotWidth, height = plotHeight)
  plot <- data %>% gather(key="Configuration", value="Comparison") %>%
    ggplot(aes(x = Comparison, y = Configuration, fill = Configuration)) +
    geom_density_ridges(alpha=0.6, stat="binline", bins=bins) +
    theme_ridges() +
    theme(legend.position = "none") +
    xlim(-0.15, max(data) + 0.05*max(data)) # max)
  print(plot)
  dev.off()
}

plotRidgeLine(perf_cur_normalised_Rean, perf_cur_out, 50, 2)
plotRidgeLine(perf_gen_normalised_Rean, perf_gen_out)

# Violin plot

plotViolin <- function(filtered_data, out) {
  data <- filtered_data[, 2:length(filtered_data)]
  png(out, width = plotWidth, height = plotHeight)
  plot <- data %>% gather(key="Configuration", value="ms") %>%
    ggplot(aes(x="Configuration", y="ms", fill="Configuration")) +
    geom_violin() +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    xlab("Configuration")
  print(plot)
  dev.off()
}

#plotViolin(perf_cur_normalised_Rean, perf_cur_out)