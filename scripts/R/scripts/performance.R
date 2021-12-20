min_init_time <- 100 # The initial analysis must run at least 100 seconds.
mutations_per_file <- 5

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
    times_slow <- (times_slow %>% slice(-drop))
  }
  return(times_slow)
}

# Ridgelineplot

plotRidgeLine <- function(perf_data, mutations, out, bins = 25, max = 5) {
  filtered_data <- filter_times(perf_data, mutations)
  div <- filtered_data[, 2:length(filtered_data)] / filtered_data[, 3]
  png(out, width = plotWidth, height = plotHeight)
  plot <- div %>% gather(key="Configuration", value="Comparison") %>%
    ggplot(aes(x = Comparison, y = Configuration, fill = Configuration)) +
    geom_density_ridges(alpha=0.6, stat="binline", bins=bins) +
    theme_ridges() +
    theme(legend.position = "none") +
    xlim(-0.15, max)
  print(plot)
  dev.off()
}
plotRidgeLine(perf_gen, TRUE, "graphs/perfGen.png")
plotRidgeLine(perf_cur, FALSE, "graphs/perfCur.png", 50, 2)