getAbsValue <- function(string) {
  as.integer(gsub(" ", "", (strsplit(string, split="[(]"))[[1]][1]))
}

getRelValue <- function (string) {
  as.double(gsub(" ", "", (strsplit(string, split="[(%)]"))[[1]][2]))
}

applyToColumn <- function(column) {
  unname(unlist(Map(getRelValue, column)))
}

#library(purrr)
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
plotRidgeLinePrec(filter_precision(prec_gen_full, filter_times(perf_gen, TRUE)), "graphs/precGenFULL.png", 25)
plotRidgeLinePrec(filter_precision(prec_cur_full, filter_times(perf_cur, FALSE)), "graphs/precCurFULL.png", 25)
plotRidgeLinePrec(filter_precision(prec_gen_noopt, filter_times(perf_gen, TRUE), "More"), "graphs/precGenNOOPT.png", 25)
plotRidgeLinePrec(filter_precision(prec_cur_noopt, filter_times(perf_cur, FALSE), "More"), "graphs/precCurNOOPT.png", 25)