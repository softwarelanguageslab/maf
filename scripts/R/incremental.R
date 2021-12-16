perf_cur = read.csv("scripts/R/data/performance curated.txt")
perf_gen = read.csv("scripts/R/data/performance generated.txt")

prec_cur = read.csv("scripts/R/data/precision curated.txt")
prec_gen = read.csv("scripts/R/data/precision generated.txt")

prop_cur = read.csv("scripts/R/data/properties curated.txt")
prop_gen = read.csv("scripts/R/data/properties generated.txt")

library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)

##### Performance #####

min_init_time = 100 # The initial analysis must run at least 100 seconds.
mutations_per_file = 5

filter_times = function(perf_data, mutations) {
  # First, omit entries that are not complete.
  data_complete = na.omit(perf_data)
  # Second, remove the standard deviations.
  times = data_complete %>% select(starts_with("benchmark") | starts_with("ms"))
  # Third, remove benchmarks for which the initial time was under the min_init_time.
  times_slow = times[(times$ms..init.>=min_init_time),]
  # Lastly, only keep entries for which all 5 variations are present if these are the generated benchmarks.
  if (mutations) {
    drop = c()
    original_sources = substr(times_slow[,1], 1, nchar(times_slow[,1])-6)
    for (row in 1:nrow(times_slow)) {
      cut_name = substr(times_slow[row, 1], 1, nchar(times_slow[row, 1])-6)
      filtered = times_slow %>% filter(startsWith(benchmark, cut_name))
      if (nrow(filtered) != mutations_per_file) {
        drop <- c(row, drop)
      }
    }
    times_slow <- (times_slow %>% slice(-drop))
  }
  return(times_slow)
}

# Ridgelineplot

plotRidgeLine = function(perf_data) {
  filtered_data = filter_times(perf_data, TRUE)
  div = filtered_data[,2:length(filtered_data)] / filtered_data[,3]
  div %>% gather(key="Configuration", value="Comparison") %>%
    ggplot(aes(x = Comparison, y = Configuration, fill = Configuration)) +
    geom_density_ridges(alpha=0.6, stat="binline", bins=100) +
    theme_ridges() +
    theme(legend.position = "none") +
    xlim(0, 5)
}
plotRidgeLine(perf_gen)

##### Precision #####

getAbsValue = function(string) {
  as.integer(gsub(" ", "", (strsplit(string, split="[(]"))[[1]][1]))
}

getRelValue = function (string) {
  as.double(gsub(" ", "", (strsplit(string, split="[(%)]"))[[1]][2]))
}

applyToColumn <- function(column) {
  unname(unlist(Map(getRelValue, column)))
}

#library(purrr)
filter_precision = function(prec_data, filtered_times) {
  # First, only keep the benchmarks for which we have retained timing information.
  drop = c()
  for (row in 1:nrow(prec_data)) {
    find = filtered_times %>% filter(startsWith(benchmark, prec_data[row, 1]))
    if (nrow(find) != 1) {
      drop <- c(row, drop)
    }
  }
  prec_data <- (prec_data %>% slice(-drop))
  # Second, keep the percentage of values that are less precise.
  prec_data <- prec_data %>% select(starts_with("benchmark") | starts_with("less"))
  # Third, get the percentages.
  prec_data <- prec_data %>% mutate(across(starts_with("Less"), applyToColumn))
  return(prec_data)
}

plotRidgeLinePrec = function(prec_data) {
  div = prec_data[,2:length(prec_data)]
  div %>% gather(key="Configuration", value="Comparison") %>%
    ggplot(aes(x = Comparison, y = Configuration, fill = Configuration)) +
    geom_density_ridges(alpha=0.6, stat="binline", bins=15) +
    theme_ridges() +
    theme(legend.position = "none") +
    xlim(0, 100)
}
plotRidgeLinePrec(filter_precision(prec_gen, filter_times(perf_gen, TRUE)))

# table = read.csv("scripts/R/data/performance generated.txt")
# table = read.csv("scripts/R/data/performance curated.txt")
# table_complete = na.omit(table) # Remove rows with missing values (there should be none but there are...).
# library(dplyr)
# times = table_complete %>% select(starts_with("ms")) # Remove the standard deviations.
# namedTimes = table_complete %>% select(starts_with("benchmark") | starts_with("ms"))
# DSs = table_complete[,c(TRUE, FALSE)] # Remove the times.
#
# # Create violin plots of the runtimes per configuration.
# library(tidyr)
# library(ggplot2)
# times_pruned = times[(times$ms..init.>100),]
# times_pruned %>%
#   gather(key="Configuration", value="ms") %>%
#   ggplot(aes(x=Configuration, y = ms, fill=Configuration)) +
#   geom_violin() +
#   geom_boxplot(width=0.1, color="grey", alpha=0.2) +
#   xlab("Configuration")
#
# # Differences with full reanalysis.
# library("RColorBrewer")
# noZero = namedTimes[(namedTimes$ms..rean.!=0),1:length(namedTimes)] # We don't want to divide by zero.
# diffs = noZero[,2:length(noZero)] / noZero[,3]
# baseNames = substr(noZero[,1], 1, nchar(noZero[,1])-6)
# data = cbind(baseNames, diffs)
# res = aggregate(data[,2:length(data)],FUN=mean, by=list(data$baseNames))
# #dev.new(width=1500, height=500, unit="px")
# xs = c()
# ys = c()
# for(line in 1:nrow(res)){
#   xs = append(xs, rep(line,each=length(4:length(res))))
#   ys = append(ys, res[line,4:length(res)])
# }
# #display.brewer.all()
# #pal = brewer.pal(n = length(2:length(res)), name = "Dark2")
# plot(1:nrow(res), res[,11], ylim=c(0,2.25),labels=FALSE,pch=4,xlab="", ylab="")#, col = rep(colors(),length(4:length(res))))
# plot(xs, ys, ylim=c(0,2.25),labels=FALSE,pch=4,xlab="", ylab="", col = rep(colors(),length(4:length(res))))
# axis(1, at=1:nrow(res), labels=res[,1],las=2)
# axis(2, at=seq(0,100,0.5), las=1)
# abline(h=1, col="blue")
# abline(h=0)
#
# # Heatmap.
# baseData = namedTimes[namedTimes$ms..rean.>0,]
# heatmapData = baseData[,2:length(baseData)]/baseData[,3]
# names = baseData[,1]
# originalProgramNames = substr(names, 1, nchar(names)-6)
# withShortenedNames = cbind(originalProgramNames, heatmapData)
# perOriginalProgram = aggregate(withShortenedNames[,2:length(withShortenedNames)], FUN=median,by=list(originalProgramNames))
# numbers = as.matrix(perOriginalProgram[,2:length(perOriginalProgram)])
# rownames(numbers) = perOriginalProgram[,1]
# heatmap(numbers, Rowv = NA, Colv = NA)
