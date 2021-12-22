# table = read.csv("scripts/R/data/performance generated.csv")
# table = read.csv("scripts/R/data/performance curated.csv")
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