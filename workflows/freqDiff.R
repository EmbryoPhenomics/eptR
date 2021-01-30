
# Quantifying the differences in frequencies between temperature groups - for population means

mean20 <- read.csv("/Users/bioImaging2/Desktop/ziad_scripts/data/means/all/20_deg/150.csv")
mean25 <- read.csv("/Users/bioImaging2/Desktop/ziad_scripts/data/means/all/25_deg/150.csv")
mean30 <- read.csv("/Users/bioImaging2/Desktop/ziad_scripts/data/means/all/30_deg/150.csv")

mean20_rescaled <- rescale(mean20, columns = 3:ncol(mean20))
mean25_rescaled <- rescale(mean25, columns = 3:ncol(mean25))
mean30_rescaled <- rescale(mean30, columns = 3:ncol(mean30))

mean_rescaled <- data.frame(group = NA)
mean_rescaled[1:367,1] <- 20
mean_rescaled[368:731,1] <- 25
mean_rescaled[732:1095,1] <- 30
mean_rescaled$group <- factor(mean_rescaled$group)
mean_rescaled[,2:ncol(mean20_rescaled)] <- NA

mean_rescaled[1:367, 2:ncol(mean20_rescaled)] <- mean20_rescaled[,2:ncol(mean20_rescaled)]
mean_rescaled[368:731, 2:ncol(mean25_rescaled)] <- mean25_rescaled[,2:ncol(mean25_rescaled)]
mean_rescaled[732:1095, 2:ncol(mean30_rescaled)] <- mean30_rescaled[,2:ncol(mean30_rescaled)]

colnames(mean_rescaled) <- colnames(mean20)
colnames(mean_rescaled)[1:2] <- c("group", "time")

mean_rescaled <- na.omit(mean_rescaled)

# Three groups produced:
  # 20 deg -> 25 deg
  # 25 deg -> 30 deg
  # 20 deg -> 30 deg

# Data normalisation may be required as lower frequencies may get over-represented

# Compute differences between each for all time-points
# Frequencies that change the most can be plotted together, possibly as groups if extraction can be conducted without loss
  # of too much data

freqDiff <- function(dataset1, dataset2) {
  
  if (nrow(dataset1) != nrow(dataset2))
    dataset1 <- dataset1[1:nrow(dataset2),]
  
  dataDiff <- data.frame(group = dataset1[,1], time = dataset1[,2])
  
  for (i in 3:ncol(dataset1)) 
    dataDiff[,i] <- as.vector(dataset1[,i] - dataset2[,i])    
  
  return(dataDiff)
  
}

diff20_25 <- freqDiff(dataset1 = mean20_rescaled, dataset2 = mean25_rescaled)
diff25_30 <- freqDiff(dataset1 = mean25_rescaled, dataset2 = mean30_rescaled)
diff20_30 <- freqDiff(dataset1 = mean20_rescaled, dataset2 = mean30_rescaled)

maxDiff20_25 <- data.frame(means = colMeans(diff20_25[,3:152]),
                           bins = 3:152)
maxDiff25_30 <- data.frame(means = colMeans(diff25_30[,3:152]),
                           bins = 3:152)
maxDiff20_30 <- data.frame(means = colMeans(diff20_30[,3:152]),
                           bins = 3:152)

library(ggplot2)

ggplot() + 
  geom_point(data = diff20_25, aes(V113, V110, color = "20 - 25")) +
  geom_point(data = diff25_30, aes(V113, V110, color = "25 - 30")) +
  geom_point(data = diff20_30, aes(V113, V110, color = "20 - 30"))

ggplot(maxDiff20_25, aes(bins, means)) + geom_col()
ggplot(maxDiff25_30, aes(bins, means)) + geom_col()
ggplot(maxDiff20_30, aes(bins, means)) + geom_col()

ggplot(mean_rescaled, aes(time, V71, color = group)) + geom_col()
ggplot(mean_rescaled, aes(time, V4, color = group)) + geom_col()
ggplot(mean_rescaled, aes(time, V25, color = group)) + geom_col()
ggplot(mean_rescaled, aes(time, V24, color = group)) + geom_col()
ggplot(mean_rescaled, aes(time, V113, color = group)) + geom_col()
ggplot(mean_rescaled, aes(time, V110, color = group)) + geom_col()
ggplot(mean_rescaled, aes(time, V3, color = group)) + geom_col()

ggplot(mean_rescaled, aes(V4, V24, color = group)) + geom_point()

animateFreqChange <- function(data, outpath) {
  
  for (i in 3:ncol(data)) {
    
    subdat <- data.frame(data[,c(1,2,i)])
    
    p <- ggplot(subdat, aes(subdat[,2], subdat[,3], color = subdat[,1])) + 
      geom_col() + 
      labs(x = "Time (hr)", y = paste0("Bin ", i - 2)) + 
      guides(color = guide_legend(title = "Group"))
    
    plot.save(p, width = 1000, height = 800, filename = paste0("bin", i, ".png"), path = outpath)
    
  }
  
}

animateFreqChange(data = mean_rescaled, outpath = "/Users/bioImaging2/Desktop/ziad_scripts/plots/freqChange/")

