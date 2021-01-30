# Visualisations for various bin sizes

library(plotly)
library(ggplot2)
source("~/Desktop/github/misc_embryoPhenomics/R/data_vis.R")
source("~/Desktop/github/misc_embryoPhenomics/R/data_analysis.R")
source('~/Desktop/github/misc_embryoPhenomics/R/data_wrangling.R')

# 3D plotly graph from 3 bins, without further transformations

meanAll20 <- read.csv("/run/media/z/Z/means/all/20_deg/3.csv", header = TRUE)
meanAll25 <- read.csv("/run/media/z/Z/means/all/25_deg/3.csv", header = TRUE)
meanAll30 <- read.csv("/run/media/z/Z/means/all/30_deg/3.csv", header = TRUE)

meanAll <- data.frame(group = rep(c(20,25,30), each = 367))
meanAll$group <- factor(meanAll$group)

meanAll[1:367, 2:ncol(meanAll20)] <- meanAll20[,2:ncol(meanAll20)]
meanAll[368:731, 2:ncol(meanAll25)] <- meanAll25[,2:ncol(meanAll25)]
meanAll[732:1095, 2:ncol(meanAll30)] <- meanAll30[,2:ncol(meanAll30)]
meanAll <- na.omit(meanAll)

meanAll <- meanAll[meanAll$bin3 < 0.0015,]

plot_ly(data = meanAll, x = ~bin1, y = ~bin2, z = ~bin3, color = ~group) %>%
  add_markers(size = 2)

p1 <- ggplot(meanAll, aes(bin1, bin2, color = group)) + 
  geom_point() +
  labs(title = "Mean power values for various temperature groups",
       subtitle = "Bin number = 3")

plot.save(p1, 
          path = "~/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/",
          filename = "2D_bins-3_raw.png",
          width = 750, 
          height = 550)


# 3D plotly graph but with rescaled frequency/power values

meanAll25 <- meanAll25[meanAll25$bin3 < 0.0015,] # Remove outlier

meanAll20_rescaled <- rescale(meanAll20, columns = 3:ncol(meanAll20))
meanAll25_rescaled <- rescale(meanAll25, columns = 3:ncol(meanAll25))
meanAll30_rescaled <- rescale(meanAll30, columns = 3:ncol(meanAll30))

meanAll_rescaled <- data.frame(group = NA)
meanAll_rescaled[1:367,1] <- 20
meanAll_rescaled[368:730,1] <- 25
meanAll_rescaled[731:1094,1] <- 30
meanAll_rescaled$group <- factor(meanAll_rescaled$group)

meanAll_rescaled[1:367, 2:ncol(meanAll20_rescaled)] <- meanAll20_rescaled[,2:ncol(meanAll20_rescaled)]
meanAll_rescaled[368:730, 2:ncol(meanAll25_rescaled)] <- meanAll25_rescaled[,2:ncol(meanAll25_rescaled)]
meanAll_rescaled[731:1094, 2:ncol(meanAll30_rescaled)] <- meanAll30_rescaled[,2:ncol(meanAll30_rescaled)]

plot_ly(data = meanAll_rescaled, x = ~bin1, y = ~bin2, z = ~bin3, color = ~group) %>%
  add_markers(size = 2)

p2 <- ggplot(meanAll, aes(bin1, bin2, color = group)) + 
  geom_point() +
  labs(title = "Mean rescaled power values for various temperature groups",
       subtitle = "Bin number = 3")

plot.save(p2, 
          path = "~/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/",
          filename = "2D_bins-3_rescaled.png",
          width = 750, 
          height = 550)


# PCA analysis

# For bin number = 3

pca <- prcomp(meanAll_rescaled[,3:ncol(meanAll_rescaled)])

allPC <- data.frame(group = meanAll_rescaled$group, pca$x[,1:3])
allPC$group <- factor(allPC$group)
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100)

pc1 <- ggplot(allPC, aes(PC1, PC2, colour = group)) +
  geom_point() + 
  labs(title = "Mean rescaled power values for various temperature groups",
       subtitle = "PCA transform, bin number = 3",
       x = paste("PC1", paste0(expl.var[1], "%")), 
       y = paste("PC2", paste0(expl.var[2], "%"))) + 
  guides(colour = guide_legend(title = "Group:"))

plot.save(pc1, 
          path = "~/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/",
          filename = "PCA_bins-3.png",
          width = 750, 
          height = 550)

# For remaining bin numbers

# PCA results

pcaAll <- doPCAforBins(path_20 = "/run/media/z/Z/means/all/20_deg/",
             path_25 = "/run/media/z/Z/means/all/25_deg/",
             path_30 = "/run/media/z/Z/means/all/30_deg/",
             bins = c(10, 20, 50, 100, 150))

pcaTrain <- doPCAforBins(path_20 = "/run/media/z/Z/means/train/20_deg/",
             path_25 = "/run/media/z/Z/means/train/25_deg/",
             path_30 = "/run/media/z/Z/means/train/30_deg/",
             bins = c(10, 20, 50, 100, 150))

pcaTest <- doPCAforBins(path_20 = "/run/media/z/Z/means/test/20_deg/",
             path_25 = "/run/media/z/Z/means/test/25_deg/",
             path_30 = "/run/media/z/Z/means/test/30_deg/",
             bins = c(10, 20, 50, 100, 150))

# PCA plots

doPCAforBins(path_20 = "/run/media/z/Z/means/all/20_deg/",
             path_25 = "/run/media/z/Z/means/all/25_deg/",
             path_30 = "/run/media/z/Z/means/all/30_deg/",
             bins = c(10, 20, 50, 100, 150),
             plot = TRUE,
             outpath = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/PCA/PCA_binTests/Entire_population/")

doPCAforBins(path_20 = "/run/media/z/Z/means/train/20_deg/",
             path_25 = "/run/media/z/Z/means/train/25_deg/",
             path_30 = "/run/media/z/Z/means/train/30_deg/",
             bins = c(10, 20, 50, 100, 150),
             plot = TRUE,
             outpath = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/PCA/PCA_binTests/Train_data/")

doPCAforBins(path_20 = "/run/media/z/Z/means/test/20_deg/",
             path_25 = "/run/media/z/Z/means/test/25_deg/",
             path_30 = "/run/media/z/Z/means/test/30_deg/",
             bins = c(10, 20, 50, 100, 150),
             plot = TRUE,
             outpath = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/PCA/PCA_binTests/Test_data/")

# Contribution results ----------

pcContribution <- function(pca) round(pca$sdev^2/sum(pca$sdev^2)*100)

contributionAll <- lapply(pcaAll, pcContribution)
contributionTrain <- lapply(pcaTrain, pcContribution)
contributionTest <- lapply(pcaTest, pcContribution)

plotContribution <- function(vals, outpath, bins) {
  
  for (i in 1:length(bins)) {
    
    valData <- data.frame(pc = 1:length(vals[[i]]), val = vals[[i]])
    
    pcvar <- ggplot(valData, aes(pc, val)) +
      geom_col() +
      labs(title = "Percentage variance explained by each PC", 
           x = "Principal Component",
           y = "Variance explained (%)")
    
    plot.save(plot = pcvar, width = 750, height = 550, 
              filename = paste0("variance_bins-", bins[i], ".png"),
              path = outpath)
    
  }

}

plotContribution(vals = contributionAll, 
                 outpath = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/PCA/PCA_binTests/Entire_population/",
                 bins = c(10, 20, 50, 100, 150))

plotContribution(vals = contributionTrain, 
                 outpath = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/PCA/PCA_binTests/Train_data/",
                 bins = c(10, 20, 50, 100, 150))

plotContribution(vals = contributionTest, 
                 outpath = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/PCA/PCA_binTests/Test_data/",
                 bins = c(10, 20, 50, 100, 150))

# ANOVA tests --------------

results <- list()
Names <- names(pcaAll)

# Conduct multi-ANOVA's
for (i in 1:length(pcaAll)) {
  lencol <- length(pcaAll[[i]]$x)
  results[[Names[i]]] <- maov(3:lencol, "group", pcaAll[[i]]$x)
  results[[Names[i]]] <- lapply(results[[Names[i]]], summary.aov)
  results[[Names[i]]] <- results[[Names[i]]] # Required to evaluate the ANOVA call
}

# Get p-Values
for (bin in names(results)) {
  for (pc in names(results[[bin]])) {
    results[[bin]][[pc]] <- results[[bin]][[pc]][[1]][[5]][[1]] 
  }
}

# Reformat values to easy to plot dataframes
for (bin in names(results)) {
  results[[bin]] <- unlist(results[[bin]])
  results[[bin]] <- data.frame(results[[bin]])
  results[[bin]] <- data.frame(pc = 1:length(results[[bin]][,1]), p = results[[bin]][,1])
}

results$bin10$pc <- factor(results$bin10$pc) # to improve x axis labelling for small bin number

for (bin in names(results)) {
  
  aovPlot <- ggplot(results[[bin]], aes(pc, p)) + 
    geom_col() +
    scale_y_continuous(trans = "log10") +
    labs(title = "ANOVA results for PCA analysis",
         subtitle = paste0("Bins = ", 
                           gsub("bin", "", bin), 
                           ", Species = Radix Balthica"),
         x = "Principal component",
         y = "p-value (log10 scale)") + 
    theme_bw()
  
  plot.save(aovPlot, 
            width = 750, 
            height = 550, 
            filename = paste0(bin, ".png"),
            path = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/PCA/PCA_binTests/Entire_population/")
  
}

