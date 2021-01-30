#!/usr/bin/env Rscript

# Data prep
hatchTime <- read.csv("/Users/bioImaging2/Downloads/hatchTime.csv")

temp20 <- hatchTime[hatchTime$Temperature == 20,]
temp25 <- hatchTime[hatchTime$Temperature == 25,]
temp30 <- hatchTime[hatchTime$Temperature == 30,]

files20 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/", pattern = ".HDF5")
files25 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/", pattern = ".HDF5")
files30 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/", pattern = ".HDF5")

meta20 <- data.frame(cbind(ID = temp20$Index, file = files20, hatchTime = temp20$HatchTime))[1:42,]
meta25 <- data.frame(cbind(ID = temp25$Index, file = files25, hatchTime = temp25$HatchTime))[1:44,]
meta30 <- data.frame(cbind(ID = temp30$Index, file = files30, hatchTime = temp30$HatchTime))[1:33,]
meta30 <- na.omit(meta30)

convertFromFactor <- function(data) {
  data$ID        <- as.numeric(as.character(data$ID))
  data$file      <- as.character(data$file)
  data$hatchTime <- as.numeric(as.character(data$hatchTime))
  return(data)
}

meta20 <- convertFromFactor(meta20)
meta25 <- convertFromFactor(meta25)
meta30 <- convertFromFactor(meta30)

# Parallel plotting function
plotBinned_withHatchTime <- function(metadata, inpath, outpath, bins) {
  
  require(foreach)
  require(parallel)
  require(doParallel)
  
  ncores <- detectCores()
  cl     <- makeCluster(ncores - 1)
  registerDoParallel(cl)
  
  foreach (n = 1:length(metadata$ID)) %dopar% {
    
    require(ggplot2)
    require(gridExtra)
    source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_handling/getXArrayData.R")
    source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/bin_FreqOutput1x1.R")
    source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_vis/plot.save.R")
    source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/plot_binned_FreqOutput.R")
    source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_handling/binMatrix_mean.R")
    
    for (i in 1:length(bins)) {
      
      array <- getXArrayData_basic(paste0(inpath, metadata$file[n]), "FreqOutput_1x1")
      binned_data <- bin_FreqOutput1x1(array, bins = bins[i], hatchTime = metadata$hatchTime[n])
      
      plot <- plot_binned_FreqOutput(binned_data)
      
      plot.save(
        plot = plot,
        width = 1920,
        height = 1080,
        filename = gsub(".HDF5", ".png", metadata$file[n]),
        path = outpath[i]
      )
    }
  }
  
  stopCluster(cl)
  
}

# Render plots
plotBinned_withHatchTime(metadata = meta20, 
                         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
                         outpath = c("/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_10/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_20/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_50/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_100/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_150/"), 
                         bins = c(10, 20, 50, 100, 150))

plotBinned_withHatchTime(metadata = meta20, 
                         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
                         outpath = c("/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_10/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_20/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_50/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_100/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_150/"), 
                         bins = c(10, 20, 50, 100, 150))

plotBinned_withHatchTime(metadata = meta20, 
                         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
                         outpath = c("/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_10/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_20/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_50/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_100/",
                                     "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_150/"), 
                         bins = c(10, 20, 50, 100, 150))
