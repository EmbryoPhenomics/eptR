#!/usr/bin/env Rscript

#############################
# To be run at command line # 
#############################

source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_handling/getXArrayData.R")
source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_handling/binMatrix_mean.R")
source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_vis/plot.save.R")
source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_vis/plot_binned_FreqOutput.R")

library(parallel)
library(gridExtra)
library(ggplot2)

plot_save <- function(array, inpath, outpath, bins) {
  
  file <- getXArrayData_basic(paste0(inpath, array), subdataset = "FreqOutput_1x1")
  
  plot <- plot_binned_FreqOutput(file, bins = bins)
  
  plot.save(
    plot = plot,
    width = 1920,
    height = 1080, 
    filename = gsub(".HDF5", ".png", array),
    path = outpath
    )
}

files20 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/", pattern = ".HDF5")
files25 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/", pattern = ".HDF5")
files30 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/", pattern = ".HDF5")

# Bin = 10 --------------------
mclapply(files20, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_10/",
         bins = 10,
         mc.cores = detectCores() - 1)

mclapply(files25, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_10/",
         bins = 10,
         mc.cores = detectCores() - 1)

mclapply(files30, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_10/",
         bins = 10,
         mc.cores = detectCores() - 1)

# Bin = 20 ---------------------
mclapply(files20, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_20/",
         bins = 20,
         mc.cores = detectCores() - 1)

mclapply(files25, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_20/",
         bins = 20,
         mc.cores = detectCores() - 1)

mclapply(files30, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_20/",
         bins = 20,
         mc.cores = detectCores() - 1)

# Bin = 50 ---------------------
mclapply(files20, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_50/",
         bins = 50,
         mc.cores = detectCores() - 1)

mclapply(files25, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_50/",
         bins = 50,
         mc.cores = detectCores() - 1)

mclapply(files30, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_50/",
         bins = 50,
         mc.cores = detectCores() - 1)

# Bin = 100 --------------------
mclapply(files20, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_100/",
         bins = 100,
         mc.cores = detectCores() - 1)

mclapply(files25, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_100/",
         bins = 100,
         mc.cores = detectCores() - 1)

mclapply(files30, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_100/",
         bins = 100,
         mc.cores = detectCores() - 1)

# Bin = 150 ----------------------
mclapply(files20, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/20_deg/bin_150/",
         bins = 150,
         mc.cores = detectCores() - 1)

mclapply(files25, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/25_deg/bin_150/",
         bins = 150,
         mc.cores = detectCores() - 1)

mclapply(files30, 
         plot_save, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/plots/30_deg/bin_150/",
         bins = 150,
         mc.cores = detectCores() - 1)

