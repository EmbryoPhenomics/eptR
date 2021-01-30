# Script for parallel conversion and binning of FreqOutput_1x1 for further analysis

library(parallel)

files20 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/", pattern = ".HDF5")
files25 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/", pattern = ".HDF5")
files30 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/", pattern = ".HDF5")

# 20 deg conversion and binning -----------
mclapply(files20, 
         import_bin_save_Mac, 
         bins = 10, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/20_deg/bin_10/", 
         mc.cores = detectCores() - 1)

mclapply(files20, 
         import_bin_save_Mac, 
         bins = 20, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/20_deg/bin_20/", 
         mc.cores = detectCores() - 1)

mclapply(files20, 
         import_bin_save_Mac, 
         bins = 50, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/20_deg/bin_50/", 
         mc.cores = detectCores() - 1)

mclapply(files20, 
         import_bin_save_Mac, 
         bins = 100, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/20_deg/bin_100/", 
         mc.cores = detectCores() - 1)

mclapply(files20, 
         import_bin_save_Mac, 
         bins = 150, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/20_deg/bin_150/", 
         mc.cores = detectCores() - 1)

# 25 deg conversion and binning -------------
mclapply(files25, 
         import_bin_save_Mac, 
         bins = 10, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/25_deg/bin_10/", 
         mc.cores = detectCores() - 1)

mclapply(files25, 
         import_bin_save_Mac, 
         bins = 20, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/25_deg/bin_20/", 
         mc.cores = detectCores() - 1)

mclapply(files25, 
         import_bin_save_Mac, 
         bins = 50, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/25_deg/bin_50/", 
         mc.cores = detectCores() - 1)

mclapply(files25, 
         import_bin_save_Mac, 
         bins = 100, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/25_deg/bin_100/", 
         mc.cores = detectCores() - 1)

mclapply(files25, 
         import_bin_save_Mac, 
         bins = 150, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/25_deg/bin_150/", 
         mc.cores = detectCores() - 1)

# 30 deg conversion and binning ------------
mclapply(files30, 
         import_bin_save_Mac, 
         bins = 10, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/30_deg/bin_10/", 
         mc.cores = detectCores() - 1)

mclapply(files30, 
         import_bin_save_Mac, 
         bins = 20, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/30_deg/bin_20/", 
         mc.cores = detectCores() - 1)

mclapply(files30, 
         import_bin_save_Mac, 
         bins = 50, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/30_deg/bin_50/", 
         mc.cores = detectCores() - 1)

mclapply(files30, 
         import_bin_save_Mac, 
         bins = 100, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/30_deg/bin_100/", 
         mc.cores = detectCores() - 1)

mclapply(files30, 
         import_bin_save_Mac, 
         bins = 150, 
         inpath = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
         outpath = "/Users/bioImaging2/Desktop/ziad_scripts/data/binned_1x1_output/30_deg/bin_150/", 
         mc.cores = detectCores() - 1)
