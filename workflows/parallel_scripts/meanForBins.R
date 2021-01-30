#!/usr/bin/env Rscript

# Exclude embryos 
group20 <- c('A5', 'F3', 'B5', 'C3','E1','E7')
group25 <- c('A2', 'A3', 'D7','E4')
group30 <- c('B2','B4','B7','B8','C2','C4','E5','E7','E8','A2','A3','D3','D5','F5','F3')

completeName <- function(name) {
  name <- paste0(name, "dataset.HDF5")
}

group20 <- sapply(group20, completeName)
group25 <- sapply(group25, completeName)
group30 <- sapply(group30, completeName)

files20 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/", pattern = ".HDF5")
files25 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/", pattern = ".HDF5")
files30 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/", pattern = ".HDF5")

files20 <- files20[!grepl(paste0(group20, collapse = "|"), files20)]
files25 <- files25[!grepl(paste0(group25, collapse = "|"), files25)]
files30 <- files30[!grepl(paste0(group30, collapse = "|"), files30)]
files30[28] <- NA
files30 <- as.character(na.omit(files30))

makeTrainTest_files <- function(files, train = 0.7) {
  
  trainFiles <- sample(files, size = length(files)*train)
  
  allFiles <- cbind(files, files %in% trainFiles)
  testFiles <- allFiles[,1][allFiles[,2] == FALSE]
  
  trainTest <- list(train = trainFiles, 
                    test = testFiles)
  
  return(trainTest)
  
}

# Split populations into train/validate sub-groups
trainTest20 <- makeTrainTest_files(files20)
trainTest25 <- makeTrainTest_files(files25)
trainTest30 <- makeTrainTest_files(files30)

# Compile into metadata for parallel compute
metadata <- data.frame(group = rep(c(20,25,30), each = 3),
                       inpaths = rep(c("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
                                       "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
                                       "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/"),
                                     each = 3),
                       outpaths = c("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/20_deg/",
                                    "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/20_deg/",
                                    "/Users/bioImaging2/Desktop/ziad_scripts/data/means/all/20_deg/",
                                    "/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/25_deg/",
                                    "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/25_deg/",
                                    "/Users/bioImaging2/Desktop/ziad_scripts/data/means/all/25_deg/",
                                    "/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/30_deg/",
                                    "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/30_deg/",
                                    "/Users/bioImaging2/Desktop/ziad_scripts/data/means/all/30_deg/"),
                       files = c("trainTest20$train",
                                 "trainTest20$test",
                                 "files20",
                                 "trainTest25$train",
                                 "trainTest25$test",
                                 "files25",
                                 "trainTest30$train",
                                 "trainTest30$test",
                                 "files30")
                       )
metadata$inpaths <- as.character(metadata$inpaths)
metadata$outpaths <- as.character(metadata$outpaths)
metadata$files <- as.character(metadata$files)

meanForBins <- function(metadata, bins) {
  
  require(parallel)
 
  mclapply(1:length(metadata$group), function(i) {
    
    source("/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_handling/binMatrix_mean.R")
    source('/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_handling/mean_binnedFreqOutput.R')
    source('/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_handling/getXArrayData.R')
    source('/Users/bioImaging2/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/bin_FreqOutput1x1.R')
    
    for (bin in 1:length(bins)) {
      
      mBin <- mean_binnedFreqOutput(path = metadata$inpaths[i], 
                                    files = eval(parse(text = metadata$files[i])),
                                    bins = bins[[bin]])
      
      write.csv(mBin, file = paste0(metadata$outpaths[i], bins[[bin]], ".csv"))
      
    }
    
  },
  mc.cores = detectCores() - 1)
  
}

meanForBins(metadata = metadata, bins = c(3, 10, 20, 50, 100, 150))


