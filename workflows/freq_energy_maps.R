source("/home/z/Desktop/github/misc_embryoPhenomics/R/data_handling.R")
source("/home/z/Desktop/github/misc_embryoPhenomics/R/data_vis.R")

library(parallel)
library(pbmcapply)

# Hatch times to exclude data from
hatchTimes <- read.csv("/run/media/z/Z/test data/hatchTime.csv", header=TRUE)
hatch20 <- hatchTimes[hatchTimes$Temperature == 20,]
hatch25 <- hatchTimes[hatchTimes$Temperature == 25,]
hatch30 <- hatchTimes[hatchTimes$Temperature == 30,]

# Embryos to exclude
group20 <- c('A5', 'F3', 'B5', 'C3','E1','E7')
group25 <- c('A2', 'A3', 'D7','E4')
group30 <- c('B2','B4','B7','B8','C2','C4','E5','E7','E8','A2','A3','D3','D5','F5','F3')

completeName <- function(name) {
  name <- paste0(name, "dataset.HDF5")
}

# Complete to filenames for removing from file list
group20 <- sapply(group20, completeName)
group25 <- sapply(group25, completeName)
group30 <- sapply(group30, completeName)

files20 <- list.files("/run/media/z/Z/embryoCV_data/20_deg/", pattern = ".HDF5")
files25 <- list.files("/run/media/z/Z/embryoCV_data/25_deg/", pattern = ".HDF5")
files30 <- list.files("/run/media/z/Z/embryoCV_data/30_deg/", pattern = ".HDF5")

# Exclude embryos from file list
files20 <- files20[!grepl(paste0(group20, collapse = "|"), files20)]
files25 <- files25[!grepl(paste0(group25, collapse = "|"), files25)]
files30 <- files30[!grepl(paste0(group30, collapse = "|"), files30)]

names20 <- sapply(files20, function(x) gsub(pattern="dataset.HDF5", "", x))
names25 <- sapply(files25, function(x) gsub(pattern="dataset.HDF5", "", x))
names30 <- sapply(files30, function(x) gsub(pattern="dataset.HDF5", "", x))

path20 <- "/run/media/z/Z/embryoCV_data/20_deg/"
path25 <- "/run/media/z/Z/embryoCV_data/25_deg/"
path30 <- "/run/media/z/Z/embryoCV_data/30_deg/"

plotEnergy <- function(array, outpath, hatchTime) {
  
  freqs <- 1:dim(array)[1]
  time <- 1:hatchTime
  
  pbmclapply(1:length(freqs), function(f) {
    
    library(ggplot2)
    
    f_path = paste0(outpath, freqs[f], "/")
    dir.create(f_path)
    
    for (t in 1:length(time)) {
      
      data <- as.matrix(array[f, 2, 1:16, 1:16, t])
      df <- matrixToDataframe(data)
      
      p <- ggplot(df, aes(x, y, fill = z)) +
        geom_raster() + 
        theme_void() +
        theme(legend.position = "none") +
        scale_fill_viridis_c()
      
      plot.save(plot=p, width=750, height=650, filename=paste0(t, ".jpg"), path=f_path)
      
    }
    
  }, mc.cores = 3)
  
}


test <- openXArray("/home/z/Documents/testData/A1.HDF5")

test1 <- getXArrayData("/home/z/Documents/testData/A1.HDF5", "Size_Position_Mean")

data <- data.frame(y = test1[1,1:6], x = 1:6)

ggplot(data, aes(x, y)) + geom_path()

gplotEnergy(array = freq16x16, 
           outpath="/home/z/Desktop/github/misc_embryoPhenomics/R/visualisations/FreqOutput_raw/energy_raw/", 
           hatchTime=hatch20[1, 5])








