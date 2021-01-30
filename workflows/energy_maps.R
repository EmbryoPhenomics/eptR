library(ggplot2)
library(pbmcapply)
source("/home/z/Desktop/github/misc_embryoPhenomics/R/data_handling.R")
source("/home/z/Desktop/github/misc_embryoPhenomics/R/data_vis.R")

data <- getXArrayData_basic("/home/z/Documents/RealDataTesting/Radix_1/C1dataset.HDF5", "FreqOutput_250x250")

plotEnergy <- function(array, outpath) {
  
  freqs <- 1:dim(array)[1]

  pbmclapply(1:length(freqs), function(f){
  
    jpeg(filename = paste0(outpath, f, ".jpg"))
    print(lattice::levelplot(array[f, 2, 1:250, 1:250], col.regions = terrain.colors(1000), colorkey = FALSE))
    dev.off()
    
  }, mc.cores = 3)
  
} 
  
plotEnergy(array = data, outpath="/home/z/Documents/energy_maps/energy_250_C1_radix/")
