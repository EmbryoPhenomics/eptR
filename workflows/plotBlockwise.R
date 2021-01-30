setwd("/run/media/z/Z/test data")

library(parallel)
library(foreach)
library(doParallel)
source("/home/z/Desktop/github /minor_projects/ancient_woodland/R/fun/plot.save.R")
source("/home/z/Desktop/github /misc_embryoPhenomics/R/data_handling/getXArrayData.R")
source("/home/z/Desktop/github /misc_embryoPhenomics/R/data_handling/matrixToDataframe.R")
source("/home/z/Desktop/github /misc_embryoPhenomics/R/data_handling/values.R")

blockwisetest <- getXArrayData("A2Dataset.HDF5", "BlockWise_16x16")

# =================================================================
# ------ 2D visualisation for animation (exploratory) -------------
# =================================================================

# Produce individual matrices for blockwise signals and plot
for (hr in 1:(dim(blockwisetest)[4])) {
  
  for (frame in 1:(dim(blockwisetest)[3])) {
    
    dataRaw      <- blockwisetest[1:16, 1:16, frame, hr]
    dataNaN_omit <- apply(dataRaw, MARGIN = 2, FUN = function(x) ifelse(x %in% NaN, 0, x))
    
    data_df <- matrixToDataframe(dataNaN_omit) 
    
    p <- ggplot(data_df, aes(x, y, fill = z)) + 
      geom_raster() +
      theme_void() + 
      labs(title = paste("Hour:", hr), 
           subtitle = paste("Frame:", frame)) +
      scale_fill_viridis_c() + 
      guides(fill = guide_legend(title = "Pixel intensity"))
    
    plot.save(plot = p, width = 365, height = 332, filename = paste0(hr, "-", frame, ".png"), 
              path = "/home/z/Desktop/embyrophenomics/testvis/testplots/blockwisetests/16x16")
    
  }
    
}

# Parallelised for loop (working)

plotBlockWise16x16 <- function(array, frame, hr) {
  
  require(ggplot2)
  
  dataRaw      <- array[1:16, 1:16, frame, hr]
  dataNaN_omit <- apply(dataRaw, MARGIN = 2, FUN = function(x) ifelse(x %in% NaN, 0, x))
  
  data_df <- matrixToDataframe(dataNaN_omit) 
  data_df$z <- as.numeric(data_df$z)
  
  p <- ggplot(data_df, aes(x, y, fill = z)) + 
    geom_raster() +
    theme_void() + 
    labs(title = paste("Hour:", hr), 
         subtitle = paste("Frame:", frame)) +
    scale_fill_viridis_c() + 
    guides(fill = guide_legend(title = "Pixel intensity"))
  
  plot.save(plot = p, width = 530, height = 430, filename = paste0(hr, "-", frame, ".png"), 
            path = "/home/z/Desktop/embyrophenomics/testvis/testplots/blockwise/16x16/")
  
}

ncores <- detectCores()
cl     <- makeCluster(ncores - 1)
registerDoParallel(cl)

foreach (hr = 1:(dim(blockwisetest)[4])) %dopar% {
  
  for (frame in 1:(dim(blockwisetest)[3])) {
    
    plotBlockWise16x16(blockwisetest, frame = frame, hr = hr)
    
  }
  
}

stopCluster()

# Blockwise plots for individual hrs, at frame of 300

plotBlockWise16x16_2 <- function(array, frame, hr, path) {
  
  require(ggplot2)
  
  dataRaw      <- array[1:16, 1:16, frame, hr]
  dataNaN_omit <- apply(dataRaw, MARGIN = 2, FUN = function(x) ifelse(x %in% NaN, 0, x))
  
  data_df <- matrixToDataframe(dataNaN_omit) 
  data_df$z <- as.numeric(data_df$z)
  
  p <- ggplot(data_df, aes(x, y, fill = z)) + 
    geom_raster() +
    theme_void() + 
    labs(title = paste("Hour:", hr), 
         subtitle = paste("Frame:", frame)) +
    scale_fill_viridis_c() + 
    guides(fill = guide_legend(title = "Pixel intensity"))
  
  plot.save(plot = p, width = 530, height = 430, filename = paste0(hr, ".png"), 
            path = path)
  
}

# Imperative implementation (parallel)
ncores <- detectCores()
cl     <- makeCluster(ncores - 1)
registerDoParallel(cl)

foreach (hr = 1:(dim(blockwisetest)[4])) %dopar% {
  
  plotBlockWise16x16(blockwisetest, frame = 300, hr = hr)
  
}

stopCluster()

# Declarative implementation (safer) (parallel)
mclapply(1:(dim(blockwisetest)[4]),
         plotBlockWise16x16_2,
         array = blockwisetest,
         frame = 300,
         path = "/home/z/Desktop/embyrophenomics/testvis/testplots/blockwise/16x16/",
         mc.cores = detectCores() - 1)

# =================================================================
# ------ 3D visualisation for animation (exploratory) -------------
#==================================================================

plotBlockWise16x16_3D <- function(array, frame, hr, path) {
  
  require(plot3D)
  
  dataRaw      <- array[1:16, 1:16, frame, hr]
  dataNaN_omit <- apply(dataRaw, MARGIN = 2, FUN = function(x) ifelse(x %in% NaN, 0, x))
  
  png(filename = paste0(path, hr, ".png"),
      width = 700,
      height = 630)
  
  plot3D::persp3D(z = dataNaN_omit,
                  colkey = FALSE,
                  expand = 0.5, 
                  bty = "g", 
                  phi = 30, 
                  shade = 0.2, 
                  ltheta = 120,
                  space = 0.3, 
                  ticktype = "detailed", 
                  d = 2,
                  main = paste("Hour:", hr)
                  )
  dev.off()
  
}

mclapply(1:(dim(blockwisetest)[4]),
         plotBlockWise16x16_3D,
         array = blockwisetest,
         frame = 300,
         path = "/home/z/Desktop/embyrophenomics/testvis/testplots/blockwise/16x16/3D/",
         mc.cores = detectCores() - 1)
