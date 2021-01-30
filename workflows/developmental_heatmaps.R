library(ggplot2)
library(gridExtra)

# Developmental time series heatmaps

source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_wrangling.R')
#source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_handling.R')
#source('/home/z/Desktop/github/misc_embryoPhenomics/R//data_analysis.R')
#source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_vis.R')

hatchTime <- read.csv("/run/media/z/Z/test data/hatchTime.csv")

# Compute mean hatchtime for individual temperatures
hatchTime20 <- round(mean(hatchTime$HatchTime[hatchTime$Temperature == 20], na.rm = TRUE))
hatchTime25 <- round(mean(hatchTime$HatchTime[hatchTime$Temperature == 25], na.rm = TRUE))
hatchTime30 <- round(mean(hatchTime$HatchTime[hatchTime$Temperature == 30], na.rm = TRUE))

# Import population means - here for a binning regime of 150
mean20 <- read.csv("/run/media/z/Z/means/all/20_deg/150.csv")
mean25 <- read.csv("/run/media/z/Z/means/all/25_deg/150.csv")
mean30 <- read.csv("/run/media/z/Z/means/all/30_deg/150.csv")

# Normalise population means
mean20_rescaled <- rescale(mean20, columns = 3:ncol(mean20))
mean25_rescaled <- rescale(mean25, columns = 3:ncol(mean25))
mean30_rescaled <- rescale(mean30, columns = 3:ncol(mean30))

# Subset mean data to corresponding mean hatch time
mean20 <- mean20[mean20$cols <= hatchTime20,]
mean25 <- mean25[mean25$cols <= hatchTime25,]
mean30 <- mean30[mean30$cols <= hatchTime30,]

mean20_rescaled <- mean20_rescaled[mean20_rescaled$cols <= hatchTime20,]
mean25_rescaled <- mean25_rescaled[mean25_rescaled$cols <= hatchTime25,]
mean30_rescaled <- mean30_rescaled[mean30_rescaled$cols <= hatchTime30,]

# Compute breakpoints
break20 <- round(seq(from = 1, to = hatchTime20, length.out = 6))
break25 <- round(seq(from = 1, to = hatchTime25, length.out = 6))
break30 <- round(seq(from = 1, to = hatchTime30, length.out = 6))

# Split data by breakpoints 
splitAtBreak <- function(data, breaks) {
  
  splitAtBreaks <- list()
  
  startRow <- breaks
  
  endRow <- startRow
  endRow[1] <- NA
  endRow <- na.omit(endRow)
  
  startRow <- startRow + 1
  startRow[1] <- 1
  startRow[length(startRow)] <- NA
  startRow <- na.omit(startRow)
  
  rowInfo <- data.frame(startRow, endRow)
  
  for (i in 1:nrow(rowInfo)) {
    
    atBreak <- data[rowInfo[i,1]:rowInfo[i,2],]
    name <- paste0("break", rowInfo[i,2])
    splitAtBreaks[[name]] <- atBreak
    
  }
  
  return(splitAtBreaks)
  
}

broken20 <- splitAtBreak(mean20_rescaled, breaks = break20)
broken25 <- splitAtBreak(mean25_rescaled, breaks = break25)
broken30 <- splitAtBreak(mean30_rescaled, breaks = break30)


# Compute correlation for all bins, and return xyz
corForBreaks <- function(datasets, outpath) {
  
  corData <- list()
  
  for (i in 1:length(datasets)) {
    
    data <- datasets[[i]]
    
    df <- data.frame(x = rep.int(1:(ncol(data) - 2), times = ncol(data) - 2),
                     y = rep(1:(ncol(data) - 2), each = ncol(data) - 2),
                     p = NA)
    
    message("Large correlation task starting:")
    pb <- txtProgressBar(min = 1, max = nrow(df), initial = 1, style = 3)
    
    for (row in 1:nrow(df)) {
      
      df[row,3] <- cor(x = data[,df[row,1] + 2], y = data[,df[row,2] + 2], method = "pearson")
      
      setTxtProgressBar(pb, row)
      
    }
    
    write.csv(df, file = paste0(outpath, i/length(datasets), ".csv"))
    
    close(pb)
    
    name <- paste0("cor", i)
    corData[[name]] <- df
    
  }
  
  return(corData)
  
}

df20 <- corForBreaks(broken20, outpath = "/home/z/Desktop/EMP/data/20_deg/bin_150/")
df25 <- corForBreaks(broken25, outpath = "/home/z/Desktop/EMP/data/25_deg/bin_150/")
df30 <- corForBreaks(broken30, outpath = "/home/z/Desktop/EMP/data/30_deg/bin_150/")

# Create grobs for final developmental heatmap
p20 <- list(p1_20 = ggplot(df20$cor1, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p2_20 = ggplot(df20$cor2, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p3_20 = ggplot(df20$cor3, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p4_20 = ggplot(df20$cor4, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p5_20 = ggplot(df20$cor5, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c())

p25 <- list(p1_25 = ggplot(df25$cor1, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p2_25 = ggplot(df25$cor2, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p3_25 = ggplot(df25$cor3, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p4_25 = ggplot(df25$cor4, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p5_25 = ggplot(df25$cor5, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c())

p30 <- list(p1_30 = ggplot(df30$cor1, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p2_30 = ggplot(df30$cor2, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p3_30 = ggplot(df30$cor3, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p4_30 = ggplot(df30$cor4, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c(),
            p5_30 = ggplot(df30$cor5, aes(x,y,fill = p)) + geom_raster() + theme_void() + theme(legend.position = "none") + scale_fill_viridis_c())

grid.arrange(grobs = c(p20,p25,p30), ncol = 5, nrow = 3)
