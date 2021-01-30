# Aggregate data and rescale individual populations to 0 - 1
   # For use with population means that are the result of binning

aggregateData <- function(path_20, path_25, path_30, bins) { 
  
  dataList <- list()
  
  for (i in 1:length(bins)) {
    
    # Per population rescaling 
    mean20 <- read.csv(paste0(path_20, bins[i], ".csv"), header = TRUE)
    mean25 <- read.csv(paste0(path_25, bins[i], ".csv"), header = TRUE)
    mean30 <- read.csv(paste0(path_30, bins[i], ".csv"), header = TRUE)
    
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
    
    mean_rescaled <- na.omit(mean_rescaled)
    
    name <- paste0("data_", bins[i])
    
    dataList[[name]] <- mean_rescaled
    
  }
  
  return(dataList)
  
}
