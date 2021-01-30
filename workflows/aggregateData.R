
# Aggregate the results of binning population means, across temperatures
  # For use with analysis methods
  # Each files... argument corresponds to a list of binned data 

aggregateData <- function(files20, files25, files30) { 
  
  dataList <- list()
  
  for (i in 1:length(files20)) {
    
    dat20 <- read.csv(files20[[i]])
    dat25 <- read.csv(files25[[i]])
    dat30 <- read.csv(files30[[i]])
    
    # Add grouping variable and aggregate data
    allfiles <- data.frame(group = rep(c(20,25,30), each = 367))
    allfiles[,2:ncol(dat20)] <- NA
    allfiles[1:367, 2:ncol(dat20)] <- dat20[1:367, 3:ncol(dat20)]
    allfiles[368:731, 2:ncol(dat20)] <- dat25[1:364, 3:ncol(dat20)]
    allfiles[732:1095, 2:ncol(dat20)] <- dat30[1:364, 3:ncol(dat20)]
    allfiles$group <- factor(allfiles$group)
    allfiles <- na.omit(allfiles)
    
    name <- paste0("data_", i)
    
    dataList[[name]] <- allfiles
    
  }
  
  return(dataList)

}

