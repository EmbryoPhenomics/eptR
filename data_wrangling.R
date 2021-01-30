
# -------------------------- Data wrangling methods --------------------------- #
# ---------------------------------------------------------------------------- #
# This file contains various methods for manipulating the XArray files produced   
# from EmbryoCV. Documentation for individual methods can be found in the 
# data_wrangling.md file found in this repository.
# ---------------------------------------------------------------------------- #

# Divide a matrix into bins and return the mean for each bin. Output is in dataframe format with a 
# column for column number. 
  # Note that the output has been transposed from the original matrix. 

binMatrix_mean <- function(matrix, bins) {
  
  if (requireNamespace("magrittr", quietly = TRUE)) {
    
    startRow <- ceiling(seq(1, nrow(matrix), length.out = bins + 1))
    
    endRow <- startRow
    endRow[1] <- NA
    endRow <- na.omit(endRow)
    
    startRow <- startRow + 1
    startRow[1] <- 1
    startRow[length(startRow)] <- NA
    startRow <- na.omit(startRow)
    
    rowInfo <- data.frame(startRow, endRow)
    
    df <- data.frame(cols = 1:ncol(matrix))
    
    for (i in 1:length(rowInfo$startRow)) {
      
      colName <- paste0("bin", i)
      
      binMean <- matrix[rowInfo$startRow[i] : rowInfo$endRow[i], 1:ncol(matrix)] %>%
        colMeans()
      
      df[[colName]] <- binMean
      
    }
    
    return(df)
    
  } else {
    
    stop("magrittr is required for this function.")
    
    
  }
  
}

# ---------------------------------------------------------------------------- #

# Divide a matrix into bins and return the sum for each bin. Output is in dataframe format with a 
  # column for column number.
  # Note that the output has been transposed from the original matrix. 

binMatrix_sum <- function(matrix, bins) {
  
  if (requireNamespace("magrittr", quietly = TRUE)) {
    
    startRow <- ceiling(seq(1, nrow(matrix), length.out = bins + 1))
    
    endRow <- startRow
    endRow[1] <- NA
    endRow <- na.omit(endRow)
    
    startRow <- startRow + 1
    startRow[1] <- 1
    startRow[length(startRow)] <- NA
    startRow <- na.omit(startRow)
    
    rowInfo <- data.frame(startRow, endRow)
    
    df <- data.frame(cols = 1:ncol(matrix))
    
    for (i in 1:length(rowInfo$startRow)) {
      
      colName <- paste0("bin", i)
      
      binSum <- matrix[rowInfo$startRow[i] : rowInfo$endRow[i], 1:ncol(matrix)] %>%
        colSums()
      
      df[[colName]] <- binSum
      
    }
    
    return(df)
    
  } else {
    
    stop("magrittr is required for this function.")
    
  }
  
}

# ---------------------------------------------------------------------------- #

# Bin the Freqeuncy/Power output (1x1) for a single embryo 
  # Subdataset from the original HDF5 file will come under the name FreqOutput_1x1

bin_FreqOutput1x1 <- function(array, binMethod = "mean", bins = 10, hatchTime = NULL) {
  
  mat <- array[1:dim(array)[1], 2, 1:dim(array)[3]]
  
  if (binMethod == "sum") 
    binnedMat <- binMatrix_sum(matrix = mat, bins = bins)
  else if (binMethod == "mean") 
    binnedMat <- binMatrix_mean(matrix = mat, bins = bins)
  else 
    stop("Only mean and sum type binning methods are supported.")
  
  if (is.null(hatchTime))
    data <- binnedMat
  else
    data <- binnedMat[binnedMat$cols <= hatchTime, ]
  
  return(data)
  
}

# ---------------------------------------------------------------------------- #

# Bin and compute the mean of 1x1 frequency/power arrays from a specified 
  # path to the individual HDF5 files obtained through EmbryoCV

mean_binnedFreqOutput <- function(path, files = NULL, bins) {
  
  if (is.null(files))
    files <- list.files(pattern = ".HDF5", path = path)
  
  df_all <- list()
  
  for (bin in 1:bins) {
    binName <- paste0("bin", bin)
    df_all[[binName]] <- list()
  }
  
  for (n in names(df_all)) {
    
    for (file in files) {
      
      array <- getXArrayData_basic(paste0(path, file), "FreqOutput_1x1")
      binned_data <- bin_FreqOutput1x1(array, bins = bins)
      
      name <- gsub(".HDF5", "", file)
      df_all[[n]][[name]] <- binned_data[[n]]
      
    }
    
    dims <- lapply(df_all[[n]], length)
    maxDim <- max(unlist(dims), na.rm = TRUE)
    
    df_all[[n]] <- matrix(unlist(df_all[[n]]), ncol = length(df_all[[n]]), nrow = maxDim)
    df_all[[n]] <- rowSums(df_all[[n]], na.rm = TRUE)
    
  }
  
  df <- data.frame(cols = 1:length(df_all[[1]]))
  
  for (i in names(df_all)) df[[i]] <- df_all[[i]]
  
  return(df)
  
}

# ---------------------------------------------------------------------------- #

# Retrieve all the FreqOutput_1x1 arrays at a given path for a specified number
  # of individual XArrays.

retrieveFiles <- function(files, path, output = "raw", format = "list", group, rescale = TRUE) {
  
  data <- list()
  
  for (i in 1:length(files)) {
    
    array <- getXArrayData_basic(paste0(path, files[i]), "FreqOutput_1x1")
    
    if (output == "matrix")
      dat <- t(array[1:dim(array)[1], 2, 1:dim(array)[3]])
    else if (output == "raw")
      dat <- array
    
    name <- gsub("dataset.HDF5", "", files[i])
    data[[name]] <- dat
  }
  
  if (format == "list") {
    
    return(data)
    
  } else if (format == "dataframe") {
    
    # Calculate length of dataframe row-wise
    times <- data.frame(unlist(lapply(data, nrow)))
    totalTime <- sum(times[,1])
    
    # Compute stfart and end rows for compiling individual data
    rowInfo <- data.frame(numRows = times[,1])
    rowInfo$endRow <- cumsum(rowInfo$numRows)
    rowInfo$startRow <- (rowInfo$endRow - rowInfo$numRows) + 1
    
    # Set up empty dataframe - group included if format option is dataframe
    df <- data.frame(group = rep(group, times = totalTime), embryo = NA)
    df[,3:303] <- NA # 301 is known frequency number
    
    embryoNames <- names(data)
    
    for (i in 1:length(data)) {
      
      if (rescale) 
        df[rowInfo$startRow[i] : rowInfo$endRow[i], 3:303] <- rescale(data[[i]], columns = 1:ncol(data[[i]]))
      else
        df[rowInfo$startRow[i] : rowInfo$endRow[i], 3:303] <- data[[i]]
      
      df[rowInfo$startRow[i] : rowInfo$endRow[i], 2] <- embryoNames[i]
    }
    
    return(df)
    
  } else {
    
    stop("Format not supported, only output options are list or dataframe.")
    
  }
  
}

# ---------------------------------------------------------------------------- #

# Compute the frequency with the maximum energy at all time points from a FreqOutput_1x1 array

maxFreq_atTime <- function(array) {
  
  if (requireNamespace("magrittr", quietly = TRUE)) {
    
    data <- array[1:dim(array)[1], 2, 1:dim(array)[3]] %>%
      rescale(byrow = TRUE, bycol = FALSE) %>%
      data.frame()
    
    data$freq <- array[1:dim(array)[1], 1, median(1:dim(array)[3])]
    
    maxFreq <- data.frame(time = 1:dim(array)[3])
    maxFreq$freq <- NA_integer_
    
    for (i in 1:(ncol(data) - 1))
      maxFreq$freq[i] <- data$freq[data[,i] == max(data[,i], na.rm = TRUE)]
    
    return(maxFreq)
    
    
  } else {
    
    stop("magrittr is required for this function.")
    
  }
  
}

# ---------------------------------------------------------------------------- #

# Rescale a dataframe or matrix to 0 - 1, by column or by row - i.e. each column or row is rescaled 
  # independently of other columns or rows. Default is by column. 

rescale <- function(data, columns = 2:ncol(data), rows = 2:nrow(data), byrow = FALSE, bycol = TRUE) {
  
  if (bycol == TRUE & byrow == FALSE) {
    
    for (i in columns) {
      data[,i] <- scale(data[,i],
                        center = min(data[,i], na.rm = TRUE),
                        scale = max(data[,i], na.rm = TRUE) - min(data[,i], na.rm = TRUE))
    }
    
  } else if (byrow == TRUE & bycol == FALSE) {
    
    for (i in rows) {
      data[i,] <- scale(data[i,],
                        center = min(data[i,], na.rm = TRUE),
                        scale = max(data[i,], na.rm = TRUE) - min(data[i,], na.rm = TRUE))
    }
    
  } else {
    
    stop("Can only rescale by rows or by columns, not both.")
    
  }
  
  return(data)
  
}

# ---------------------------------------------------------------------------- #


