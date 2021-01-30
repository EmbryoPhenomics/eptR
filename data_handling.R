
# -------------------------- Data handling methods --------------------------- #
# ---------------------------------------------------------------------------- #
# This file contains various methods for handling the XArray files produced   
# from EmbryoCV. Documentation for individual methods can be found in the 
# data_handling.md file found in this repository.
# ---------------------------------------------------------------------------- #

# Retrieve metadata for a particular XArray file. 
  # Only tested on linux.

openXArray <- function(filename, path = NULL) {
  
  RawfileData <- system(command = paste("gdalinfo", paste0(path, filename)), 
                        intern = TRUE, 
                        ignore.stdout = FALSE, 
                        ignore.stderr = TRUE, 
                        wait = TRUE)
  
  arrayNamesRaw    <- RawfileData[grep("^.*SUBDATASET.*NAME", RawfileData)]
  arrayNames_split <- unlist(strsplit(arrayNamesRaw, ":"))
  arrayNamesOnly   <- arrayNames_split[seq(3, length(arrayNames_split), 3)]
  
  arrayDescRaw    <- RawfileData[grep("^.*SUBDATASET.*DESC", RawfileData)]
  arrayDesc_split <- unlist(strsplit(arrayDescRaw, "="))
  arrayDimsRaw    <- arrayDesc_split[seq(2, length(arrayDesc_split), 2)]
  arrayDimsOnly   <- sub(" .*", "", arrayDimsRaw)
  
  if (is.null(path)) 
    Location = getwd()
  else 
    Location = path
  
  metadata <- list(
    Location = Location,
    File = filename,
    Driver = gsub("Driver: ", "", RawfileData[1]),
    SubDataset = data.frame(ArrayName = arrayNamesOnly, ArrayDimensions = arrayDimsOnly)
  )
  
  return(metadata)
  
}

# ---------------------------------------------------------------------------- #

# Import a subdataset of a given XArray file to the global environment (user R session)
  # Only XArray files of NCDF are currently supported

getXArrayData <- function(file, subdataset) {
  
  fileData <- openXArray(file, path = NULL)
  
  if ("netCDF" %in% unlist(strsplit(fileData$Driver, "/"))) {
    
    if (requireNamespace("ncdf4", quietly = TRUE)) {
      
      data <- ncdf4::nc_open(file)
      
      if (subdataset %in% fileData$SubDataset$ArrayName) {
        
        array <- ncdf4::ncvar_get(data, data$var[[subdataset]])
        
        ncdf4::nc_close(data)
        
        return(array)
        
      } else {
        
        warning("Subdataset not in HDF5 file. Make sure it is one of the above.")
        return(subdata)
        
      }
      
    } else {
      
      stop("Package ncdf4 is required for this function. Install using install.packages('ncdf4')")
      
    }
    
  } else {
    
    stop("Only HDF5 files of format NCDF or netCDF (Network Common Data Format) are supported for this method.")
    
  }
  
}

# ---------------------------------------------------------------------------- #

# A simpler implementation of the above which is compatible with any OS (e.g. MacOS)

getXArrayData_basic <- function(file, subdataset) {
  
  if (requireNamespace("ncdf4", quietly = TRUE)) {
    
    data <- ncdf4::nc_open(file)
    
    array <- ncdf4::ncvar_get(data, data$var[[subdataset]])
    
    ncdf4::nc_close(data)
    
    return(array)
    
  } else {
    
    stop("Package ncdf4 is required for this function. Install using install.packages('ncdf4')")
    
  }
  
}

# ---------------------------------------------------------------------------- #

# List all the embryo names found in a particular folder

getEmbryoLabels <- function(path) {
  
  files <- list.files(path = path, pattern = ".HDF5")
  
  onlyEmbryo <- unlist(
    lapply(files, 
           FUN = function(x) gsub("dataset.HDF5", "", x)
    )
  )
  
  return(onlyEmbryo)
  
}

# ---------------------------------------------------------------------------- #

# Convert a matrix to dataframe format

matrixToDataframe <- function(matrix) {
  
  vals <- values(matrix)
  ncols <- 1:ncol(matrix)
  nrows <- 1:nrow(matrix)
  
  xyz <- data.frame(x = rep.int(x = ncols, times = length(nrows)),
                    y = rep(x = nrows, each = length(ncols)), 
                    z = vals)
  return(xyz)
  
} 

# ---------------------------------------------------------------------------- #

# Function for retrieving the values contained in a matrix

values <- function(matrix){
  
  matrix_Vals <- c()
  
  for (i in matrix) matrix_Vals <- append(matrix_Vals, i)
  
  return(matrix_Vals)
  
}

# ---------------------------------------------------------------------------- #


