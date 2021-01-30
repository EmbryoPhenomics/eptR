# Supply a dataset and kernel parameter to produce a list of models, ordered to the amounts of features they
  # have from the original data
# This is a simple method for feature growth, but where growth is linear and not exhaustive of potential 
  # feature combinations

cumulativeSVM <- function(data, kernel, cost = 1, gamma = 1/length(3:ncol(data)), scale = FALSE) {
  
  models <- list()
  
  message(paste("Cumulative SVM with", length(3:ncol(data)), "features beginning:"))
  pb <- txtProgressBar(min = 1, max = length(3:ncol(data)), initial = 1, style = 3)

  for (i in 3:ncol(data)) {
    
    subData <- data[,c(1, 3:i)]
    
    model <- svm(group~., 
        data = subData, 
        kernel = kernel,
        scale = scale,
        cost = cost,
        gamma = gamma)
    
    modelName <- paste0("model_upto-", i - 2)
    
    models[[modelName]] <- model
    
    setTxtProgressBar(pb, i)
    
  }
  
  models <- models[2:(ncol(data)-2)]
  
  close(pb)
  
  return(models)
  
}

# Parallel implementation of the above
par_cumulativeSVM <- function(data, kernel, scale = FALSE, cost = 1, gamma = 1/length(3:ncol(data)),
                              threads = detectCores() - 1, verbose = TRUE) {
  
  require(parallel, quietly = TRUE)
  require(pbmcapply, quietly = TRUE)
  
  if (verbose == TRUE) {
    
    message(paste("Cumulative SVM with", length(3:ncol(data)), "features beginning:"))
    
    models <- pbmclapply(3:ncol(data), function(i) {
      
      subData <- data[,c(1, 3:i)]
      
      svm(group~., 
          data = subData, 
          kernel = kernel,
          scale = scale,
          cost = cost,
          gamma = gamma)
      
    }, mc.cores = threads)
    
  } else if (verbose == FALSE) {
    
    models <- mclapply(3:ncol(data), function(i) {
      
      subData <- data[,c(1, 3:i)]
      
      svm(group~., 
          data = subData, 
          kernel = kernel,
          scale = scale,
          cost = cost,
          gamma = gamma)
      
    }, mc.cores = threads)
    
  }
  
  models <- models[2:(ncol(data)-2)]
  
  return(models)
  
}
