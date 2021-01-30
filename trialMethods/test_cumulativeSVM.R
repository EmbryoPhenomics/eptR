# Method for producing accuracy scores for individual methods produced with growSVM()
# Returns a list of accuracy scores corresponding to each model

test_cumulativeSVM <- function(data, models, response) {
  
  require(magrittr, quietly = TRUE)
  
  accuracyScores <- list()
  
  modelNames <- 1:length(models)
  
  message("Testing cumulative SVM's:")
  pb <- txtProgressBar(min = 1, max = length(models), initial = 1, style = 3)
  
  for (i in 1:length(models)) {
    
    pred <- predict(models[[i]], data[,3:(i+3)])
    
    dat <- data.frame(pred = pred, actual = data[[response]])
    
    datError <- ifelse(dat$pred == dat$actual,
                       NA, dat$pred) %>% na.omit()
    
    accuracy <- (1 - (length(datError) / length(dat$actual))) * 100
    
    modelName <- paste0("model", modelNames[i])
    accuracyScores[[modelName]] <- accuracy
    
    setTxtProgressBar(pb, i)
    
  }
  
  close(pb)
  
  return(accuracyScores)
  
}


# Parallel implementation of the above
parTest_cumulativeSVM <- function(data, models, response, threads, verbose = TRUE) {
  
  require(magrittr, quietly = TRUE)
  require(pbmcapply, quietly = TRUE)
  require(parallel, quietly = TRUE)
  
  if (verbose == TRUE) {
    
    message("Testing cumulative SVM's:")
    
    accuracyScores <- pbmclapply(1:length(models), function(i) {
      
      pred <- predict(models[[i]], data[,3:(i+3)])
      
      dat <- data.frame(pred = pred, actual = data[[response]])
      
      datError <- ifelse(dat$pred == dat$actual,
                         NA, dat$pred) %>% na.omit()
      
      (1 - (length(datError) / length(dat$actual))) * 100
      
    }, mc.cores = threads)
    
  } else if (verbose == FALSE) {
    
    accuracyScores <- mclapply(1:length(models), function(i) {
      
      pred <- predict(models[[i]], data[,3:(i+3)])
      
      dat <- data.frame(pred = pred, actual = data[[response]])
      
      datError <- ifelse(dat$pred == dat$actual,
                         NA, dat$pred) %>% na.omit()
      
      (1 - (length(datError) / length(dat$actual))) * 100
      
    }, mc.cores = threads)
    
  }
  
  
  return(accuracyScores)
  
}
