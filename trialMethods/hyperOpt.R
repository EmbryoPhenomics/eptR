# Hyperparameter optimization for radial SVM's using grid search

hyperOpt <- function(data, scale = FALSE, cost, gamma, verbose = TRUE) {
  
  require(parallel, quietly = TRUE)
  
  models <- list()
  
  paramInfo <- data.frame(cost = rep.int(cost, times = length(gamma)), 
                          gamma = rep(gamma, each = length(cost)),
                          score = NA)
  
  message(paste("Testing", nrow(paramInfo), "total parameter combinations:"))
  pb <- txtProgressBar(min = 1, max = nrow(paramInfo), initial = 1, style = 3)
  
  for (x in 1:nrow(paramInfo)) {
    
    model <- svm(group~., 
                 data = data,
                 kernel = "radial",
                 scale = scale,
                 cost = paramInfo$cost[x],
                 gamma = paramInfo$gamma[x])
    
    dat <- data.frame(pred = model$fitted, actual = data[["group"]])
    
    datError <- ifelse(dat$pred == dat$actual,
                       NA, dat$pred) %>% na.omit()
    
    paramInfo$score[x] <- (1 - (length(datError) / length(dat$actual))) * 100
    
    setTxtProgressBar(pb, x)
    
  }
  
  close(pb)
  
  return(paramInfo)
  
}
