# Method for applying varying optimal parameter combinations with cumulativeSVM()

multi_cumulativeSVM <- function(trainDataset, testDataset, response, threads, params, verbose = TRUE) {
  
  results <- list()
  
  for (x in 1:nrow(params)) {
    
    models <- par_cumulativeSVM(data = trainDataset, 
                                kernel = "radial", 
                                cost = params$cost[x], 
                                gamma = params$gamma[x],
                                threads = threads,
                                verbose = verbose)
    
    valid <- parTest_cumulativeSVM(data = testDataset,
                                   models = models, 
                                   response = response,
                                   threads = threads,
                                   verbose = verbose)
    
    name <- paste0("params", x)
    
    results[[name]] <- list(cost = params$cost[x],
                            gamma = params$gamma[x],
                            accuracy = data.frame(Vars = 1:length(models), score = unlist(valid)))
    
  }
  
  return(results)
  
}


