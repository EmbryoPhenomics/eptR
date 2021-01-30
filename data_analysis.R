# -------------------------- Data analysis methods --------------------------- #
# ---------------------------------------------------------------------------- #
# This file contains various methods for analysing the data produced   
# from EmbryoCV. Documentation for individual methods can be found in the 
# data_analysis.md file found in this repository.
# ---------------------------------------------------------------------------- #

# A more flexible Multi-ANOVA method for large numbers of response variables
# Note this is not a MANOVA, an ANOVA is computed on each response variable, 
# and the results are returned as a list

# Example usage: maov(responseVars = 1:12, predictVar = "group", data = data)

maov <- function(responseVars, predictVar, data) {
  
  result <- list()
  
  Names <- names(data[,responseVars])
  
  for (name in Names) {
    
    result[[name]] <- aov(data[[name]] ~ data[[predictVar]])
    
  }
  
  return(result)
  
}

# ---------------------------------------------------------------------------- #

# Do PCA for several binning regimes produced through previous methods. 
# Can optionally return plots of the results to a path specified in outpath

# Note this method is only implemented for the 3 temperature groups and is therefore not a generic method

doPCAforBins <- function(path_20, path_25, path_30, bins, plot = FALSE, outpath) {
  
  require(ggplot2, quietly = TRUE)
  
  pca_results <- list()
  
  for (i in 1:length(bins)) {
    
    # Per population rescaling 
    mean20 <- read.csv(paste0(path_20, bins[i], ".csv"))
    mean25 <- read.csv(paste0(path_25, bins[i], ".csv"))
    mean30 <- read.csv(paste0(path_30, bins[i], ".csv"))
    
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
    
    # PCA
    pca <- prcomp(mean_rescaled[,3:ncol(mean_rescaled)])
    
    allPC <- data.frame(group = mean_rescaled$group, time = mean_rescaled[,2], pca$x[,1:ncol(pca$x)])
    allPC$group <- factor(allPC$group)
    
    # Compute contribution of various PC's
    expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100)
    
    if (plot == TRUE) {
      
      pcplot <- ggplot(allPC, aes(PC1, PC2, colour = group)) +
        geom_point() + 
        labs(title = "Mean rescaled power values for various temperature groups",
             subtitle = paste0("PCA transform, bin number = ", bins[i]),
             x = paste("PC1", paste0(expl.var[1], "%")), 
             y = paste("PC2", paste0(expl.var[2], "%"))) + 
        guides(colour = guide_legend(title = "Group:"))
      
      plot.save(pcplot, 
                path = outpath,
                filename = paste0("PCA_bins-", bins[i], ".png"),
                width = 750, 
                height = 550)
      
    } else if (plot == FALSE) {
      
      name <- paste0("bin", bins[i])
      
      #pca$x <- allPC
      #pca$expl.var <- expl.var
      pca_results[[name]] <- pca
      
    }
    
  }
  
  return(pca_results)
  
}

# ---------------------------------------------------------------------------- #

# Randomly sample a list of files (each being an individual) and assign to 
# two subgroups for training/testing a model

# Output is a list of 2: files for training and testing

makeTrainTest_files <- function(files, train = 0.7) {
  
  trainFiles <- sample(files, size = length(files)*train)
  
  allFiles <- cbind(files, files %in% trainFiles)
  testFiles <- allFiles[,1][allFiles[,2] == FALSE]
  
  trainTest <- list(train = trainFiles, 
                    test = testFiles)
  
  return(trainTest)
  
}

# All methods below this point have been deprecated
  # see better implementations in the trialMethods folder

# ---------------------------------------------------------------------------- #

# Train SVM's of a given kernel on training data, where trainDatasets is a list of
  # dataframe containing training data 
# It is intended for mean datasets and not raw individual data 
# Note that this method is single threaded and so will be slower than the parallel implementation below for 
  # large numbers of training files and/or cross-validation parameters

trainSVM <- function(trainDatasets, kernel = "linear", 
                     crossValidate = FALSE, cost, gamma, crossValidateResults = FALSE) {
  
  require(e1071)
  
  perfList <- list()
  
  for (i in 1:length(trainDatasets)) {
    
    if (crossValidate == TRUE) {
      
      # Perform cross validation 
      tune.out <- tune(svm, group~., 
                       data = trainDatasets[[i]], 
                       kernel = kernel, 
                       ranges = list(cost = cost,
                                     gamma = gamma))
      
      modelName <- paste0("model", i)
      
      if (crossValidateResults == TRUE)
        perfList[[modelName]] <- tune.out
      else if (crossValidateResults == FALSE)
        perfList[[modelName]] <- tune.out$best.model
      
    } else if (crossValidate == FALSE) {
      
      model <- svm(group~., 
                   data = trainDatasets[[i]],
                   kernel = kernel)
      
      modelName <- paste0("model", i)
      
      perfList[[modelName]] <- model
      
    }
  }
  
  return(perfList)
  
}

# ---------------------------------------------------------------------------- #

# Retrieve the accuracy for a given set of SVM models, requires supplying a grouping class
# returns a list of accuracy values for each model

modelsAccuracy <- function(models, groupVar) {
  
  require(magrittr, quietly = TRUE)
  
  accuracy <- list()
  
  for (i in 1:length(models)) {
    
    dat <- data.frame(pred = models[[i]]$fitted, actual = groupVar)
    
    datError <- ifelse(dat$pred == dat$actual,
                       NA, dat$pred) %>% na.omit()
    
    modelName <- paste0("model", i)
    accuracy[[modelName]] <- (1 - (length(datError) / length(dat$actual))) * 100
    
  }
  
  accuracyData <- data.frame(accuracy = unlist(accuracy))
  
  return(accuracyData)
  
}


# ---------------------------------------------------------------------------- #

# Test the SVM models produced above against testing data
# Returns a list of accuracies and predictions for each model and the corresponding 
  # testing data

testSVM <- function(testDatasets, models) {
  
  require(e1071, quietly = TRUE)
  
  perfList <- list()
  
  for (i in 1:length(testDatasets)) {    
    
    pred <- predict(models[[i]], testDatasets[[i]][,2:ncol(testDatasets[[i]])])
    
    # Compute accuracy and coerce predictions to dataframe format
    dat <- data.frame(pred, actual = as.numeric(as.character(testDatasets[[i]]$group)))
    datError <- ifelse(dat[,1] == dat[,2], 
                       NA, dat[,2])
    accuracy <- (1 - sum(datError, na.rm = TRUE) / sum(dat[,2])) * 100 
    
    modelName <- paste0("prediction_", i)
    perfList[[modelName]] <- list(accuracy = accuracy,
                                  predictions = dat)
    
  }
  
  return(perfList)
  
}


# ---------------------------------------------------------------------------- #
