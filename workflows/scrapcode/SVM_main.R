
setwd("/Users/bioImaging2/Desktop/ziad_scripts/data/means/all/")

source('~/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_wrangling.R')
source('~/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_handling.R')
source('~/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_analysis.R')
#source('~/Desktop/ziad_scripts/misc_embryoPhenomics-master/R/data_vis.R')

library(e1071)

# list of PC's to use, list of binning regimes to use

# iterate through binning regimes in raw data, test prediction accuracy at differing levels of binning

# iterate through binning regimes, and test prediction accuracies for varying numbers of PC's used. 

# test for radial and sigmoid classifiers


# methods required for raw data
# retrieve files for all individuals in training group
# compile into main dataframe
# conduct cross validation for SVM with kernel of choice
# use best model for predicting testing data
# calculate number of wrong classifications and return error rate
# iterate across binning regimes and return table of error rates alongside binning regime 

# apply the same as above except transform to PC's, and iterate across number of PC's i.e. 2:n(PC)
# If time, perhaps attempt differing combinations of individual PC's - likely to take a long time but will 
# be interesting in terms of best performing classifier depending on initial PC data

train20 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/20_deg/", ".csv", full.names = TRUE)
train25 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/25_deg/", ".csv", full.names = TRUE)
train30 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/30_deg/", ".csv", full.names = TRUE)

test20 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/20_deg/", ".csv", full.names = TRUE)
test25 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/25_deg/", ".csv", full.names = TRUE)
test30 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/30_deg/", ".csv", full.names = TRUE)

trainModels <- trainSVM(train_20 = train20, 
                        train_25 = train25,
                        train_30 = train30,
                        kernel = "radial",
                        crossValidate = FALSE)

trainData <- aggregateData(train20, train25, train30)
trainModels <- trainSVM(trainDatasets = trainData, groupingVar = "group")



table(trainModels$model1$fitted, trainData$data_1$group)


group <- c(rep(20, times = 367), rep(25, times  = 364), rep(30, times = 364))
modelsAccuracy(trainModels, group)

pred10 <- predict(trainModels$model2, pcatestDat$bin150$x[,3:152])

dat <- data.frame(pred10, group = group)
datError <- ifelse(dat[,1] == dat[,2], 
                   NA, dat[,2])
modelName <- paste0("model", i)
(1 - sum(datError, na.rm = TRUE) / sum(dat[,2])) * 100

pcaDat <- doPCAforBins(path_20 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/20_deg/",
                       path_25 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/25_deg/",
                       path_30 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/30_deg/",
                       bins = c(10, 20, 50, 100, 150))

trainSVM <- function(pcData, kernel = "linear") {
  
  require(e1071, quietly = TRUE)
  
  perfList <- list()
  
  for (i in 1:length(pcData)) {
    
    dat <- pcData[[i]]$x[,c(1, 3:ncol(pcData[[i]]$x))]
    
    model <- svm(group~., data = dat, kernel = kernel)
    
    modelName <- paste0("model_", names(pcData)[i])
    
    perfList[[modelName]] <- model 
   
  }
  
  return(perfList)
  
}

t <- trainSVM(pcData = pcaDat, kernel = "sigmoid")
trainingAccuracy(t, group) # higher pc number results in greater accuracy

pcatestDat <- doPCAforBins(path_20 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/20_deg/",
                           path_25 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/25_deg/",
                           path_30 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/30_deg/",
                           bins = c(10, 20, 50, 100, 150))

pred10 <- predict(t$model_bin150, pcatestDat$bin150$x[,3:152])

dat <- data.frame(pred10, group = group)
datError <- ifelse(dat[,1] == dat[,2], 
                   NA, dat[,2])
modelName <- paste0("model", i)
(1 - sum(datError, na.rm = TRUE) / sum(dat[,2])) * 100


testData <- aggregateData(files20 = test20,
                          files25 = test25,
                          files30 = test30)


test <- testSVM(models = trainModels,
                testDatasets = testData)

model <- trainModels$model1
data <- testData$data_1[,2:12]

pred <- predict(model, data)



testSVM(test_20 = test20,
        test_25 = test25,
        test_30 = test30,
        models = trainModels)
