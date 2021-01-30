library(e1071)
library(magrittr)

source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_wrangling.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_handling.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_analysis.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/trialMethods/aggregateData.R')

#train20 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/20_deg/", ".csv", full.names = TRUE)
#train25 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/25_deg/", ".csv", full.names = TRUE)
#train30 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/30_deg/", ".csv", full.names = TRUE)

#test20 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/20_deg/", ".csv", full.names = TRUE)
#test25 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/25_deg/", ".csv", full.names = TRUE)
#test30 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/30_deg/", ".csv", full.names = TRUE)

# ----------------------------------------- Raw data model training and testing ---------------------------------------

trainData <- aggregateData(path_20 = "/run/media/z/Z/means/train/20_deg/",
                           path_25 = "/run/media/z/Z/means/train/25_deg/", 
                           path_30 = "/run/media/z/Z/means/train/30_deg/",
                           bins = c(10, 20, 50, 100, 150))

testData <- aggregateData(path_20 = "/run/media/z/Z/means/test/20_deg/",
                          path_25 = "/run/media/z/Z/means/test/25_deg/", 
                          path_30 = "/run/media/z/Z/means/test/30_deg/",
                          bins = c(10, 20, 50, 100, 150))

# trainModels <- trainSVM(trainDatasets = trainData, kernel = "radial")

# table(trainModels$model5$fitted, trainData$data_150$group)

# # Testing initial model
# dat <- data.frame(pred = trainModels$model5$fitted, actual = trainData$data_150$group)

# datError <- ifelse(dat$pred == dat$actual,
#                    NA, dat$pred) %>% na.omit()

# accuracy <- (1 - (length(datError) / length(dat$actual))) * 100

# # Testing against test data
# pred <- predict(trainModels$model5, testData$data_150[,2:152])
# table(pred, testData$data_150$group)

# dat <- data.frame(pred = pred, actual = testData$data_150$group)

# datError <- ifelse(dat$pred == dat$actual,
#                    NA, dat$pred) %>% na.omit()

# accuracy <- (1 - (length(datError) / length(dat$actual))) * 100

# Training SVM with varying levels of feature growth
testModels <- cumulativeSVM(trainData$data_150, kernel = "radial")  
testModels <- par_cumulativeSVM(trainData$data_150, kernel = "radial", threads = 3)
  
t <- data.frame(Vars = 2:150, modelsAccuracy(testModels, groupVar = trainData$data_10$group))
  
library(ggplot2)
ggplot(t, aes(Vars, accuracy)) + geom_path()

# Testing SVM's produced from previous feature growth
valid <- test_cumulativeSVM(testData$data_150, models = testModels, response = "group")
valid <- parTest_cumulativeSVM(testData$data_150, models = testModels, response = "group", threads = 3)

validData <- data.frame(Vars = 2:150, score = unlist(valid))

ggplot(validData, aes(Vars, score)) + geom_path()

# For all binning regimes

testDatasets <- list()

for (i in 1:length(trainData)) {
  
  testModels <- growSVM(trainData[[i]], kernel = "radial")  
  
  valid <- testGrownSVM(testData = testData[[i]], models = testModels, group = "group")
  validData <- data.frame(x = 1:length(testModels), y = unlist(valid))
  
  name <- paste0("bin_", i)
  
  testDatasets[[name]] <- validData
  
}

ggplot() + 
  geom_path(data = testDatasets$bin_1, aes(x, y, color = "10")) + 
  geom_path(data = testDatasets$bin_2, aes(x, y, color = "20")) + 
  geom_path(data = testDatasets$bin_3, aes(x, y, color = "50")) + 
  geom_path(data = testDatasets$bin_4, aes(x, y, color = "100")) + 
  geom_path(data = testDatasets$bin_5, aes(x, y, color = "150")) + 
  labs(title = "Accuracy of SVM's trained on varying feature sizes",
       subtitle = "Kernel: radial",
       x = "Number of bins incorporated into SVM",
       y = "Classifier accuracy (%)") +
  guides(color = guide_legend(title = "Bins:")) +
  ylim(0,100)
  

# ----------------------------------------- PCA data model training and testing ---------------------------------------

pcaTrain <- doPCAforBins(path_20 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/20_deg/",
                         path_25 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/25_deg/", 
                         path_30 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/30_deg/",
                         bins = c(10, 20, 50, 100, 150))

pcaTest <- doPCAforBins(path_20 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/20_deg/",
                        path_25 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/25_deg/", 
                        path_30 = "/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/30_deg/",
                        bins = c(10, 20, 50, 100, 150))

# Training SVM with varying levels of feature growth
testModels <- growSVM(pcaTrain$bin150$x, kernel = "sigmoid")  

t <- data.frame(Vars = 2:150, modelsAccuracy(testModels, groupVar = pcaTrain$bin150$x$group))

ggplot(t, aes(Vars, accuracy)) + geom_path()

# Testing SVM's produced from previous feature growth
valid <- testGrownSVM(pcaTest$bin150$x, models = testModels, group = "group")

validData <- data.frame(Vars = 2:150, score = unlist(valid))

ggplot(validData, aes(Vars, score)) + geom_path()



