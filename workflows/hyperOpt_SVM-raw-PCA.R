library(e1071)
library(magrittr)
library(ggplot2)

source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_wrangling.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_handling.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/data_analysis.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/trialMethods/aggregateData.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/trialMethods/multi_cumulativeSVM.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/trialMethods/cumulativeSVM.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/trialMethods/test_cumulativeSVM.R')
source('/home/z/Desktop/github/misc_embryoPhenomics/R/trialMethods/hyperOpt.R')

# ----------------------------------------- Raw data model training and testing ---------------------------------------

trainData <- aggregateData(path_20 = "/run/media/z/Z/means/train/20_deg/",
                           path_25 = "/run/media/z/Z/means/train/25_deg/", 
                           path_30 = "/run/media/z/Z/means/train/30_deg/",
                           bins = c(10, 20, 50, 100, 150))

testData <- aggregateData(path_20 = "/run/media/z/Z/means/test/20_deg/",
                          path_25 = "/run/media/z/Z/means/test/25_deg/", 
                          path_30 = "/run/media/z/Z/means/test/30_deg/",
                          bins = c(10, 20, 50, 100, 150))

# Hyper parameter optimization
paramResults <- hyperOpt(data = trainData$data_150, 
                         scale = FALSE, 
                         cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000),
                         gamma = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10)) 

# Subset to best performing models
paramOpt <- paramResults[paramResults$score == max(paramResults$score),]

# Multi-cumulative SVM for various optimal parameter combinations
test <- multi_cumulativeSVM(trainDataset = trainData$data_150,
                            testDataset = testData$data_150, 
                            response = "group",
                            threads = 3,
                            params = paramOpt,
                            verbose = TRUE)

ggplot() +
  geom_path(data = test[[1]]$accuracy, 
            aes(Vars, score, color = "1")) +
  geom_path(data = test[[2]]$accuracy, 
            aes(Vars, score, color = "2")) +
  geom_path(data = test[[3]]$accuracy, 
            aes(Vars, score, color = "3")) +
  geom_path(data = test[[4]]$accuracy, 
            aes(Vars, score, color = "4")) +
  geom_path(data = test[[5]]$accuracy, 
            aes(Vars, score, color = "5")) +
  geom_path(data = test[[6]]$accuracy, 
            aes(Vars, score, color = "6")) +
  geom_path(data = test[[7]]$accuracy, 
            aes(Vars, score, color = "7")) +
  geom_path(data = test[[8]]$accuracy, 
            aes(Vars, score, color = "8")) +
  geom_path(data = test[[9]]$accuracy, 
            aes(Vars, score, color = "9")) +
  geom_path(data = test[[10]]$accuracy, 
            aes(Vars, score, color = "10")) +
  geom_path(data = test[[11]]$accuracy, 
            aes(Vars, score, color = "11")) +
  geom_path(data = test[[12]]$accuracy, 
            aes(Vars, score, color = "12")) +
  geom_path(data = test[[13]]$accuracy, 
            aes(Vars, score, color = "13")) +
  geom_path(data = test[[14]]$accuracy, 
            aes(Vars, score, color = "14")) +
  geom_path(data = test[[15]]$accuracy, 
            aes(Vars, score, color = "15")) +
  geom_path(data = test[[16]]$accuracy, 
            aes(Vars, score, color = "16")) +
  geom_path(data = test[[17]]$accuracy, 
            aes(Vars, score, color = "17")) +
  geom_path(data = test[[18]]$accuracy, 
            aes(Vars, score, color = "18")) +
  geom_path(data = test[[19]]$accuracy, 
            aes(Vars, score, color = "19"))


# ----------------------------------------- PCA data model training and testing ---------------------------------------

pcaTrain <- doPCAforBins(path_20 = "/run/media/z/Z/means/train/20_deg/",
                         path_25 = "/run/media/z/Z/means/train/25_deg/", 
                         path_30 = "/run/media/z/Z/means/train/30_deg/",
                         bins = c(10, 20, 50, 100, 150))

pcaTest <- doPCAforBins(path_20 = "/run/media/z/Z/means/test/20_deg/",
                        path_25 = "/run/media/z/Z/means/test/25_deg/", 
                        path_30 = "/run/media/z/Z/means/test/30_deg/",
                        bins = c(10, 20, 50, 100, 150))

# Hyper parameter optimization
paramResults <- hyperOpt(data = pcaTrain$bin150$x[,c(1,3:152)], 
                         scale = FALSE, 
                         cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000),
                         gamma = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10)) 

# Subset to best performing models
paramOpt <- paramResults[paramResults$score == max(paramResults$score),]

# Multi-cumulative SVM for various optimal parameter combinations
test <- multi_cumulativeSVM(trainDataset = pcaTrain$bin150$x[,c(1,3:152)],
                            testDataset = pcaTest$bin150$x[,c(1,3:152)], 
                            response = "group",
                            threads = 3,
                            params = paramOpt,
                            verbose = TRUE)

ggplot() +
  geom_path(data = test[[1]]$accuracy, 
            aes(Vars, score, color = "1")) +
  geom_path(data = test[[2]]$accuracy, 
            aes(Vars, score, color = "2")) +
  geom_path(data = test[[3]]$accuracy, 
            aes(Vars, score, color = "3")) +
  geom_path(data = test[[4]]$accuracy, 
            aes(Vars, score, color = "4")) +
  geom_path(data = test[[5]]$accuracy, 
            aes(Vars, score, color = "5")) +
  geom_path(data = test[[6]]$accuracy, 
            aes(Vars, score, color = "6")) +
  geom_path(data = test[[7]]$accuracy, 
            aes(Vars, score, color = "7")) +
  geom_path(data = test[[8]]$accuracy, 
            aes(Vars, score, color = "8")) +
  geom_path(data = test[[9]]$accuracy, 
            aes(Vars, score, color = "9")) +
  geom_path(data = test[[10]]$accuracy, 
            aes(Vars, score, color = "10")) +
  geom_path(data = test[[11]]$accuracy, 
            aes(Vars, score, color = "11")) +
  geom_path(data = test[[12]]$accuracy, 
            aes(Vars, score, color = "12")) +
  geom_path(data = test[[13]]$accuracy, 
            aes(Vars, score, color = "13")) +
  geom_path(data = test[[14]]$accuracy, 
            aes(Vars, score, color = "14")) +
  geom_path(data = test[[15]]$accuracy, 
            aes(Vars, score, color = "15")) +
  geom_path(data = test[[16]]$accuracy, 
            aes(Vars, score, color = "16")) +
  geom_path(data = test[[17]]$accuracy, 
            aes(Vars, score, color = "17")) +
  geom_path(data = test[[18]]$accuracy, 
            aes(Vars, score, color = "18")) +
  geom_path(data = test[[19]]$accuracy, 
            aes(Vars, score, color = "19")) +
  geom_path(data = test[[20]]$accuracy, 
            aes(Vars, score, color = "20")) +
  geom_path(data = test[[21]]$accuracy, 
            aes(Vars, score, color = "21")) +
  geom_path(data = test[[22]]$accuracy, 
            aes(Vars, score, color = "22"))






