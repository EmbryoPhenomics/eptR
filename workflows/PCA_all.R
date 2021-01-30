# PCA for all embryos, with select embryos excluded

# Exclude embryos 
group20 <- c('A5', 'F3', 'B5', 'C3','E1','E7')
group25 <- c('A2', 'A3', 'D7','E4')
group30 <- c('B2','B4','B7','B8','C2','C4','E5','E7','E8','A2','A3','D3','D5','F5','F3')

completeName <- function(name) {
  name <- paste0(name, "dataset.HDF5")
}

group20 <- sapply(group20, completeName)
group25 <- sapply(group25, completeName)
group30 <- sapply(group30, completeName)

files20 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/", pattern = ".HDF5")
files25 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/", pattern = ".HDF5")
files30 <- list.files("/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/", pattern = ".HDF5")

files20 <- files20[!grepl(paste0(group20, collapse = "|"), files20)]
files25 <- files25[!grepl(paste0(group25, collapse = "|"), files25)]
files30 <- files30[!grepl(paste0(group30, collapse = "|"), files30)]
files30[28] <- NA
files30 <- as.character(na.omit(files30))

# Split populations into train/validate sub-groups
trainTest20 <- makeTrainTest_files(files20)
trainTest25 <- makeTrainTest_files(files25)
trainTest30 <- makeTrainTest_files(files30)

# ------------------ Raw data for PCA (Poor clustering achieved) -------------------------

# Retrieve data for train sub-group
train20 <- retrieveFiles(files = trainTest20$train, 
                         path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
                         return = "dataframe",
                         group = 20)
train25 <- retrieveFiles(files = trainTest25$train, 
                         path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
                         return = "dataframe",
                         group = 25)
train30 <- retrieveFiles(files = trainTest30$train, 
                         path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
                         return = "dataframe",
                         group = 30)

train20 <- na.omit(train20)
train25 <- na.omit(train25)
train30 <- na.omit(train30)

all <- data.frame(group = rep(NA, times = 15132))
all[,2:303] <- NA

all[1:6938, 1:303] <- train20[1:6938,1:303]
all[6939:11623, 1:303] <- train25[1:4685,1:303]
all[11624:15130, 1:303] <- train30[1:3507,1:303]
all <- na.omit(all)

pca <- prcomp(all[,3:303])

allPC <- data.frame(group = all$group, pca$x[,1:2])
allPC$group <- factor(allPC$group)
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100)

library(ggplot2)
ggplot(allPC, aes(PC1, PC2, colour = group)) +
  geom_point() + 
  labs(x = paste("PC1", paste0(expl.var[1], "%")), y = paste("PC2", paste0(expl.var[2], "%"))) + 
  guides(colour = guide_legend(title = "Group:"))

# --------------------------- Mean data for PCA ---------------------------

# Compute mean freqoutput for all embyros
mean20 <- mean_binnedFreqOutput(path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
                                files = trainTest20$train,
                                bins = 10)
mean25 <- mean_binnedFreqOutput(path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
                                files = trainTest25$train,
                                bins = 10)
mean30 <- mean_binnedFreqOutput(path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
                                files = trainTest30$train,
                                bins = 10)

group20 <- rescale(mean20)
group25 <- rescale(mean25)
group30 <- rescale(mean30)

# Construct final dataframe for PCA
all <- data.frame(group = rep(c(20,25,30), each = 367))
all[,2:11] <- NA
all[1:367, 2:11] <- group20[1:367,2:11]
all[368:731, 2:11] <- group25[1:364,2:11]
all[732:1095, 2:11] <- group30[1:364,2:11]
all$group <- factor(all$group)
all <- na.omit(all)

pca <- prcomp(all[,2:11])
allPC <- data.frame(group = all$group, pca$x[,1:10])
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100)

ggplot(allPC, aes(PC1, PC2, colour = group)) +
  geom_point() + 
  labs(x = paste("PC1", paste0(expl.var[1], "%")), y = paste("PC2", paste0(expl.var[2], "%"))) + 
  guides(colour = guide_legend(title = "Group:"))


# Fitting time to the PCA data
pc20 <- allPC[allPC$group == 20,]
pc20$time <- 1:367
pc25 <- allPC[allPC$group == 25,]
pc25$time <- 1:367
pc30 <- allPC[allPC$group == 30,]
pc30$time <- 1:361

# PC1
ggplot() + 
  geom_path(data = pc20, aes(time, PC1, color = "20 C")) + 
  geom_path(data = pc25, aes(time, PC1, color = "25 C")) +
  geom_path(data = pc30, aes(time, PC1, color = "30 C")) +
  labs(x = "Time (hrs)", y = paste("PC1", paste0(expl.var[1], "%")))

# PC2
ggplot() + 
  geom_path(data = pc20, aes(time, PC2, color = "20 C")) + 
  geom_path(data = pc25, aes(time, PC2, color = "25 C")) +
  geom_path(data = pc30, aes(time, PC2, color = "30 C")) +
  labs(x = "Time (hrs)", y = paste("PC2", paste0(expl.var[2], "%")))

# PC3
ggplot() + 
  geom_path(data = pc20, aes(time, PC3, color = "20 C")) + 
  geom_path(data = pc25, aes(time, PC3, color = "25 C")) +
  geom_path(data = pc30, aes(time, PC3, color = "30 C")) +
  labs(x = "Time (hrs)", y = paste("PC3", paste0(expl.var[3], "%")))


# Predictions for supplementary individuals

test20 <- retrieveFiles(files = trainTest20$test, 
                        path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
                        output = "raw",
                        return = "list")

test25 <- retrieveFiles(files = trainTest25$test, 
                        path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
                        output = "raw",
                        return = "list")

test30 <- retrieveFiles(files = trainTest30$test, 
                        path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
                        output = "raw",
                        return = "list")


predictFromTest <- function(testData) {
  
  predictData <- list()
  
  for (x in 1:length(testData)) {
    
    data <- bin_FreqOutput1x1(testData[[x]])
    data <- rescale(data)
    colnames(data) <- c("cols", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11")
    
    testcoord <- predict(pca, newdata = data[,2:11])
    testcoord <- data.frame(group = factor(rep(20, times = nrow(testcoord))),
                            testcoord)
    testcoord <- na.omit(testcoord)
    testcoord$time <- 1:nrow(testcoord)
    
    name <- names(testData)[x]
    
    predictData[[name]] <- testcoord
    
  }
  
  return(predictData)
  
}

predict20 <- predictFromTest(test20)
predict25 <- predictFromTest(test25)
predict30 <- predictFromTest(test30)

# Note that the sub-data used can change depending on the random sampling above.
ggplot() +
  geom_path(data = predict20$B1, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$B2, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$B6, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$B8, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$C1, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$C7, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$D1, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$D3, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$D4, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$D5, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$D7, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$E2, aes(time, PC1, color = "20 C")) + 
  geom_path(data = predict20$E6, aes(time, PC1, color = "20 C")) +
  
  geom_path(data = predict25$A7, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$B2, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$B5, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$B6, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$B8, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$D2, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$D6, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$D8, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$E2, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$E7, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$E8, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$F1, aes(time, PC1, color = "25 C")) + 
  geom_path(data = predict25$F2, aes(time, PC1, color = "25 C")) +
  geom_path(data = predict25$F7, aes(time, PC1, color = "25 C")) +
  
  geom_path(data = predict30$A6, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$B1, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$C8, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$D2, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$D6, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$D8, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$E1, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$F2, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$F4, aes(time, PC1, color = "30 C")) + 
  geom_path(data = predict30$F6, aes(time, PC1, color = "30 C")) + 
  
  labs(x = "Time (hrs)", y = paste("PC1", paste0(expl.var[1], "%")))


ggplot() +
  geom_path(data = predict20$B1, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$B2, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$B6, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$B8, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$C1, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$C7, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$D1, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$D3, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$D4, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$D5, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$D7, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$E2, aes(time, PC2, color = "20 C")) + 
  geom_path(data = predict20$E6, aes(time, PC2, color = "20 C")) +
  
  geom_path(data = predict25$A7, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$B2, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$B5, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$B6, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$B8, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$D2, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$D6, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$D8, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$E2, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$E7, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$E8, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$F1, aes(time, PC2, color = "25 C")) + 
  geom_path(data = predict25$F2, aes(time, PC2, color = "25 C")) +
  geom_path(data = predict25$F7, aes(time, PC2, color = "25 C")) +
  
  geom_path(data = predict30$A6, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$B1, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$C8, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$D2, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$D6, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$D8, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$E1, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$F2, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$F4, aes(time, PC2, color = "30 C")) + 
  geom_path(data = predict30$F6, aes(time, PC2, color = "30 C")) + 
  
  labs(x = "Time (hrs)", y = paste("PC2", paste0(expl.var[1], "%")))


ggplot() +
  geom_path(data = predict20$B1, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$B2, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$B6, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$B8, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$C1, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$C7, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$D1, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$D3, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$D4, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$D5, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$D7, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$E2, aes(time, PC3, color = "20 C")) + 
  geom_path(data = predict20$E6, aes(time, PC3, color = "20 C")) +
  
  geom_path(data = predict25$A7, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$B2, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$B5, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$B6, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$B8, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$D2, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$D6, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$D8, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$E2, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$E7, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$E8, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$F1, aes(time, PC3, color = "25 C")) + 
  geom_path(data = predict25$F2, aes(time, PC3, color = "25 C")) +
  geom_path(data = predict25$F7, aes(time, PC3, color = "25 C")) +
  
  geom_path(data = predict30$A6, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$B1, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$C8, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$D2, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$D6, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$D8, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$E1, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$F2, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$F4, aes(time, PC3, color = "30 C")) + 
  geom_path(data = predict30$F6, aes(time, PC3, color = "30 C")) + 
  
  labs(x = "Time (hrs)", y = paste("PC3", paste0(expl.var[1], "%")))


# Regression lines - not worth it

ggplot() +
  geom_smooth(data = predict20$B1, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$B2, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$B6, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$B8, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$C1, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$C7, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$D1, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$D3, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$D4, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$D5, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$D7, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$E2, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict20$E6, aes(PC1, PC2, color = "20"), method = "lm", se = FALSE) +
  
  geom_smooth(data = predict25$A7, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$B2, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$B5, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$B6, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$B8, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$D2, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$D6, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$D8, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$E2, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$E7, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$E8, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$F1, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict25$F2, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) +
  geom_smooth(data = predict25$F7, aes(PC1, PC2, color = "25"), method = "lm", se = FALSE) +
  
  geom_smooth(data = predict30$A6, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$B1, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$C8, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$D2, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$D6, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$D8, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$E1, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$F2, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$F4, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) + 
  geom_smooth(data = predict30$F6, aes(PC1, PC2, color = "30"), method = "lm", se = FALSE) +
  
  geom_point(data = allPC, aes(PC1, PC2, colour = group)) + 
  labs(x = paste("PC1", paste0(expl.var[1], "%")), y = paste("PC2", paste0(expl.var[2], "%"))) + 
  guides(colour = guide_legend(title = "Group:"))

# points
ggplot() +
  geom_point(data = predict20$B1, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$B2, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$B6, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$B8, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$C1, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$C7, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$D1, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$D3, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$D4, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$D5, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$D7, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$E2, aes(PC1, PC2, color = "20") ) + 
  geom_point(data = predict20$E6, aes(PC1, PC2, color = "20") ) +
  
  geom_point(data = predict25$A7, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$B2, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$B5, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$B6, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$B8, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$D2, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$D6, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$D8, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$E2, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$E7, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$E8, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$F1, aes(PC1, PC2, color = "25") ) + 
  geom_point(data = predict25$F2, aes(PC1, PC2, color = "25") ) +
  geom_point(data = predict25$F7, aes(PC1, PC2, color = "25") ) +
  
  geom_point(data = predict30$A6, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$B1, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$C8, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$D2, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$D6, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$D8, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$E1, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$F2, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$F4, aes(PC1, PC2, color = "30") ) + 
  geom_point(data = predict30$F6, aes(PC1, PC2, color = "30") ) +
  
  #geom_point(data = allPC, aes(PC1, PC2, colour = group)) + 
  labs(x = paste("PC1", paste0(expl.var[1], "%")), y = paste("PC2", paste0(expl.var[2], "%"))) + 
  guides(colour = guide_legend(title = "Group:"))



