
library(ggplot2)
source("~/Desktop/github/misc_embryoPhenomics/R/data_vis.R")
source("~/Desktop/github/misc_embryoPhenomics/R/data_analysis.R")
source('~/Desktop/github/misc_embryoPhenomics/R/data_wrangling.R')

pcaAll <- doPCAforBins(path_20 = "/run/media/z/Z/means/all/20_deg/",
                       path_25 = "/run/media/z/Z/means/all/25_deg/",
                       path_30 = "/run/media/z/Z/means/all/30_deg/",
                       bins = c(10, 20, 50, 100, 150))

model150 <- pcaAll$bin150

files20 <- list.files('/run/media/z/Z/embryoCV_data/data_binned/binned_1x1_output/20_deg/bin_150', full.names = TRUE)
files25 <- list.files('/run/media/z/Z/embryoCV_data/data_binned/binned_1x1_output/25_deg/bin_150', full.names = TRUE)
files30 <- list.files('/run/media/z/Z/embryoCV_data/data_binned/binned_1x1_output/30_deg/bin_150', full.names = TRUE)

test20 <- readRDS(files20[[1]])
test25 <- readRDS(files25[[1]])
test30 <- readRDS(files30[[1]])

test <- data.frame(group = NA)
test[1:367,1] <- 20
test[368:730,1] <- 25
test[731:1094,1] <- 30
test$group <- factor(test$group)

test[1:367, 2:ncol(test20)] <- test20[,2:ncol(test20)]
test[368:730, 2:ncol(test20)] <- test25[,2:ncol(test25)]
test[731:1094, 2:ncol(test20)] <- test30[,2:ncol(test30)]

Test <- as.matrix(test[,2:151])
colnames(test) <- NULL
colnames(test)[[1]] <- 'col'

predict(model150, newdata = Test)

plot(pcaAll$bin10$x[,2] ~ pcaAll$bin10$x[,1])
plot(pcaAll$bin150$x[,2] ~ pcaAll$bin150$x[,1])


library(cluster)
library(ggfortify)
pca_data <- read.csv("/home/z/Documents/Ziad Ibbini - PCA.csv")
tsne_data <- read.csv("/home/z/Documents/Ziad Ibbini - tSNE.csv")
p <- ggplot(pca_data,aes(PC1, PC2, colour = factor(group, labels = c("20°C","25°C","30°C")))) +
  geom_point(show.legend=FALSE) + labs(colour = "Temperature") + xlab("Principal component 1") + ylab("Principal component 2") 
p1 <- ggplot(tsne_data,aes(X1, X2, colour = factor(group, labels = c("20°C","25°C","30°C")))) +
  geom_point() + labs(colour = "Temperature") + xlab("Dimension 1") + ylab("Dimension 2") + xlim(c(-80,100)) +
  theme(legend.position = c(0.81,0.8))
p1
all_ps <- plot_grid(p,p1, labels = "AUTO")
all_ps

fanModel <- fanny(x = pca_data[, 4:5], k = 3)

autoplot(pam(x = pca_data[, 4:5], k = 3), frame = TRUE)

pca_data <- read.csv("/home/z/Documents/Ziad Ibbini - PCA.csv")
fanModel <- fanny(x = pca_data[, 4:5], k = 3)
pca_data <- pca_data[,c(2,4:5)]
pca_data$group <- factor(pca_data$group)
autoplot(fanModel, data = pca_data, frame = TRUE, frame.type = 'norm', colour = 'group')

model10 <- pcaAll$bin10
pca_data10 <- as.data.frame(model10$x)
pca_data10$group <- pca_data$group

model150 <- pcaAll$bin150
autoplot(model150, data = pca_data, colour = 'group',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3,  frame = TRUE, frame.type = 'norm')

autoplot(model150, data = pca_data, frame = TRUE, frame.type = 'norm', colour = 'group')

pca_data$time <- 1:length(pca_data$group)
pca_data[1:367,4] <- 1:367
pca_data[368:731,4] <- 1:364
pca_data[732:1095,4] <- 1:364

ggplot() + 
  geom_path(data = pca_data, aes(time, PC2, colour = group)) + labs(title = 'PC2')



normalize <- function(data) {
  rescaled <- scale(data,
        center = min(data, na.rm = TRUE),
        scale = max(data, na.rm = TRUE) - min(data, na.rm = TRUE))
  return(rescaled)
}

rotPC1 <- normalize(model150$rotation[,1])
rotPC1 <- data.frame(cor = rotPC1)
rotPC1$freq <- 1:length(rotPC1$cor)
ggplot(rotPC1, aes(freq, cor)) + geom_col() + labs(title = 'PC1')

rotPC2 <- normalize(model150$rotation[,2])
rotPC2 <- data.frame(cor = rotPC2)
rotPC2$freq <- 1:length(rotPC2$cor)
ggplot(rotPC2, aes(freq, cor)) + geom_col() + labs(title = 'PC2')



load.rot <- model150$rotation
names(load.rot[,1][order(abs(load.rot[,1]),decreasing=TRUE)][1:5])
names(load.rot[,2][order(abs(load.rot[,2]),decreasing=TRUE)][1:5])




