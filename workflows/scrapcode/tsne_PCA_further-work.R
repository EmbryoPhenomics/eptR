library(ggplot2)

meanAll20 <- read.csv("/run/media/z/Z/means/all/20_deg/150.csv", header = TRUE)
meanAll25 <- read.csv("/run/media/z/Z/means/all/25_deg/150.csv", header = TRUE)
meanAll30 <- read.csv("/run/media/z/Z/means/all/30_deg/150.csv", header = TRUE)

meanAll <- data.frame(group = rep(c(20,25,30), each = 367))
meanAll$group <- factor(meanAll$group)

meanAll[1:367, 2:ncol(meanAll20)] <- meanAll20[,2:ncol(meanAll20)]
meanAll[368:731, 2:ncol(meanAll25)] <- meanAll25[,2:ncol(meanAll25)]
meanAll[732:1095, 2:ncol(meanAll30)] <- meanAll30[,2:ncol(meanAll30)]
meanAll <- na.omit(meanAll)

# meanAll <- meanAll[meanAll$bin3 < 0.0015,]

pcaAll <- doPCAforBins(path_20 = "/run/media/z/Z/means/all/20_deg/",
                       path_25 = "/run/media/z/Z/means/all/25_deg/",
                       path_30 = "/run/media/z/Z/means/all/30_deg/",
                       bins = c(10, 20, 50, 100, 150))

pca150 <- pcaAll$bin150

pca150$expl.var # First two components explain only 63% of the data

pcaAll$bin10$expl.var # First two components explain 84% of the data

# Correlation stats

corPC1 <- sapply(3:152, function(x) cor(x = pca150$x$PC1, y = meanAll[,x]))
corPC2 <- sapply(3:152, function(x) cor(x = pca150$x$PC2, y = meanAll[,x]))

corStats <- data.frame(PC1 = corPC1, PC2 = corPC2, bin = 1:150)

ggplot(data = corStats) +
  geom_col(aes(bin, PC1))

ggplot(data = corStats) +
  geom_col(aes(bin, PC2))

# tSNE tests 

library(Rtsne)

meanAll20_rescaled <- rescale(meanAll20, columns = 3:ncol(meanAll20))
meanAll25_rescaled <- rescale(meanAll25, columns = 3:ncol(meanAll25))
meanAll30_rescaled <- rescale(meanAll30, columns = 3:ncol(meanAll30))

meanAll_rescaled <- data.frame(group = NA)
meanAll_rescaled[1:367,1] <- 20
meanAll_rescaled[368:730,1] <- 25
meanAll_rescaled[731:1094,1] <- 30
meanAll_rescaled$group <- factor(meanAll_rescaled$group)

meanAll_rescaled[1:367, 2:ncol(meanAll20_rescaled)] <- meanAll20_rescaled[,2:ncol(meanAll20_rescaled)]
meanAll_rescaled[368:731, 2:ncol(meanAll25_rescaled)] <- meanAll25_rescaled[,2:ncol(meanAll25_rescaled)]
meanAll_rescaled[732:1095, 2:ncol(meanAll30_rescaled)] <- meanAll30_rescaled[,2:ncol(meanAll30_rescaled)]

meanAll_rescaled <- na.omit(meanAll_rescaled)

tSNE <- lapply(seq(5, 55, 5), function(x) {
  Rtsne(meanAll_rescaled[,3:152], 
        pca = TRUE,
        pca_center = TRUE,
        theta = 0.0, 
        dims = 2, 
        verbose = TRUE, 
        num_threads = 3, 
        max_iter = 1000,
        perplexity = x)
})
                             
tSNE_Transformed <- data.frame(tSNE[[11]]$Y, group = meanAll_rescaled$group)
colnames(tSNE_Transformed) <- c("Dim1", "Dim2", "group")

ggplot(tSNE_Transformed, aes(Dim1, Dim2, color = group)) + geom_point()

# Raw vis 

ggplot(meanAll20, aes(bin1, bin2)) + geom_point()

ggplot() + 
  geom_point(data = meanAll20_rescaled, aes(bin110, bin113, color = "20 C")) +
  geom_point(data = meanAll25_rescaled, aes(bin110, bin113, color = "25 C")) +
  geom_point(data = meanAll30_rescaled, aes(bin110, bin113, color = "30 C"))






