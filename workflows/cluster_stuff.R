library(cluster)
test <- cluster::agnes(x = pcaAll$bin10$x[2:10])
plot(test)

d <- dist(pcaAll$bin10$x[,3:12], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit)


# model based clustering
library(mclust)
fit <- Mclust(pcaAll$bin10$x[,3:4])
plot(fit) 
summary(fit)

# Kmeans clustering (mostly validates PCA)
fit <- kmeans(pcaAll$bin10$x[,3:4], 
              centers = 3L,
              nstart = 25L,
              iter.max = 10L)

test <- data.frame(pcaAll$bin10$x, cluster = factor(fit$cluster))

p1 <- ggplot(test, aes(PC1, PC2, colour = cluster)) + 
  geom_point() +
  labs(title = "Kmeans clusters from PCA data") +
  scale_color_manual(values = c('1'='#377eb8', '2'='#e41a1c', '3'='#4daf4a'))

p2 <- ggplot(pcaAll$bin10$x, aes(PC1, PC2, colour = group)) +
  geom_point() +
  labs(title = "Initial PCA clusters") +
  scale_color_manual(values = c('30'='#377eb8', '20'='#e41a1c', '25'='#4daf4a'))

gridExtra::grid.arrange(p1, p2, ncol = 2, nrow = 1)

# SVM clustering
library(e1071)
dat <- pcaAll$bin10$x[,c(1,3,4)]
svmfit <- svm(group~., data = dat, kernel = "radial", cost = 10, scale = FALSE)

grid <- expand.grid(seq(min(dat[, 2]), max(dat[, 2]),length.out=100),                                                                                                         
                    seq(min(dat[, 3]), max(dat[, 3]),length.out=100)) 
names(grid) <- names(dat)[2:3]
preds <- predict(svmfit, grid)
df <- data.frame(grid, preds)

ggplot() + 
  geom_tile(data = df, aes(x = PC1, y = PC2, fill = preds)) +
  geom_point(data = dat, aes(PC1, PC2, color = group)) +
  scale_color_manual(values = c('30'='blue', '20'='red', '25'='green')) +
  ggtitle("Radial SVM for PCA transformed data", subtitle = "Bin = 10, Species = Radix Balthica") +
  theme_bw() +
  guides(fill = guide_legend(title = "Hyperplanes"),
         color = guide_legend(title = "Group"))


# hierarchial clustering
fit <- dat %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

library(factoextra)
fviz_dend(fit, k = 3, 
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, 
          rect = TRUE 
)

