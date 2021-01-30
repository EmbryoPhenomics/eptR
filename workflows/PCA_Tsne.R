library(rgl)
library(Rtsne)
library(plotly)
library(ggplot2)
#source('~/Desktop/github /misc_embryoPhenomics/R/data_wrangling/rescale.R')
source("~/Desktop/github /misc_embryoPhenomics/R/data_analysis/doPCAforBins.R")

# 3D plotly graph from 3 bins, without further transformations

pcaAll <- doPCAforBins(path_20 = "/run/media/z/Z/means/all/20_deg/",
                       path_25 = "/run/media/z/Z/means/all/25_deg/",
                       path_30 = "/run/media/z/Z/means/all/30_deg/",
                       bins = c(10, 20, 50, 100, 150))

testdata <- pcaAll$bin150$x[,1:52]

# For animation
for (i in seq(1, 1000, 50)) {
  
  tSNE <- Rtsne(testdata[,3:52], 
                pca = FALSE, 
                theta = 0.0, 
                dims = 2, 
                verbose = TRUE, 
                num_threads = 3, 
                max_iter = i)
  
  tSNETransformed <- data.frame(tSNE$Y, group = testdata$group)
  colnames(tSNETransformed) <- c("Dim1", "Dim2", "group")
  
  p <- ggplot(tSNETransformed, aes(Dim1, Dim2, color = group)) + 
    geom_point() + 
    theme_classic() + 
    labs(title = "PCA - tSNE transform for population means",
         subtitle = paste0("Bin number: ", 150, ", Species: Radix Balthica, Iteration: ", i),
         x = "Dim 2",
         y = "Dim 1") + 
    guides(color = guide_legend(title = "Group:"))
  
  
  plot.save(plot = p, 
            width = 650, 
            height = 550, 
            filename = paste0(i, ".png"),
            path = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/Cluster/tSNE/animated/")
  
}



# plotly
plot_ly(data = tsneTransformed, x = ~Dim1, y = ~Dim2, z = ~Dim3, color = ~group) %>%
  add_markers(size = 2)

# rgl
rgl::plot3d(x = tsneTransformed$`Dim 1`, y = tsneTransformed$`Dim 2`, z = tsneTransformed$`Dim 3`, 
            xlab = "Dim 1", ylab = "Dim 2", zlab = "Dim 3", type = "p", col = tsneTransformed$group,
            box = FALSE, axes = TRUE)

#size = 2L, main = "PCA - TSNE transformed data", sub = "Bins = 10, Species = Radix Balthica"

