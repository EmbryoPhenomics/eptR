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

# Compute mean freqoutput for all embyros
mean20 <- mean_binnedFreqOutput(path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
                                files = files20,
                                bins = 10)
mean25 <- mean_binnedFreqOutput(path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
                                files = files25,
                                bins = 10)
mean30 <- mean_binnedFreqOutput(path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
                                files = files30,
                                bins = 10)

group20 <- rescale(group20)
group25 <- rescale(group25)
group30 <- rescale(group30)

library(FactoMineR)
library(factoextra)

# Construct final dataframe for PCA
all <- data.frame(group = rep(c(20,25,30), each = 367))
all[1:367, 2:11] <- test[1:367,2:11]
all[368:731, 2:11] <- test1[1:364,2:11]
all[732:1095, 2:11] <- test2[1:364,2:11]
all$group <- factor(all$group)
all <- na.omit(all)

t <- PCA(all[2:11], graph = FALSE)

fviz_pca_ind(t,
             geom.ind = "point", 
             col.ind = all$group, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Groups")

