# PCA on population means for all groups, at a bin of 10

group20 <- mean_binnedFreqOutput("/run/media/z/Z/embryoCV_data/20_deg/", bins = 10)
group25 <- mean_binnedFreqOutput("/run/media/z/Z/embryoCV_data/25_deg/", bins = 10)
group30 <- mean_binnedFreqOutput("/run/media/z/Z/embryoCV_data/30_deg/", bins = 10)

# Rescale to 0 - 1
group20 <- rescale(group20)
group25 <- rescale(group25)
group30 <- rescale(group30)

# Test visualisation of groups between bins 
library(ggplot2)
ggplot() +
  geom_point(data = group20, aes(bin1, bin2, col = "20 C")) +
  geom_point(data = group25, aes(bin1, bin2, col = "25 C")) + 
  geom_point(data = group30, aes(bin1, bin2, col = "30 C"))

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
             legend.title = "Groups"
)
