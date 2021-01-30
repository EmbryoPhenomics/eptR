#!/usr/bin/env Rscript

library(Rtsne)

dat20 <- read.csv('/run/media/z/Z/all20.csv')
dat25 <- read.csv('/run/media/z/Z/all25.csv')
dat30 <- read.csv('/run/media/z/Z/all30.csv')

dat20 <- na.omit(dat20)
dat25 <- na.omit(dat25)
dat30 <- na.omit(dat30)

all <- data.frame(NA)
all[1:10083, 1:303] <- dat20[1:10083, 2:304]
all[10084:16920, 1:303] <- dat25[1:6837, 2:304]
all[16921:21886, 1:303] <- dat30[1:4966, 2:304]
colnames(all)[1] <- 'group'
all$group <- factor(all$group)

print('Finished aggregating data...')

pca <- stats::prcomp(all[,3:303])
dat <- data.frame(group = all$group, pca$x[,1:50])
dat <- data.frame(dat[sample(nrow(dat), 15000), ])

print('Finished computing PCA...')
print('Beginning tSNE...')

tSNE <- Rtsne(dat[,2:51], 
              pca = FALSE, 
              theta = 0.0, 
              dims = 3, 
              verbose = TRUE, 
              num_threads = 3, 
              max_iter = 500)

tSNE_Transformed <- data.frame(tSNE$Y, group = dat$group)
colnames(tSNE_Transformed) <- c("Dim1", "Dim2", "Dim3", "group")

write.csv(tSNE_Transformed, file = '/run/media/z/Z/tSNE_15000.csv')

library(plotly)
plot_ly(data = tSNE_Transformed, x = tSNE_Transformed$Dim1, 
        y = tSNE_Transformed$Dim2, z = tSNE_Transformed$Dim3,
        color = tSNE_Transformed$group) %>%
  add_markers(size = 1) %>%
  layout(paper_bgcolor='black')

library(plot3D)


# Resolve across timepoints, for individuals and create interactive 3d plot for interrogating the time component
# involves creating data for frequency at each timepoint






































