
# Animating frequency vs power over time, except power values are rescaled for individual frequencies to range 0 - 1

meanAll20 <- read.csv("/run/media/z/Z/means/all/20_deg/150.csv", header = TRUE)
meanAll25 <- read.csv("/run/media/z/Z/means/all/25_deg/150.csv", header = TRUE)
meanAll30 <- read.csv("/run/media/z/Z/means/all/30_deg/150.csv", header = TRUE)

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

t20 <- t(meanAll20_rescaled[,3:152])
t25 <- t(meanAll25_rescaled[,3:152])
t30 <- t(meanAll30_rescaled[,3:152])

animateFreq <- function(time, bins, outpath) {
  
  for (i in 1:length(time)) {
    
    group20 <- data.frame(x = t20[,time[i]], y = 1:bins)
    group25 <- data.frame(x = t25[,time[i]], y = 1:bins)
    group30 <- data.frame(x = t30[,time[i]], y = 1:bins)
    
    p <- ggplot() + 
      geom_point(data = group20, aes(x, y, color = "20 C")) +
      geom_point(data = group25, aes(x, y, color = "25 C")) + 
      geom_point(data = group30, aes(x, y, color = "30 C")) +
      theme_classic() + 
      labs(title = "Rescaled power values for various frequency bins, animated over time",
           subtitle = paste0("Bin number: ", bins, ", Species: Radix Balthica, Hour: ", time[i]),
           x = "Power",
           y = "Frequency bin") + 
      guides(color = guide_legend(title = "Group:"))
    
    plot.save(plot = p, 
              width = 650, 
              height = 550, 
              filename = paste0(time[i], ".png"),
              path = outpath)
    
  }
  
}

animateFreq(time = 1:364, bins = 150, 
            outpath = "/home/z/Desktop/github /misc_embryoPhenomics/R/data_vis/visualisations/FreqOutput_raw/animated/")



