
# Produce temporal animations of the various PCA results for various binning regimes

animatePCA <- function(pcaGroup, bins, outpath) {
  
  require(foreach)
  require(parallel)
  require(doParallel)
  
  ncores <- detectCores()
  cl     <- makeCluster(ncores - 1)
  registerDoParallel(cl)
  
  foreach (i = 1:length(pcaGroup)) %dopar% {
    
    require(ggplot2)
    require(gganimate)
    
    expl.var <- round(pcaGroup[[i]]$sdev^2/sum(pcaGroup[[i]]$sdev^2)*100)
    
    anim <- ggplot(pcaGroup[[i]]$x, aes(PC1, PC2, color = group)) +
      geom_path() +
      labs(title = "Mean rescaled power values for various temperature groups",
           subtitle = paste0("PCA transformed, Bins = ", bins[i], ", Species = Radix Balthica"),
           x = paste("PC1", paste0(expl.var[1], "%")), 
           y = paste("PC2", paste0(expl.var[2], "%"))) + 
      guides(colour = guide_legend(title = "Group:")) +
      transition_reveal(along = pcaGroup[[i]]$x$time)
    
    anim_save(filename = paste0(outpath, "bin", bins[i], ".gif"),
              animation = anim)
    
  }
  
  stopCluster(cl)
  
}
