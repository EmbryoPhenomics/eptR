
# --------------------- Data visualisation methods --------------------------- #
# ---------------------------------------------------------------------------- #
# This file contains various methods for visualising the data produced   
# from EmbryoCV. Documentation for individual methods can be found in the 
# data_vis.md file found in this repository.
# ---------------------------------------------------------------------------- #

# Save a ggplot object by resolution

plot.save <- function(plot, width, height, filename, path) {
  
  #' Save a ggplot object by resolution
  #' 
  #' @description This wrapper function allows you to specify the resolution of a plot to be saved, rather than
  #' dimensions in standard units (e.g. cm). 
  #' 
  #' @param plot The plot object to be saved.
  #' @param width Width in pixels. 
  #' @param height Height in pixels.
  #' @param filename Name of the file to create on disk.
  #' @param path Path to save file to. 
  #' 
  #' @usage plot.save(plot, width, height, filename, path)
  #' 
  #' @details Since this is a wrapper function, you can pass any grob object you normally would if you were using ggsave(). 
  #' 
  #' @examples
  #' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  #' 
  #' plot.save(plot = p, width = 800, height = 600, 
  #'           filename = "mtcars.png", path = ".")
  
  dpi   <- 100
  resw <- width/dpi
  resh <- height/dpi
  
  ggplot2::ggsave(filename = filename, dpi = dpi, width = resw, height = resh, units = 'in', plot = plot, path = path)
}

docstring::docstring(plot.save)

# ---------------------------------------------------------------------------- #

# Plot the raw frequency/power values for a given embryo at a specified 
  # hour. Can optionally return a grob object for further modification.

plotRaw_FreqOutput_at_hr <- function(array, hr, axisLabels = FALSE, 
                                     return = "plot", type = "geom_path()") {
  
  if (requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("gridExtra", quietly = TRUE)) {
    
    #============================#
    # --- For signals of 1x1 ----#
    #============================#
    
    if (length(dim(array)) == 3) {
      
      atHR <- array[1:301, 1:2, hr]
      df   <- data.frame(Frequency = atHR[,1], Power = atHR[,2])
      
      if (return == "plot") {
        
        if (isFALSE(axisLabels)) {
          
          ggplot(df, aes(x = Frequency, y = Power)) + 
            eval(parse(text = type)) +
            theme_void()
          
        } else if (isTRUE(axisLabels)) {
          
          ggplot(df, aes(x = Frequency, y = Power)) + 
            eval(parse(text = type)) 
          
        } else {
          
          stop("axisLabels is a logical argument, can only pass TRUE or FALSE.")
          
        }
        
      } else if (return == "grob") {
        
        ggplot(df, aes(x = Frequency, y = Power))
        
      }
      
      #============================#
      # --- For signals of >1x1 ---#
      #============================# 
      
    } else if (length(dim(array)) == 5) {
      
      rows <- dim(array)[3]
      cols <- dim(array)[4]
      
      atHR <- array[1:301, 1:2, 1:rows, 1:cols, hr]
      
      # Split into individual dataframes for blockwise plotting
      dfs <- list()
      
      for (row in 1:rows) {
        for (col in 1:cols) {
          dfname <- paste0("df",row,"x",col)
          dfs[[dfname]] <- data.frame(Frequency = atHR[,,row, col][,1], Power = atHR[,,row,col][,2])
        }
      }
      
      # Compute max power for ylim
      maxPowers <- c()
      for (df in dfs) maxPowers <- append(maxPowers, max(df$Power))
      maxPower <- max(maxPowers)
      
      if (return == "plot") {
        
        if (isFALSE(axisLabels)) {
          
          # Produce individual grob objects for dataframes (ready for plotting)
          plots <- list()
          
          for (i in 1:length(dfs)) {
            plotname <- paste0("p", i)
            plots[[plotname]] <- 
              ggplot(dfs[[i]], aes(Frequency, Power)) + 
              eval(parse(text = type)) +
              ylim(0, maxPower) +
              theme_void()
          }
          
          gridExtra::grid.arrange(grobs = plots, nrow = rows, ncol = cols)
          
        } else if (isTRUE(axisLabels)) {
          
          plots <- list()
          
          for (i in 1:length(dfs)) {
            plotname <- paste0("p", i)
            plots[[plotname]] <- 
              ggplot(dfs[[i]], aes(Frequency, Power)) + 
              eval(parse(text = type)) +
              ylim(0, maxPower) 
          }
          
          gridExtra::grid.arrange(grobs = plots, nrow = rows, ncol = cols)
          
        } else {
          
          stop("axisLabels is a logical argument, can only pass TRUE or FALSE.")
          
        }
        
      } else if (return == "grob") {
        
        # Produce individual grob objects for dataframes (for use with further ggplot modifications)
        grobs <- list()
        
        for (i in 1:length(dfs)) {
          grobname <- paste0("p", i)
          grobs[[grobname]] <- 
            ggplot(dfs[[i]], aes(Frequency, Power)) + 
            ylim(0, maxPower)
        }
        
        return(grobs)
        
      }
      
    }
    
  } else {
    
    stop("Packages ggplot2 and gridExtra are required for this function.")
    
  }
  
}

# ---------------------------------------------------------------------------- #

# Plot time vs power, for each bin in descending order from a binned 
  # FreqOutput_1x1 array for a given individual. 

plot_binned_FreqOutput <- function(data, axisLabels = FALSE, type = "geom_path()", 
                                   return = "plot", scaleToMax = FALSE) {
  
  maxPower <- max(data, na.rm = TRUE)
  minPower <- min(data, na.rm = TRUE)
  
  if (requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("gridExtra", quietly = TRUE)) {
    
    require(ggplot2)
    
    dfs <- list()
    
    for (i in 2:length(names(data))) {
      name <- paste0("bin", i - 1)
      dfs[[name]] <- data.frame(x = data$cols, y = data[[name]])
    }
    
    if (axisLabels == FALSE) {
      
      plots <- list()
      
      for (i in 1:length(dfs)) {
        plotname <- paste0("p", i)
        plots[[plotname]] <- 
          ggplot(dfs[[i]], aes(x = x, y = y)) + 
          eval(parse(text = type)) +
          theme_void() +
          if (isTRUE(scaleToMax)) ylim(minPower, maxPower)
        
      }
      
      gridExtra::grid.arrange(grobs = plots, nrow = length(names(data)) - 1, ncol = 1)
      
    } else if (axisLabels == TRUE) {
      
      plots <- list()
      
      for (i in 1:length(dfs)) {
        plotname <- paste0("p", i)
        plots[[plotname]] <- 
          ggplot(dfs[[i]], aes(x = x, y = y)) + 
          eval(parse(text = type)) +
          labs(x = "Time (Hr)", y = "Power") +
          if (isTRUE(scaleToMax)) ylim(minPower, maxPower)
        
      }
      
      gridExtra::grid.arrange(grobs = plots, nrow = length(names(data)) - 1, ncol = 1)
      
    } else {
      
      stop("axisLabels is a logical argument, can only pass TRUE or FALSE.")
      
    }
    
  } else {
    
    stop("Packages ggplot2 and gridExtra required for this function.")
    
  }
  
}

# ---------------------------------------------------------------------------- #

# Produce animations of the various PCA results for various binning regimes
  # Function is executed in parallel  

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

# ---------------------------------------------------------------------------- #

# Plot a 2D representation of a blockwise signal at a given hr and frame. 
  # Note this method is only for a given Embryo's blockwise signals

plotBlockWise_2D <- function(array, frame, hr) {
  
  dataRaw      <- array[1:dim(array)[1], 1:dim(array)[2], frame, hr]
  dataNaN_omit <- apply(dataRaw, MARGIN = 2, FUN = function(x) ifelse(x %in% NaN, 0, x))
  
  data_df <- matrixToDataframe(dataNaN_omit) 
  data_df$z <- as.numeric(data_df$z)
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    
    ggplot(data_df, aes(x, y, fill = z)) + 
      geom_raster() +
      theme_void() + 
      labs(title = paste("Hour:", hr), 
           subtitle = paste("Frame:", frame)) +
      scale_fill_viridis_c() + 
      guides(fill = guide_legend(title = "Pixel intensity"))
    
  } else {
    
    stop("Package ggplot2 is required for this method.")
    
  }  
  
  
}

# ---------------------------------------------------------------------------- #

# Plot a 3D representation of a blockwise signal at a given hr and frame. 
  # Note this method is only for a given Embryo's blockwise signals

plotBlockWise_3D <- function(array, frame, hr) {
  
  dataRaw      <- array[1:dim(array)[1], 1:dim(array)[2], frame, hr]
  dataNaN_omit <- apply(dataRaw, MARGIN = 2, FUN = function(x) ifelse(x %in% NaN, 0, x))
  
  if (requireNamespace("plot3D", quietly = TRUE)) {
    
    persp3D(z = dataNaN_omit,
            colkey = FALSE,
            expand = 0.5, 
            bty = "g", 
            phi = 30, 
            shade = 0.2, 
            ltheta = 120,
            space = 0.3, 
            ticktype = "detailed", 
            d = 2,
            main = paste("Hour:", hr)
                    
    )
    
  } else {
    
    stop("Package plot3D is required for this method.")
    
  }

}

# ---------------------------------------------------------------------------- #

