## data_vis

This script contains various methods for visualising the data produced from EmbryoCV. Not all methods are suited to generic applications and may only work with specific data formats. Further, it is important to note that many of these methods are built to complement methods developed for earlier on in the processing pipeline. This may or may not mean that a specific processing pipeline is required to use some of the methods below. 

#### `plot.save()`

Wrapper to `ggsave()` for saving ggplots by resolution, rather than in standard units (e.g. cm). Since this is a wrapper function, you can pass any grob object you normally would if you were using `ggsave()`. 

##### Arguments

* plot: The plot object to be save. 
* width: Numeric. Width in pixels. 
* height: Numeric. Height in pixels. 
* filename: Character string. Name of the file to create on disk. 
* path: Character string. Path to save file to. 

##### Example usage.

``` R
source('path/to/data_vis.R')

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()

plot.save(plot = p, width = 800, height = 600, filename = "mtcars.png", path = ".")

```

#### `plotRaw_FreqOutput_at_hr()`

Plot the raw frequency/power values for a given embryo at a specified hour. Can optionally return the grob objects for further modification. The axis for the plots returned are x = Frequency and y = Power. 

##### Arguments

* array: Array. Frequency/power subdataset for a specific embryo. 
* hr: Numeric. Hour to plot raw data for. 
* axisLabels: Boolean. Default is FALSE. Whether to include axis labels and gridlines. 
* return: Character string. Default is "plot". Whether to plot the data or return the grob objects. 
* type: Character string. Default is "geom_path()". Plot type to display. Since this character string becomes an evaluated expression, you can pass any additional arguments you normally would e.g. additional aesthetic arguments. Note however that you cannot pass additional ggplot modifications outside the main function call itself e.g. "geom_path() + theme_classic()". 

##### Example usage

``` R

source('path/to/data_vis.R')
source('path/to/data_handling.R')

# Retrieve the 8x8 frequency/power subdataset for embryo A2
freqOutput_8x8 <- getXArrayData(file = "A2Dataset.HDF5", subdataset = "FreqOutput_8x8")

# Plot the raw values for hr = 100
plotRaw_FreqOutput_at_hr(array = freqOutput_8x8, hr = 100)



```

#### `plot_binned_FreqOutput()`

Plot a binned 1x1 frequency/power array for a given embryo. Axis of the plots returned are as follows: x = time, in units chosen for the experiment, and y = power for a given frequency bin. Can optionally return the grob objects for further modification. 

##### Arguments

* data: Dataframe. Binned 1x1 frequency/power array. 
* axisLabels: Boolean. Default is FALSE. Whether to include axis labels and gridlines.
* type: Character string. Default is "geom_path()". Plot type to display. Since this character string becomes an evaluated expression, you can pass any additional arguments you normally would e.g. additional aesthetic arguments. Note however that you cannot pass additional ggplot modifications outside the main function call itself e.g. "geom_path() + theme_classic()". 
* return: Character string. Default is "plot". Whether to plot the data or return the grob objects. 
* scaleToMax: Boolean. Default is FALSE. Whether to set y axis limits according to the min/max power values for all frequency bins. 

##### Example usage

``` R

source('path/to/data_vis.R')
source('path/to/data_handling.R')
source('path/to/data_wrangling.R')
# Import the Freqeuncy/Power 1x1 array for embryo A2
freqOutput_1x1 <- getXArrayData(file = "A2Dataset.HDF5", subdataset = "FreqOutput_1x1")

# Bin the frequencies to a bin number of 10 using a mean bin method
binnedData <- bin_FreqOutput1x1(array = freqOutput_1x1, binMethod = "mean", bins = 10, hatchTime = NULL)

# Plot the output of the above bining regime
plot_binned_FreqOutput(data = binnedData)

```

#### `animatePCA()`

Parallel function developed specifically for visualising various PCA results. Used in combination with specific binning workflows. 

##### Arguments

* pcaGroup: List. List of PCA results for various binning regimes. 
* bins: Numeric vector. Bins used in previous methods, each corresponding to a PCA result in pcaGroup. 
* outpath: Character string. Path to save animations to. 


#### `plotBlockWise_2D()`

Plot a 2D representation of a blockwise signal at a given hour and frame. 

##### Arguments

* array: Array. Blockwise subdataset for a given embryo. 
* frame: Numeric. Frame to plot.
* hr: Numeric. Hour to retrieve the specified frame's data from. 

#### `plotBlockWise_3D()`

Plot a 3D representation of a blockwisesignal at a given hour and frame. 

##### Arguments

* array: Array. Blockwise subdataset for a given embryo. 
* frame: Numeric. Frame to plot.
* hr: Numeric. Hour to retrieve the specified frame's data from. 
