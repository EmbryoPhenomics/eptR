## data_analysis

This script describes several methods for analyzing the data obtained from `EmbryoCV`. Note that all code for SVM's can now be found in the trialMethods folder. Though the code is developmental, the methods are improved and allow for parallel execution. 

#### `maov()`

A more flexible Multi-ANOVA method for large numbers of response variables. Note this is not a MANOVA, an ANOVA is computed on each response variable, and the results are returned as a list.

##### Arguments

* responseVars: Numeric vector of column indices of response variables.
* predictVar: Character string. Predictor variable.
* data: DataFrame. Dataset to conduct the method on. 

##### Example usage

``` R
source('path/to/data_analysis.R')

maov(responseVars = 1:12, predictVar = "group", data = data)
 
```

#### `doPCAforBins()`

Execute PCA for several binning regimes produced through binning methods in `data_wrangling.R`. Can optionally return plots of the results to a path specified in `outpath`. Note that this method is only implemented for the 3 temperature groups in the main experiment and so therefore may need some adjustment for other experiment types. 

##### Arguments

* path_20: Character string. File path to csv files of data from the 20 degrees group, binned to various levels. 
* path_25: Character string. Same as above except for 25 degrees. 
* path_30: Character string.Same as above except for 30 degrees.
* bins: Numeric vector. Binning regimes used i.e. c(10,20,50,100,150). 
* plot: Boolean. Default is FALSE. Whether to plot the results. 
* outpath: Character string. Path to save plots to. 

##### Example usage

``` R
source('path/to/data_analysis.R')

doPCAforBins(path20 = "path/to/20_deg/",
             path25 = "path/to/20_deg/",
             path30 = "path/to/20_deg/",
             bins = c(10, 20, 50, 100, 150),
             plot = TRUE, 
             outpath = "path/to/save/plots/")
```

#### `makeTrainTest_files()`

Randomly sample a list of files and produce training and testing groups. File lists are returned for training and testing groups.

##### Arguments

* files: Character vector. List of files to make train/test groups for. 
* train: Numeric value. Default is 0.7 (70% of data). Fraction of files to assign as train data, represented as decimal. 

##### Example usage

``` R

source('path/to/data_analysis.R')

# Make a train/test group from a list of HDF5 files in the current directory
  # Ratio is 70/30 respectively
traintest <- makeTrainTest_files(files = list.files(path = '.', ".HDF5"), train = 0.7)

# Print the train group
print(traintest$train)

# Print the test group
print(traintest$test)

```



