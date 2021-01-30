## data_wrangling

This script contains methods for manipulating the data produced from EmbryoCV. It is important to note that not all methods are generic, with some only applicable to the types of data found on the EmbryoPhenomics platform. If a method requires a list of files or file path to be specified, the files must be .HDF5 files. No other file format will be accepted as these methods were written specifically for manipulating the XArrays produced by EmbryoCV. 

#### `binMatrix_mean()`

Divide a matrix into bins and return the mean for each bin. Note that because of the method for iterating over columns, the final output dataframe is transposed. 

##### Arguments

* matrix: Matrix object. 2D matrix/array to bin.
* bins: Numeric. Number of bins reduce the matrix to. 

#### `binMatrix_sum()`

Same as above except computes the sum of each bin of values. 

##### Arguments

* matrix: Matrix object. 2D matrix/array to bin.
* bins: Numeric. Number of bins reduce the matrix to. 

#### `bin_FreqOutput1x1()`

Bin the frequency/power output from `EmbryoCV`, specifically the 1x1 arrays. Note that frequencies are binned in this method i.e. a binning regime of 10 reduces the approx. 300 frequencies down to 10 vectors. 

##### Arguments

* array: Array. 3D array or the subdataset FreqOutput_1x1 to bin.
* binMethod: Character string. Method for binning data, either "mean" or "sum". Default is "mean". 
* bins: Numeric. Number of bins to bin the data. 
* hatchTime: Numeric. Remove all data past the hatch time, units are hrs. Default is NULL. 

##### Example usage

``` R

source('path/to/data_handling.R')
source('path/to/data_wrangling.R')

# Import the Freqeuncy/Power 1x1 array for embryo A2
freqOutput_1x1 <- getXArrayData(file = "A2Dataset.HDF5", subdataset = "FreqOutput_1x1")

# Bin the frequencies to a bin number of 10 using a mean bin method
binnedData <- bin_FreqOutput1x1(array = freqOutput_1x1, binMethod = "mean", bins = 10, hatchTime = NULL)

```

#### `mean_binnedFreqOutput()`

Supply a list of individual embryo XArray files, and compute the mean binned frequency/power output for the 1x1 arrays. 

##### Arguments

* path: Character string. Path to individual embryo datasets.
* files: Character vector. Optional list of individual embryo datasets. 
* bins: Numeric. Number of bins to bin the data. 

##### Example usage

```R

source('path/to/data_wrangling.R')

# Compute the mean binned frequency/power signal for the 20 degree group with a  bin number of 10
mean_binnedFreqOutput(path = 'path/to/20_deg/', bins = 10)

```

#### `retrieveFiles()`

Retrieve all the 1x1 frequency/power arrays at a given path for a specified number of individual embryo datasets. 

##### Arguments

* files: Character vector. List of individual embryo datasets. 
* path: Character string. Path to embryo datasets.
* output: Character string. Default is "raw". Output format for individual datasets, options are either "matrix" or "raw". Matrix format will return the reshaped arrays, such that rows are the temporal scale and columns are the individual frequencies. Raw format will return the arrays as they would be if you obtained them individually without any post-processing of the data. 
* format: Character string. Default is "list". Output for the final aggregated dataset, options are "list" or "dataframe". A list format will return all the individual datasets as a list, with none of the datasets merged. A dataframe format will return all the individual datasets aggregated into one dataframe object. Note that dataframe format will only work if the output format is also "matrix". 
* group: Numeric. Temperature group that the embryos belonged to. 
* rescale: Boolean. Default is TRUE. Should individual datasets be normalised prior to aggregation?

##### Example usage

``` R

source('path/to/data_wrangling.R')

# List files 
files20 <- list.files("/path/to/20degrees/", pattern = ".HDF5")

# Retrieve all the individual data for the 20 degree sub group 
data20 <- retrieveFiles(files = files20, path = "/path/to/20degrees/", output = "matrix", format = "dataframe", group = 20, rescale = TRUE)

```

#### `maxFreq_atTime()`

Compute the frequency with the maximum energy at each time point for a 1x1 frequency/power array. Note that individual frequencies are normalised beforehand to avoid over-representation of low frequencies in the results. 

##### Arguments

* array: Array. 3D array or the subdataset FreqOutput_1x1.

##### Example usage

``` R

source('path/to/data_handling.R')
source('path/to/data_wrangling.R')

# Import the Freqeuncy/Power 1x1 array for embryo A2
freqOutput_1x1 <- getXArrayData(file = "A2Dataset.HDF5", subdataset = "FreqOutput_1x1")

maxFreq <- maxFreq_atTime(array = freqOutput_1x1)

```

#### `rescale()`

Normalise a dataframe or matrix to 0 - 1, by column or by row. Each column or row is independently normalised. Some defaults are chosen for ease of use with the data produced from previous methods. 

##### Arguments

* data: Dataframe or matrix. Dataset to normalise. 
* columns: Numeric vector. Default is 2:ncol(data). Columns to normalise. 
* rows: Numeric vector. Default is 2:ncol(data). Rows to normlaise. 
* byrow: Numeric vector. Default is FALSE. Whether to normalise across rows. 
* bycol: Numeric vector. Default is TRUE. Whether to normalise across columns. 

##### Example usage

```R
source('path/to/data_handling.R')
source('path/to/data_wrangling.R')

# Import the Freqeuncy/Power 1x1 array for embryo A2
freqOutput_1x1 <- getXArrayData(file = "A2Dataset.HDF5", subdataset = "FreqOutput_1x1")

# Transform the array to a matrix, where values are the power data 
freqOutput_mat <- freqOutput_1x1[1:dim(freqOutput_1x1)[1], 2, 1:dim(freqOutput_1x1)[3]]

# Normalise to 0 - 1
rescaled <- rescale(data = freqOutput_mat)


```

