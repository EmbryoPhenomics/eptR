## data_handling

This script contains various methods for reading in the XArray data produced from EmbryoCV and performing some other basic operations on the data produced. Note that some methods are only supported on linux currently, and so may require some further installation steps before being usable in other OS's. 

#### `openXArray()`

Retrieve metadata for a particular XArray file. Requires gdal to be installed. Note that only .HDF5 files have been tested so far and so other XArray formats may not be recognized.

##### Arguments

* filename: Character string. Filename of XArray file to retrieve metadata for. 
* path: Character string. Default is NULL. Path to file to obtain metadata for. If not specified the current directory is used. 

##### Example usage

``` R

source('/path/to/data_handling.R')

# Obtain metadata for XArray dataset for embryo A2
metadata <- openXArray(filename = "A2Dataset.HDF5", path = ".")

```

#### `getXArrayData()`

Import a subdataset of a given XArray file. Only HDF5 files of format NCDF or netCDF (Network Common Data Format) are supported for this method. Note that this method requires gdal to be installed, for a more barebones method please see `getXArrayData_basic()` below. 

##### Arguments

* file: Character string. Filename of XArray file to retrieve subdataset from.
* subdataset: Character string. Name of subdataset to retrieve. 

##### Example usage

``` R

source('/path/to/data_handling.R/')

# Import the Freqeuncy/Power 1x1 array for embryo A2
freqOutput_1x1 <- getXArrayData(file = "A2Dataset.HDF5", subdataset = "FreqOutput_1x1")

```

#### `getXArrayData_basic()`

Same as above except more 'barebones'. Has been tested on both MacOS and Linux. 

##### Arguments

* file: Character string. Filename of XArray file to retrieve subdataset from.
* subdataset: Character string. Name of subdataset to retrieve. 


##### Example usage

``` R

source('/path/to/data_handling.R/')

# Import the Freqeuncy/Power 1x1 array for embryo A2
freqOutput_1x1 <- getXArrayData_basic(file = "A2Dataset.HDF5", subdataset = "FreqOutput_1x1")

```

#### `getEmbryoLabels()`

List all the embryo names found in a particular folder.

##### Arguments

* path: Character string. Path to folder containing XArray data for a given set of embryos. 

------------------------------------------------------------------------------------------------

#### Important notes

Both `matrixToDataframe()` and `values()` functions should only be used for smaller datasets as they are inefficient methods and have not been optimised. 



