# Data for all embryos, with select embryos excluded

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

# Retrieve data for all embryos
data20 <- retrieveFiles(files = files20, 
                         path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/20degrees/",
                         output = "matrix",
                         format = "dataframe",
                         rescale = TRUE,
                         group = 20)

data25 <- retrieveFiles(files = files25, 
                         path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/25degrees/",
                         output = "matrix",
                         format = "dataframe",
                         rescale = TRUE,
                         group = 25)

data30 <- retrieveFiles(files = files30, 
                         path = "/Users/bioImaging2/Desktop/EmbryoCV_datasets/datasets/30degrees/",
                         output = "matrix",
                         format = "dataframe",
                         rescale = TRUE,
                         group = 30)

write.csv(data20, file = "~/Desktop/ziad_scripts/data/allIndividuals/all20.csv")
write.csv(data25, file = "~/Desktop/ziad_scripts/data/allIndividuals/all25.csv")
write.csv(data30, file = "~/Desktop/ziad_scripts/data/allIndividuals/all30.csv")

