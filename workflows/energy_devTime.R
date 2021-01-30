library(ggplot2)

source("/home/z/Desktop/github/misc_embryoPhenomics/R/data_handling.R")

# Script for calculating the total energy vs development time for Radix Balthica

# Hatch times to exclude data from
hatchTimes <- read.csv("/run/media/z/Z/test data/hatchTime.csv", header=TRUE)
hatch20 <- hatchTimes[hatchTimes$Temperature == 20,]
hatch25 <- hatchTimes[hatchTimes$Temperature == 25,]
hatch30 <- hatchTimes[hatchTimes$Temperature == 30,]

# Embryos to exclude
group20 <- c('A5', 'F3', 'B5', 'C3','E1','E7')
group25 <- c('A2', 'A3', 'D7','E4')
group30 <- c('B2','B4','B7','B8','C2','C4','E5','E7','E8','A2','A3','D3','D5','F5','F3')

completeName <- function(name) {
  name <- paste0(name, "dataset.HDF5")
}

# Complete to filenames for removing from file list
group20 <- sapply(group20, completeName)
group25 <- sapply(group25, completeName)
group30 <- sapply(group30, completeName)

files20 <- list.files("/run/media/z/Z/embryoCV_data/20_deg/", pattern = ".HDF5")
files25 <- list.files("/run/media/z/Z/embryoCV_data/25_deg/", pattern = ".HDF5")
files30 <- list.files("/run/media/z/Z/embryoCV_data/30_deg/", pattern = ".HDF5")

# Exclude embryos from file list
files20 <- files20[!grepl(paste0(group20, collapse = "|"), files20)]
files25 <- files25[!grepl(paste0(group25, collapse = "|"), files25)]
files30 <- files30[!grepl(paste0(group30, collapse = "|"), files30)]

names20 <- sapply(files20, function(x) gsub(pattern="dataset.HDF5", "", x))
names25 <- sapply(files25, function(x) gsub(pattern="dataset.HDF5", "", x))
names30 <- sapply(files30, function(x) gsub(pattern="dataset.HDF5", "", x))

path20 <- "/run/media/z/Z/embryoCV_data/20_deg/"
path25 <- "/run/media/z/Z/embryoCV_data/25_deg/"
path30 <- "/run/media/z/Z/embryoCV_data/30_deg/"

# Simple helper function for looping through data and returning the total energy values
computeTotalEnergy <- function(hatchTimes, files, names, path) {
  
  data <- data.frame(time = NA, mr = NA)
  #data <- list()
  
  for (i in 1:length(files)) {
    
    array <- getXArrayData(paste0(path, files[i]), "FreqOutput_1x1")
    powerData <- data.frame(t(array[1:dim(array)[1], 2, 1:dim(array)[3]])) # transposing is used for better readability
    
    hatchTime <- hatchTimes[hatchTimes$Embryo == names[i], 5]
    powerData$MR <- rowSums(powerData, na.rm = TRUE)
    powerData$MR <- powerData$MR
    # totalEnergy <- sum(powerData[1:hatchTime, 1:301], na.rm = TRUE)
    
    #data[[names[i]]] <- data.frame(time = 1:hatchTime, mr = powerData$MR[1:hatchTime])
    mr <- mean(powerData[1:hatchTime, "MR"], na.rm = TRUE)
    
    data[i, 1] <- hatchTime
    data[i, 2] <- mr
    
  }
  
  return(data)
  
}

data20 <- computeTotalEnergy(hatchTimes = hatch20, files = files20, names = names20, path = path20)
data25 <- computeTotalEnergy(hatchTimes = hatch25, files = files25, names = names25, path = path25)
data30 <- computeTotalEnergy(hatchTimes = hatch30, files = files30, names = names30, path = path30)

ggplot(data20$A1, aes(time, mr)) + geom_line()

ggplot() +
  geom_path(data = data20$A1, aes(time, mr, colour = "20 C")) + 
  geom_path(data = data25$A1, aes(time, mr, colour = "25 C")) + 
  geom_path(data = data30$A6, aes(time, mr, colour = "30 C")) +
  labs(x = "Time to hatching (hrs)", y = "Metabolic rate") + 
  theme_bw() + 
  guides(colour = guide_legend(title = "Temperature"))

ggplot() + 
  geom_point(data = data20, aes(time, mr, colour = "20 C")) + 
  geom_point(data = data25, aes(time, mr, colour = "25 C")) +
  geom_point(data = data30, aes(time, mr, colour = "30 C")) +
  labs(x = "Time to hatching (hrs)", y = "Metabolic rate") + 
  theme_bw() + 
  guides(colour = guide_legend(title = "Temperature"))

# Compute development cost
data20$cost <- data20$time
data25$cost <- data25$time
data30$cost <- data30$time

dataAll <- data.frame(temp = NA, cost = NA)
dataAll[1:41, 1] <- 20
dataAll[42:79, 1] <- 25
dataAll[80:103, 1] <- 30
dataAll$temp <- factor(dataAll$temp)
dataAll[1:41, 2] <- data20$cost
dataAll[42:79, 2] <- data25$cost
dataAll[80:103, 2] <- data30$cost

ggplot(dataAll, aes(temp, cost, colour = temp)) +
  geom_boxplot() +
  labs(x = "Temperature", y = "Development cost") + 
  theme_bw() +
  guides(colour = guide_legend(title = "Temperature"))

  








