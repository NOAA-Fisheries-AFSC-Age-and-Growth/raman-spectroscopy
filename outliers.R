#Check for outliers

##Packages
library(hyperSpec)
library(tidyverse)
library(EMSC) 
library(ggforce)
library(baseline)
library(prospectr)
library(plotly)

##Load in pre-processed data
### Fingerprint region
fr_data <- read.csv("~/AFSC A&G Contract/Raman Spectroscopy/raman-spectroscopy/preprocessed_fingerprint.csv") #just SG smoothing for figs

sr_data <- read.csv("~/AFSC A&G Contract/Raman Spectroscopy/raman-spectroscopy/preprocessed_stretch.csv") #derivative for PCA

## Prepare maturity data for joining 
filenames <- unique(fr_data$filenames) #make vector of filenames so data can be joined
filenames <- cbind(filenames,filenames)
colnames(filenames)[2] <- "filenameA"

mat_naming <- splitstackshape::cSplit(filenames,sep="_",splitCols="filenameA") #split filename to grab barcode for joining
colnames(mat_naming)[5] <- "barcode"
mat_naming <- mat_naming[,c(1,5)] #keep filename and barcode columns to join with maturity data

maturity_raw <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Walleye Pollock Maturity Data/Maturity_data_histomaturity.csv", strip.white=TRUE) #load in ancillary data

maturity_data <- left_join(maturity_raw, mat_naming, by = "barcode") #join to filename by barcode

#Clean up variables etc.

maturity_data$filenames.x <- NULL
maturity_data <- rename(maturity_data, filenames = filenames.y)

maturity_data$HistoMaturity <- factor(maturity_data$HistoMaturity , levels = c("IMM","ND","DEV","VIT","PSWN","SWN","PSNT","SNT","OSNT"))
levels(maturity_data$HistoMaturity)

#Add column with outlier status 1= Morgan, 2 = Irina
maturity_data$outlier <- ifelse(maturity_data$barcode %in% c(64451, 64370, 64505, 65696, 64753, 64908, 65937, 65048, 65721, 65936), 1,
                                ifelse(maturity_data$barcode %in% c(65956, 66029, 66215, 65200, 65988, 64443, 65236, 65955, 65823, 65138, 72260, 72614, 64628, 70785), 2, 0))

maturity_data$outlier <- as.factor(maturity_data$outlier)

## Join fingerprint and maturity data
fingerprint_data <- right_join(fr_data, maturity_data, by = "filenames")  #join by filename to each data set

## Join Stretch and maturity data
stretch_data <- right_join(sr_data, maturity_data, by = "filenames")  #join by filename to each data set

#Plot with outlier in red

f <- ggplot(fingerprint_data)+
  geom_line(aes(x = Ramanshift, y = spc_sg, group = filenames, color = outlier))+
  scale_x_continuous(breaks=seq(600,1800,250))+
  theme_classic()

ggplotly(f)

s <- ggplot(stretch_data)+
  geom_line(aes(x = Ramanshift, y = spc_sg, group = filenames, color = outlier))+
  scale_x_continuous(breaks=seq(600,1800,250))+
  theme_classic()

ggplotly(s)
