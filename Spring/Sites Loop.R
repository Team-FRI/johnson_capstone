#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Now Need to read in data files

#First we will need to download the readxl package since one out of the three data sets is a .xlsx

install.packages("readxl")
library(readxl)

AllFishRec <- read_excel ("All_FishRecords.xlsx")


#To build a loopp for all the sites, its easier to chose one site and mess around with that first insetad of trying all the sites at once!
#Also to make things easier, lets choose a site that has been quality control checked for temp data!
#Painter run aka Painter.LittleBear will be our first choice and its already quality control checked. 

#Now need to read in land temp and stream temp data Painter  

Painter_LT <- 