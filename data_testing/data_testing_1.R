#Set working directory 

setwd("C:/GitHub/johnson_capstone/data_testing")

getwd()
dir()

$#First we will need to download the readxl package since one out of the three data sets is a .xlsx

install.packages("readxl")
library(readxl)

#So now I will read in the .xlsx file "Data.xlsx"

Brunnerdale_F <- read_excel("FishRecords.xlsx")

#Beware that using the read_excel function turns the dara into a "tibble format". 
#In order to fix this, I need to do the as.data.frame function

Brunnerdale_Fish_Records <- as.data.frame(Data1.xlsx.tibble)


#This is not an excel file, so we don't need to download the read excell package.

#Read in water temp data for Long term Loyalsock Project at the Brunnerdale location.  

Brunnerdale_Stream_Temp <- read.csv("Brunnerdale_Run_Stream_X_QC.csv")

Brunnerdale_Land_Temp <- read.csv("Brunnerdale_Run_Land_X_QC.csv")

Brunnerdale_Fish_Records <- read.csv("FishRecords.csv")

#Now that both CSV's are red in, we need  a unique identifier that brings the two data sets together. 
#Luckily, both the temperature and fish data was taken in the same location (Brunnerdale Run). 
#However, these are not currennlty columns in the data tables, so I need to add a column.
#
