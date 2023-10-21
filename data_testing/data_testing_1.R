#Set working directory 

setwd("C:/GitHub/johnson_capstone/data_testing")

getwd()
dir()

#First we will need to download the readxl package since one out of the three data sets is a .xlsx

install.packages("readxl")
library(readxl)

#So now I will read in the .xlsx file "Data.xlsx"

Brunnerdale.ogdonia_FishRecords <- read_excel("Brunnerdale.ogdonia_FishRecords.xlsx")

#Beware that using the read_excel function turns the data into a "tibble format". 
#In order to fix this, I need to do the as.data.frame function

Brunnerdale.ogdonia_FishRecords1 <- as.data.frame(Brunnerdale.ogdonia_FishRecords)

#This other 2 datasets we need to read in are not excell files, their CSV's.

#Read in water temp data for Long term Loyalsock Project at the Brunnerdale location.  

Brunnerdale_Stream_Temp <- read.csv("Brunnerdale_Run_Stream_X_QC.csv")

Brunnerdale_Land_Temp <- read.csv("Brunnerdale_Run_Land_X_QC.csv")

#Now that both CSV's are red in, we need  a unique identifier that brings the two data sets together. 
#Luckily, both the temperature and fish data was taken in the same location (Brunnerdale Run at Ogdonia). 
#However, these are not currency columns in the data tables, so I need to add a column.
#To do that, I will need to R bind, but first I will need to remove the columns from the Brunnerdale_Fish_Records_1 file that I don't want to use????
# See above lines and edit columns taken out bellow ^??????

Brunnerdale.ogdonia_FishRecords2 <- Brunnerdale.ogdonia_Fish_Records1[,c(-3:-7, -10:-12, -14:-16)]

#Now that that's done we can add the column "Site Name" consiting of "Brunnerdale.ogdonia" to the Brunndedale stream and air temp tables

Brunnerdale_Stream_Temp$Site_Name <- "Brunnerdale.ogdonia"

Brunnerdale_Land_Temp$Site_Name <- "Brunnerdale.ogdonia"

#Now also add the column "Site Name" consiting of "Brunnerdale.ogdonia" to the Brunnerdale.ogdonia_FishRecords1 fish records data set

Brunnerdale.ogdonia_FishRecords1$Site_Name <- "Brunnerdale.ogdonia"

#Now we can finaly bring either all 3 data sets together, or just the fish records and either the land temperatue or water temperature data set together using the R bind function.

Brunnerdale_ogdonia_masterland <- rbind(Brunnerdale_Stream_Temp, Brunnerdale.ogdonia_FishRecords1)

#Get error message. I'm assuming its because the data sets don't have the same amount of rows. Maybe have to remove rows you wont do like in line 36 that you didin't run???
#When you come back to this, mess with looper date package for dates and tidy verse pacage, and then building the actual differnt model and using both air and water temps, or one or the other!!!!