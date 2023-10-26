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
#So lets add the column "Site Name" consiting of "Brunnerdale.ogdonia" to the Brunndedale stream and air temp tables

Brunnerdale_Stream_Temp$Site_Name <- "Brunnerdale.ogdonia"

Brunnerdale_Land_Temp$Site_Name <- "Brunnerdale.ogdonia"

#Now also add the column "Site Name" consiting of "Brunnerdale.ogdonia" to the Brunnerdale.ogdonia_FishRecords1 fish records data set

Brunnerdale.ogdonia_FishRecords1$Site_Name <- "Brunnerdale.ogdonia"

#In order to bring the 3 datasets together I will need to R bind, but first I will need to remove the columns from the Brunnerdale_Fish_Records_1 file that I don't want to use to have the same number of columns as Brunnerdale_Stream_Temp
# See above lines and edit columns taken out bellow ^??????

Brunnerdale.ogdonia_FishRecords2 <- Brunnerdale.ogdonia_FishRecords1[,c(-3:-7, -9:-12)]

#Now we can finaly bring either all 3 data sets together, or just the fish records and either the land temperatue or water temperature data set together using the R bind function.
#But sicne the columns are not the same name, we need to downlaod this package so the R bind will work. 

install.packages("dplyr")
library("dplyr")

Bind_ogdonia_masterland1 <- bind_rows(Brunnerdale.ogdonia_FishRecords2, Brunnerdale_Land_Temp)

#Before I try to compare fish and temperature together, I want to just look at temperature alone first. 
#But first we need to format that date and time. To do that, lets install the luberdate package. 

install.packages("lubridate")
library("lubridate")


#When you come back to this, mess with luberdate package for dates and tidy verse pacage, and then building the actual different model and using both air and water temps, or one or the other!!!!

#Always X and then Y for plots!

#Now need to get R to reed this as a year date and time rather than a string of numbers.

class(Brunnerdale_Land_Temp$DateTime)
class(Brunnerdale_Stream_Temp$DateTime)

#It's eneterd as a character, so need to change it to POSIXct
#First do land temp

Brunnerdale_Land_Temp$DateTime <- as.POSIXct(Brunnerdale_Land_Temp$DateTime)
class(Brunnerdale_Land_Temp$DateTime)

summary(Brunnerdale_Land_Temp)

#Do same thing for stream temp 

Brunnerdale_Stream_Temp$DateTime <- as.POSIXct(Brunnerdale_Stream_Temp$DateTime)
class(Brunnerdale_Stream_Temp$DateTime)

summary(Brunnerdale_Land_Temp)


#Now that its in the proper format, have to extract month as an individual column.
#First do land temp

Brunnerdale_Land_Temp_Updated <- Brunnerdale_Land_Temp %>%
  mutate(Month=month(Brunnerdale_Land_Temp$DateTime, label = TRUE))
summary(Brunnerdale_Land_Temp_Updated)

#Now do stream temp 
Brunnerdale_Stream_Temp_updated <- Brunnerdale_Stream_Temp %>%
  mutate(Month=month(Brunnerdale_Stream_Temp$DateTime, label = TRUE))
summary(Brunnerdale_Stream_Temp)

Brunnerdale_Land_Temp_Updated1 <- Brunnerdale_Land_Temp %>%
  mutate(Year=Year(Brunnerdale_Land_Temp$DateTime, label = TRUE))

