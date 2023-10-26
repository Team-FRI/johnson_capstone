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
Brunnerdale_Stream_Temp_Updated <- Brunnerdale_Stream_Temp %>%
  mutate(Month=month(Brunnerdale_Stream_Temp$DateTime, label = TRUE))
summary(Brunnerdale_Stream_Temp_Updated)

#Bellow seemed to work to get year as a column 
#Now have to extract year as a new column 
Brunnerdale_Land_Temp_YM <- Brunnerdale_Land_Temp_Updated %>%
  mutate(Year=year(Brunnerdale_Land_Temp_Updated$DateTime))
summary(Brunnerdale_Land_Temp_YM)

Brunnerdale_Stream_Temp_YM <- Brunnerdale_Stream_Temp_Updated %>%
  mutate(Year=year(Brunnerdale_Stream_Temp_Updated$DateTime))
summary(Brunnerdale_Stream_Temp_YM)

#Need to combine Year and Month columns to be one column formatted as [YYYY-MM]. 
#Will need ot use the unite functio. To do that, install tidyr package.

#example code to run
NewObjectName<- ObjectName %>%
  unite(YearMonth, Year, Month, “-“)

install.packages("tidyr")
library("tidyr")

#Now try uniting Year and Month Columns.
#NOTE***** From here down, Land temp and stream temp will be abbreviated as LT and ST in object names 
#For Land temp:

Brunnerdale_LT_Master <- Brunnerdale_Land_Temp_YM %>%
  unite(YearMonth, Year, Month)
#For Stream temp:

Brunnerdale_ST_Master <- Brunnerdale_Stream_Temp_YM %>%
  unite(YearMonth, Year, Month)

#So it works, however if I leave the "-" in their as it is in lune 114-115, it gives me an error message saying 
#"Error in unite()'
#Can't subset columns that don't exist.
#Column '-' doesn't exist."
#It gives an _ in between the year and month column instead of -. 



#Messing with YYY-MM Stuff Bellow 

Brunnerdale_Land_Temp_Updated1$date <- Brunnerdale_Land_Temp_Updated1 %>%
  as.yearmon(paste(df$year, df$month), "%Y %m")

Brunnerdale_Land_Temp_Updated1 <- 

my(Brunnerdale_Land_Temp_Updated$DateTime)

Brunnerdale_Land_Temp_Updated1 <- as.Date(with(df, paste(year, mon, day,sep="-")), "%Y-%m-%d")
Brunnerdale_Land_Temp$DateTime

Brunnerdale_Land_Temp_Updated1 %>%
  mutate(date = make_date(year, month))

#Try

Brunnerdale_Land_Temp_Updated["year"] <- Brunnerdale_Land_Temp_Updated["Year"]dt.year
