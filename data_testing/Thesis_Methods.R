#Set working directory 

setwd("C:/GitHub/johnson_capstone/data_testing")

getwd()
dir()

#All 3 excell files I'm going to read in are CSV's. 

Painter_LT <- read.csv("Painter_Land_X_QC.csv")

Painter_ST00 <- read.csv("Painter_Stream_X_QC.csv")

Painter_ST01 <- read.csv("Painter_Stream0_X_QC.csv")


#The problem is the stream temperature data is split up into two data sets, and I want them as 1 data set.
#So lets merge the two. 

Painter_ST <- merge(Painter_ST00, Painter_ST01, all = TRUE)
                    
#This works, but the dates are not in order??????????

#Before I try to compare fish and temperature together, I want to just look at temperature alone first. 
#But first we need to format that date and time. To do that, lets install the luberdate package. 

install.packages("lubridate")
library("lubridate")
        
#Now need to get R to reed this as a year date and time rather than a string of numbers.

class(Painter_LT$DateTime)
class(Painter_ST$DateTime)

#It's eneterd as a character, so need to change it to POSIXct
#First do land temp

Painter_LT$DateTime <- as.POSIXct(Painter_LT$DateTime)
class(Painter_LT$DateTime)

summary(Painter_LT)

#Do same thing for stream temp 

Painter_ST$DateTime <- as.POSIXct(Painter_ST$DateTime)
class(Painter_ST$DateTime)

summary(Painter_ST)

#Now that its in the proper format, have to extract month as an individual column.
#In order to do this we need to use the muate function, which is in the dplyr package. 

install.packages("dplyr")
library(dplyr)

#First do land temp

Painter_LT_Updated <- Painter_LT %>%
  mutate(Month=month(Painter_LT$DateTime, label = TRUE))
summary(Painter_LT_Updated)

#Now do stream temp 
Painter_ST_Updated <- Painter_ST %>%
  mutate(Month=month(Painter_ST$DateTime, label = TRUE))
summary(Painter_ST_Updated)

#Bellow seemed to work to get year as a column 
#Now have to extract year as a new column 
Painter_LT_YM <- Painter_LT_Updated %>%
  mutate(Year=year(Painter_LT_Updated$DateTime))
summary(Painter_LT_YM)

Painter_ST_YM <- Painter_ST_Updated %>%
  mutate(Year=year(Painter_ST_Updated$DateTime))
summary(Painter_ST_YM)

#Need to combine Year and Month columns to be one column formatted as [YYYY-MM]. 
#Will need ot use the unite function. To do that, install tidyr package.

install.packages("tidyr")
library(tidyr)

