#Set working directory 

setwd("C:/GitHub/johnson_capstone/data_testing")

getwd()
dir()

#3 of 4 excell files I'm going to read in are CSV's. 

Painter_LT <- read.csv("Painter_Land_X_QC.csv")

Painter_ST00 <- read.csv("Painter_Stream_X_QC.csv")

Painter_ST01 <- read.csv("Painter_Stream0_X_QC.csv")

#The Fish records file from Painter Stream is an .xslx, so need to download readxl package.

install.packages("readxl")
library(readxl)

Painter_FishRec_BT <- read_excel ("Painter_FishRecords_BrookTrout.xlsx")

#The problem is the stream temperature data is split up into two data sets, and I want them as 1 data set.
#So lets merge the two. 

Painter_ST <- merge(Painter_ST00, Painter_ST01, all = TRUE)

#To arrange dates in right order, install dplyr package.
install.packages("dplyr")
library(dplyr)

#Now to arrange dates in right order:
Painter_ST <- arrange(Painter_ST, DateTime)
                    

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
#In order to do this we need to use the muate function, which is in the dplyr package, which is already installed... 


#First do land temp

Painter_LT$YearMonth <- format(as.Date(Painter_LT$DateTime), "%Y-%m")
summary(Painter_LT)

#Now do stream temp 

Painter_ST$YearMonth <- format(as.Date(Painter_ST$DateTime), "%Y-%m")
summary(Painter_ST)


Painter_Stream_Summary <- Painter_ST %>%
  select(YearMonth, Temp_C) %>%
  group_by(YearMonth) %>%
  arrange(YearMonth) %>%
  summarise(AvgYearMonthTemp = mean(Temp_C))

Painter_Land_Summary <- Painter_LT %>%
  select(YearMonth, Temp_C) %>%
  group_by(YearMonth) %>%
  arrange(YearMonth) %>%
  summarise(AvgYearMonthTemp = mean(Temp_C))
            
            
#Use Painter_Stream_Summary to make figure 

#Use select the columns in the figure you want to use
#Filter selects rows
#So could select a site, and filter for Brook Trout

#Use plot of ggplot comes into play
#Do confident intervals. 

#In order to plot figure you want, install gg pot 2 package.

install.packages("ggplot2")
library("ggplot2")

install.packages("tidyr")
library("tidyr")

DataForExamplePlot_BRT <- Painter_FishRec_BT %>%
  select(ID, Date, Species, EventCode) %>%
  filter(Date > ymd("2018-01-01"))

Summary_Plot_BRTData <- DataForExamplePlot_BRT %>%
  group_by(Date) %>%
  summarise(Count = length(Species))

