#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Now Need to read in data files

#First we will need to download the readxl package since one out of the three data sets is a .xlsx

install.packages("readxl")
library(readxl)

AllFishRec <- read_excel ("All_FishRecords.xlsx")


#To build a loopp for all the sites, its easier to chose one site and mess around with that first instead of trying all the sites at once!
#Also to make things easier, lets choose a site that has been quality control checked for temp data!
#Painter run aka Painter.LittleBear will be our first choice and its already quality control checked. 

#Now need to read in land temp and stream temp data Painter  
#These two are .csv's though so we have to read them in using the read.csv command 

Painter_LT <- read.csv("Painter_Land_X_QC.csv")

Painter_ST <- read.csv("Painter_Stream_X_QC.csv")

install.packages("dplR")
library(dplyr)

By.Site <- filter(AllFishRec, waterName == "Painter.Run")

By.Species_BKT <- filter(By.Site, Species == "Brook Trout")

#Next figure out how to seperate fish less than or equal to certain lengths and then how to make ratio, CPUE, and then how to get fish survesy and temp both togehter to analyze?

#The next step is to classify what Brook Trout are Juvenile/Young of year (YOY) and adults bu size. 
#Based on literature searches, The range for juvenile Brook Trout is 7 to 10 inches and adults are 100 to 120mm. 
#For our protocols, we take fish lengths in milometers (mm). 
#That makes Juvenile Brook trout 70 to 100mm and adults greater than  or equal to 101mm. 
#So anything anything equal to 100mm or less will be a juvenile Trout. Anything greater than or equal to 101mm will be considered an adult Brook Trout. 
#These numbers will likely need to change. 

YOY <- subset(By.Species_BKT, By.Species_BKT$Length_mm <100)
Adult <- subset(By.Species_BKT, By.Species_BKT$Length_mm >101)

YOY$AgeClass <- c(rep("YOY", nrow(YOY)))

Adult$AgeClass <- c(rep("Adult", nrow(Adult)))

Fish.Age.Site <- rbind(YOY, Adult)


hist(By.Species_BKT$Length_mm, breaks = 20)

#Seems adults start at 90, juveniles start at 85? Ask Sara

#Messing with Catch per Unit Effort Stuff bellow

AllFishSurvey <- read_excel("All_FishSurvey.xlsx")

#Bellow is the one you want and thhat works for pulling event info for certain sites 

#In order to calculate CPUE for Painter Run, I need to pull dates, widths, and shock times from the AllFishSurvey data frame. 
PainterEventInfo <- subset(AllFishSurvey, AllFishSurvey$SiteCode == "Painter.LittleBear")
#Should I only use Painter.LittleBear as painter run or should i use other one as well like Painter.Elk , the UNT, etc???? Ask Matt next meeting 

#Install the lubridate package to doubble chekc that the dates are in POSIXct and NOT character
install.packages("lubridate")
library("lubridate")

class(PainterEventInfo$Date)

#It's in POSIXct, Yay!

#Next, lets reomove some clutter and columns we dont need from the PainterEventInfo data frame. 

PainterEventInfo1 <- PainterEventInfo[, -c(39:66)]

#Now to calculate Area by doing Length * Width 

PainterEventInfo1$Area <- PainterEventInfo1$Length * PainterEventInfo1$AveWid
print(df)

#Now we have our area column and need to calculate CPUE for Painter run 
#We will do this for each year by its unique event code. 
#First is 201107.Painter.LittleBear from 2011

event_code <- "201107.Painter.LittleBear"

TwentyEleven <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyEleven_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyEleven$Area <- TwentyEleven_Area$Area[1]

TwentyEleven$BTCount <- rep(1, times = 11)

TwentyEleven$CPUE <- sum(TwentyEleven$BTCount) / TwentyEleven$Area

#CHECK IF CPUE SHOULD BE A PERCENTAGE OR NOT. That should be an easy fix though. 
#Now repeat for each year site was sampled. 

#Next is  201206.Painter.LittleBear from 2012

event_code <- "201206.Painter.LittleBear"

TwentyTwelve <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyTwelve_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyTwelve$Area <- TwentyEleven_Area$Area[1]

TwentyTwelve$BTCount <-rep(1, times = 35)

TwentyTwelve$CPUE <- sum(TwentyTwelve$BTCount) / TwentyTwelve$Area

#Next is  201306.Painter.LittleBear from 2013

event_code <- "201306.Painter.LittleBear"

TwentyThirteen <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyThirteen_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyThirteen$Area <- TwentyThirteen_Area$Area[1]

TwentyThirteen$BTCount <-rep(1, times = 43)

TwentyThirteen$CPUE <- sum(TwentyThirteen$BTCount) / TwentyThirteen$Area

#Next is 201406.Painter.LittleBear from 2014

event_code <- "201406.Painter.LittleBear"

TwentyFourteen <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyFourteen_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyFourteen$Area <- TwentyFourteen_Area$Area[1]

TwentyFourteen$BTCount <-rep(1, times = 10)

TwentyFourteen$CPUE <- sum(TwentyFourteen$BTCount) / TwentyFourteen$Area

#Next is 201506.Painter.LittleBear from 2015

event_code <- "201506.Painter.LittleBear"

TwentyFifteen <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyFifteen_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyFifteen$Area <- TwentyFifteen_Area$Area[1]

TwentyFifteen$BTCount <-rep(1, times = 22)

TwentyFifteen$CPUE <- sum(TwentyFifteen$BTCount) / TwentyFifteen$Area

#Next is 201606.Painter.LittleBear from 2016

event_code <- "201606.Painter.LittleBear"

TwentySixteen <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentySixteen_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentySixteen$Area <- TwentySixteen_Area$Area[1]

TwentySixteen$BTCount <-rep(1, times = 24)

TwentySixteen$CPUE <- sum(TwentySixteen$BTCount) / TwentySixteen$Area

#Next is 201706.Painter.LittleBear from 2017

event_code <- "201706.Painter.LittleBear"

TwentySeventeen <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentySeventeen_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentySeventeen$Area <- TwentySeventeen_Area$Area[1]

TwentySeventeen$BTCount <-rep(1, times = 12)

TwentySeventeen$CPUE <- sum(TwentySeventeen$BTCount) / TwentySeventeen$Area

#Next is 201806.Painter.LittleBear from 2018

event_code <- "201806.Painter.LittleBear"

TwentyEighteen <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyEighteen_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyEighteen$Area <- TwentyEighteen_Area$Area[1]

TwentyEighteen$BTCount <-rep(1, times = 15)

TwentyEighteen$CPUE <- sum(TwentyEighteen$BTCount) / TwentyEighteen$Area

#Next is 201906.Painter.LittleBear from 2019

event_code <- "201906.Painter.LittleBear"

TwentyNineteen <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyNineteen_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyNineteen$Area <- TwentyNineteen_Area$Area[1]

TwentyNineteen$BTCount <-rep(1, times = 31)

TwentyNineteen$CPUE <- sum(TwentyNineteen$BTCount) / TwentyNineteen$Area

#Next is 202006.Painter.LittleBear from 2020

event_code <- "202006.Painter.LittleBear"

TwentyTwenty <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyTwenty_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyTwenty$Area <- TwentyTwenty_Area$Area[1]

TwentyTwenty$BTCount <-rep(1, times = 10)

TwentyTwenty$CPUE <- sum(TwentyTwenty$BTCount) / TwentyTwenty$Area

#Next is 202106.Painter.LittleBear from 2021

event_code <- "202106.Painter.LittleBear"

TwentyTwentyOne <- By.Species_BKT[By.Species_BKT$EventCode == event_code, ]

TwentyTwentyOne_Area <- subset(PainterEventInfo1, EventCode == event_code)

TwentyTwentyOne$Area <- TwentyTwentyOne_Area$Area[1]

TwentyTwentyOne$BTCount <-rep(1, times = 10)

TwentyTwentyOne$CPUE <- sum(TwentyTwentyOne$BTCount) / TwentyTwentyOne$Area

#2021 is the onlyy year I can't get to work. I think its because the water name in PainterEventsInfo 1 is "Painter Run" and not "Painter.Run" like all the other years are. 
#I treid multiple ways of fixing this and nothing seems to work. 

#Making Ratios 

install.packages("tidyr")
library(tidyr)

AgeClassCounts <- Fish.Age.Site %>%
  group_by(EventCode, AgeClass) %>%
  summarize(count = n())

# Pivot the data to have YOY and adult as columns
ratio_data <- AgeClassCounts %>%
  pivot_wider(names_from = AgeClass, values_from = count, values_fill = 0)

# Calculate the ratio
ratio_data <- ratio_data %>%
  mutate(Ratio_YOY_to_Adult = YOY / Adult)

#Now lets calculate biomass 

Biomass <- data.frame("2011" = NA, "2012" = NA, "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
                      
Biomass$X2011 <- sum(TwentyEleven$Wt_g)/ (TwentyEleven$Area[1])

Biomass$X2011 <- sum(TwentyEleven$Wt_g) / TwentyEleven$Area

Biomass$X2011 <- sum(TwentyEleven$Wt_g) / TwentyEleven$Area

sum(TwentyEleven$Wt_g) / TwentyEleven$Area[1]

Biomass <- data.frame("2011" = NA)
