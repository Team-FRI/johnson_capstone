#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Now Need to read in data files

#First we will need to download the readxl package since one out of the three data sets is a .xlsx

install.packages("readxl")
library(readxl)

AllFishRec <- read_excel ("All_FishRecords.xlsx")


#To build a lopp for all the sites, its easier to chose one site and mess around with that first instead of trying all the sites at once!
#Also to make things easier, lets choose a site that has been quality control checked for temp data!
#Painter run aka Painter.LittleBear will be our first choice and its already quality control checked. 

#Now need to read in land temp and stream temp data Painter  
#These two are .csv's though so we have to read them in using the read.csv command 

Painter_LT <- read.csv("Painter_Land_X_QC.csv")

Painter_ST00 <- read.csv("Painter_Stream_X_QC.csv")

Painter_ST01 <- read.csv("Painter_Stream0_X_QC.csv")

#The problem is the stream temperature data is split up into two data sets, and I want them as 1 data set.
#So lets merge the two. 

Painter_ST <- merge(Painter_ST00, Painter_ST01, all = TRUE)

#To arrange dates in right order, install dplyr package.
install.packages("dplyr")
library(dplyr)

#Now to arrange dates in right order:
Painter_ST <- arrange(Painter_ST, DateTime)

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


#Filter All things in the AllFishRec survey that haave "Painter.Run" in the water name. 
#Also filter all "Brook Trout" from the Species columnn out of the By.Site data set. 

By.Site2 <- subset(AllFishRec, waterName == "Painter.Run")
By.Site1 <- subset(AllFishRec, waterName == "Painter Run")
By.Site <- rbind(By.Site2, By.Site1)
By.Species_BKT <- filter(By.Site, Species == "Brook Trout")


#The next three factors are all Fish factors!
#Ratio

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

TwentyTwentyOne$BTCount <-rep(1, times = 24)

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

Biomass <- data.frame("2011" = NA, "2012" = NA, "2013" = NA, "2014" = NA, "2015" = NA, "2016" = NA, "2017" = NA, "2018" = NA, "2019" = NA, "2020" = NA, "2021" = NA)
                      
Biomass$X2011 <- sum(TwentyEleven$Wt_g)/ (TwentyEleven$Area[1])

Biomass$X2012 <- sum(TwentyTwelve$Wt_g) / (TwentyTwelve$Area[1])

Biomass$X2013 <- sum(TwentyThirteen$Wt_g) / (TwentyThirteen$Area[1])

Biomass$X2014 <- sum(TwentyFourteen$Wt_g) / (TwentyFourteen$Area[1])

Biomass$X2015 <- sum(TwentyFifteen$Wt_g) / (TwentyFifteen$Area[1])

Biomass$X2016 <- sum(TwentySixteen$Wt_g) / (TwentySixteen$Area[1])

Biomass$X2017 <- sum(TwentySeventeen$Wt_g) / (TwentySeventeen$Area[1])

Biomass$X2018 <- sum(TwentyEighteen$Wt_g) / (TwentyEighteen$Area[1])

Biomass$X2019 <- sum(TwentyNineteen$Wt_g) / (TwentyNineteen$Area[1])

Biomass$X2020 <- sum(TwentyTwenty$Wt_g) / (TwentyTwenty$Area[1])

Biomass$X2021 <- sum(TwentyTwentyOne$Wt_g) / (TwentyTwentyOne$Area[1])

Biomass <- as.data.frame(t(Biomass))

#2021 will not run correclty of course 

#The next three factors are temperature factors

#Brook Trout start to experience thermal stress at about 20 degress ceclisius. Temperatures of 24 to 25 degrees Celsius are lethal for them. 

#Days Over 20 degrees Celsius factor. 
#So for each year, we are going to determine how many days the tempertuaure is over 20 degrees celcius. 

#To do that, I need to pull out the year and make it a new column

Painter_LT$Year <- format(as.Date(Painter_LT$DateTime), "%Y")
summary(Painter_LT)

Painter_ST$Month <- month(ymd(Painter_ST$DateTime), label = TRUE, abbr = FALSE)

#Now do it for stream temp 

Painter_ST$Year <- format(as.Date(Painter_ST$DateTime), "%Y")
summary(Painter_ST)

Painter_ST$Month <- month(ymd(Painter_ST$DateTime), label = TRUE, abbr = FALSE)

#lubrudate package is already installed, so need to group by year and mutate a new column to show the days over 20C fro Stream Temp 

Painter_ST <- Painter_ST %>%
  group_by(Year) %>%
  mutate(Days_Over_20C = sum(Temp_C >= 20))

#Lets do the same ting but for days greater then or equal to 24 degrees Celsius

Painter_ST <- Painter_ST %>%
  group_by(Year) %>%
  mutate(Days_Over_24C = sum(Temp_C >= 24))

#The Next factor is going to be degree days for certain periods of time each year.The Higher the degree days, you can get without thermal stress, the more development and rate of development you can get.
#Lets start with Brook Trouts spawning period which is September to October. 

#Calculate average degree days for each year for september and october. 

#Messing around with degree days bellow 

base_temperature <- c(0)

Painter_ST20 <- Painter_ST %>%
  mutate(Date = as.Date(DateTime),  # Convert DateTime to Date
         DegreeDays = pmax(0, Temp_C - base_temperature)) %>%
  group_by(Year) %>%
  summarize(TotalDegreeDays = sum(DegreeDays))


Painter_ST <- Painter_ST %>%
  mutate(Date = as.Date(DateTime),
         Month = format(Date, "%m"),
         DegreeDays = pmax(0, Temp_C - base_temperature)) %>%
  filter(Month %in% c("09", "10")) %>%
  group_by(Year) %>%
  summarize(TotalDegreeDays = sum(DegreeDays))

Painter_ST <- Painter_ST %>%
  mutate(Date = as.Date(DateTime),
         Month = format(Date, "%m"),
         DegreeDays = pmax(0, Temp_C - base_temperature)) %>%
  filter(Month %in% c("09", "10")) %>%
  group_by(Year) %>%
  summarize(
    TotalDegreeDays = sum(DegreeDays),
    NumDaysBetweenSepOct = n(),  # Number of days between September and October
    AvgTemperature = mean(Temp_C)  # Average temperature for those two months
  )

#The Third factor will be the highest and lowest stream temperature of each month to see what the extremes are.

Painter_Monthly_Extremes <- Painter_ST %>%
  group_by(Year, Month) %>%
  summarize(
    Highest_Temperature_C = max(Temp_C, na.rm = TRUE),
    Lowest_Temperature_C = min(Temp_C, na.rm = TRUE)
  )         

?na.rm




######Count of days over or under 20 C


minmaxtemp <- transform(Temps, min=ave(Temp_C, strftime(DateTime, '%F'), FUN=min),
               max=ave(Temp_C, strftime(DateTime, '%F'), FUN=max))

minmaxtemp$tempdiff <- minmaxtemp$max - minmaxtemp$min
#sept2018 <- subset(minmaxtemp, minmaxtemp$Month == "September" & minmaxtemp$Year == 2018)

Max2018 <- subset(minmaxtemp, minmaxtemp$Year == 2018)
sum(unique(Max2018$max) > 15, na.rm=TRUE)

Max2019 <- subset(minmaxtemp, minmaxtemp$Year == 2019)
sum(unique(Max2019$max) > 10, na.rm=TRUE)

Max2020 <- subset(minmaxtemp, minmaxtemp$Year == 2020)
sum(unique(Max2020$max) > 20, na.rm=TRUE)


