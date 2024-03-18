#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")
#HI

#Now Need to read in data files

#First we will need to download the readxl package since one out of the three data sets is a .xlsx

install.packages("readxl")
library(readxl)

#Install package tidy R 
install.packages("tidyr")
library(tidyr)

AllFishRec <- read_excel ("All_FishRecords.xlsx")
AllFishSurvey <- read.csv("All_FishSurvey.csv")

#To arrange dates in right order, install dplyr package.
install.packages("dplyr")
library(dplyr)


By.Site2 <- subset(AllFishRec, waterName == "Painter.Run")
By.Site1 <- subset(AllFishRec, waterName == "Painter Run")
By.Site <- rbind(By.Site2, By.Site1)

By.Species_BKT <- By.Site %>%
  mutate(
    StreamName = "Painter.Run") %>%
  filter(Species == "Brook Trout"
  )

PainterEventInfo <- subset(AllFishSurvey, AllFishSurvey$SiteCode == "Painter.LittleBear")


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

By.Species_BKT <- By.Site %>%
  mutate(
    StreamName = "Painter.Run") %>%
  filter(Species == "Brook Trout"
  )

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

#Seems adults start at 90, juveniles start at 70? Ask Sara

#Messing with Catch per Unit Effort Stuff bellow

AllFishSurvey <- read_excel("All_FishSurvey.xlsx")

#Bellow is the one you want and that works for pulling event info for certain sites 

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

PainterEventInfo2 <- select(PainterEventInfo1, EventCode, Area)

#Now we have our area column and need to calculate CPUE for Painter run 
#We will do this for each year by its unique event code. 
#First is 201107.Painter.LittleBear from 2011

BKT_Painter_AllYears <- By.Species_BKT %>%
  group_by(EventCode) %>%
  summarise(
    NumBKT = length(Species)
  )

CPUE <- left_join(BKT_Painter_AllYears, PainterEventInfo2, 
                  by = "EventCode")

CPUE <- CPUE %>%
  filter(!EventCode == "201107.TP.Painter.LittleBear") %>%
  mutate(
    CPUE = (NumBKT/Area) * 100
  )

#Making Ratios 

install.packages("tidyr")
library(tidyr)

AgeClassCounts <- Fish.Age.Site %>%
  filter(!EventCode == "201107.TP.Painter.LittleBear") %>%
  group_by(EventCode, AgeClass) %>%
  summarise(count = n())

# Pivot the data to have YOY and adult as columns
ratio_data <- AgeClassCounts %>%
  pivot_wider(names_from = AgeClass, values_from = count, values_fill = 0)

# Calculate the ratio
ratio_data <- ratio_data %>%
  mutate(Ratio_YOY_to_Adult = YOY / Adult)

ratio_data1 <- ratio_data %>%
  mutate(Ratio_Adult_to_YOY = Adult / YOY)
#Maybe multiply by 100 or make a percentage?

#Now lets calculate biomass 

Biomass <- By.Species_BKT %>%
  filter(!EventCode == "201107.TP.Painter.LittleBear") %>%
  group_by(EventCode) %>%
  summarise(
    Biomass = sum(Wt_g)
  )

CPUE_Biomass <- left_join(Biomass, PainterEventInfo2,
                          by = "EventCode")

CPUE_Biomass <- mutate(CPUE_Biomass,
  CPUE_Bio = (Biomass/Area) *100
)


#The next three factors are temperature factors

#Brook Trout start to experience thermal stress at about 20 degress ceclisius. Temperatures of 24 to 25 degrees Celsius are lethal for them. 

#Days Over 20 degrees Celsius factor. 
#So for each year, we are going to determine how many days the tempertuaure is over 20(or other) degrees Celsius. 

#To do that, I need to pull out the year and make it a new column

Painter_LT$Year <- format(as.Date(Painter_LT$DateTime), "%Y")
summary(Painter_LT)

Painter_LT$Month <- month(ymd(Painter_LT$DateTime), label = TRUE, abbr = FALSE)

#Now do it for stream temp 

Painter_ST$Year <- format(as.Date(Painter_ST$DateTime), "%Y")
summary(Painter_ST)

Painter_ST$Month <- month(ymd(Painter_ST$DateTime), label = TRUE, abbr = FALSE)

#Lets take Painter_ST and make it a new data frame so we don't rewrite Painter_ST in the feature 

Temps <- as.data.frame(Painter_ST)
#Now messing with 3 new variables. 

TempsMY <- as.data.frame(Temps)

TempsMY$Month <- format(TempsMY$DateTime, "%m")

#TempsMY$DateTime <- as.Date(TempsMY$DateTime)

#TempsMY$Month <- month(TempsMY$DateTime)

#Midnight - 11:59pm >10 degrees c

TempsG10 <- TempsMY %>%
  filter(Temp_C >= 10) %>%
  group_by(DateTime) %>%
  summarise(numberoflogs_dayG10 = n())

#AvgDailyTemp > 10 degres C

AvgDailyTemp <- TempsMY %>%
  group_by(DateTime) %>% 
  summarise(
  AvgDailyTempC = mean(Temp_C)) %>%
  filter(AvgDailyTempC >= 10)

#Daily max temp > 10 degrees C 

DailyMaxTemp <- TempsMY %>%
  group_by(DateTime) %>%
  summarize(
    DailyMaxTempC = max(Temp_C)) %>%
    filter(DailyMaxTempC >= 10)

# + # of logs at max temp

#Temps Month Year

#Midnight - 11:59pm >10 degrees c

TempsG10MY <- TempsMY %>%
  filter(Temp_C >= 10) %>%
  group_by(Year, Month) %>%
  summarise(numberoflogs_MYG10 = n())

#AvgMYTemp > 10 degres C

Avg_YM_TempMY <- TempsMY %>%
  group_by(Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC >= 10)

#MY max temp > 10 degrees C 

MaxTemp_MY <- TempsMY %>%
  group_by(Year, Month) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC >= 10)

# + # of logs at max temp

#Matt's Stuff
minmaxtemp <- transform(Temps, min=ave(Temp_C, strftime(DateTime, '%F'), FUN=min),
                        max=ave(Temp_C, strftime(DateTime, '%F'), FUN=max))

minmaxtemp1 <- minmaxtemp %>%
  select(Month, Year, min, max) %>%
  unique()

minmaxtemp$tempdiff <- minmaxtemp$max - minmaxtemp$min

#Sara's stuff
minmaxtemp <- Temps %>%
  group_by(Month, Year) %>%
  summarise(
    min = min(Temp_C),
    max = max(Temp_C)
  )
  

#sept2018 <- subset(minmaxtemp, minmaxtemp$Month == "September" & minmaxtemp$Year == 2018)

Max2018 <- subset(minmaxtemp, minmaxtemp$Year == 2018)
sum(unique(Max2018$max) > 10, na.rm=TRUE)

Max2018 <- subset(minmaxtemp, minmaxtemp$Year == 2018)
sum(unique(Max2018$max) > 15, na.rm=TRUE)

Max2018 <- subset(minmaxtemp, minmaxtemp$Year == 2018)
sum(unique(Max2018$max) > 20, na.rm=TRUE)

Max2019 <- subset(minmaxtemp, minmaxtemp$Year == 2019)
sum(unique(Max2019$max) > 10, na.rm=TRUE)

Max2019 <- subset(minmaxtemp, minmaxtemp$Year == 2019)
sum(unique(Max2019$max) > 15, na.rm=TRUE)

Max2019 <- subset(minmaxtemp, minmaxtemp$Year == 2019)
sum(unique(Max2019$max) > 20, na.rm=TRUE)

Max2020 <- subset(minmaxtemp, minmaxtemp$Year == 2020)
sum(unique(Max2020$max) > 10, na.rm=TRUE)

Max2020 <- subset(minmaxtemp, minmaxtemp$Year == 2020)
sum(unique(Max2020$max) > 15, na.rm=TRUE)

Max2020 <- subset(minmaxtemp, minmaxtemp$Year == 2020)
sum(unique(Max2020$max) > 20, na.rm=TRUE)

Max2021 <- subset(minmaxtemp, minmaxtemp$Year == 2021)
sum(unique(Max2021$max) > 10, na.rm=TRUE)

Max2021 <- subset(minmaxtemp, minmaxtemp$Year == 2021)
sum(unique(Max2021$max) > 15, na.rm=TRUE)

Max2021 <- subset(minmaxtemp, minmaxtemp$Year == 2021)
sum(unique(Max2021$max) > 20, na.rm=TRUE)

#The Next factor is going to be degree days for certain periods of time each year.The Higher the degree days, you can get without thermal stress, the more development and rate of development you can get.
#Lets start with Brook Trouts spawning period which is September to October. 

#Calculate average degree days for each year for September and October. 

#Messing around with degree days bellow 

#Never ended up using degree days so comment out this section of code!!!!!

#base_temperature <- c(0)

#Painter_ST <- Painter_ST %>%
#  mutate(Date = as.Date(DateTime),  # Convert DateTime to Date
#         DegreeDays = pmax(0, Temp_C - base_temperature)) %>%
# group_by(Year) %>%
#  summarize(TotalDegreeDays = sum(DegreeDays))


#Painter_ST <- Painter_ST %>%
#  mutate(Date = as.Date(DateTime),
#         Month = format(Date, "%m"),
#         DegreeDays = pmax(0, Temp_C - base_temperature)) %>%
#  filter(Month %in% c("09", "10")) %>%
#  group_by(Year) %>%
#  summarize(TotalDegreeDays = sum(DegreeDays))

#Painter_ST <- Painter_ST %>%
#  mutate(Date = as.Date(DateTime),
#         Month = format(Date, "%m"),
#         DegreeDays = pmax(0, Temp_C - base_temperature)) %>%
#  filter(Month %in% c("09", "10")) %>%
#  group_by(Year) %>%
#  summarize(
#    TotalDegreeDays = sum(DegreeDays),
#    NumDaysBetweenSepOct = n(),  # Number of days between September and October
#    AvgTemperature = mean(Temp_C)  # Average temperature for those two months
#  )

#The Third factor will be the highest and lowest stream temperature of each month to see what the extremes are.

Painter_Monthly_Extremes <- Painter_ST %>%
  group_by(Year, Month) %>%
  summarise(
    Highest_Temperature_C = max(Temp_C, na.rm = TRUE),
    Lowest_Temperature_C = min(Temp_C, na.rm = TRUE)
  )         

#DON'T USE FOURTH FACTOR, USE FIFTH FACTOR INSTEAD
#Fourth Factor is minimum and maximum average temperature for the month based on average temperature per day

#Painter_Monthly_Extremes_Avg <- Painter_ST %>%
#  group_by(DateTime) %>%
#  summarise(
#    Avg_Temp = mean(Temp_C, na.rm = TRUE)
#    )        

#Painter_Monthly_Extremes_Avg2 <- Painter_Monthly_Extremes_Avg %>%
#  separate(DateTime, into = c("Year", "Month", "Day"), sep = "-" )
  
#Painter_Monthly_Extremes_Avg3 <- Painter_Monthly_Extremes_Avg2 %>%
#  group_by(Year, Month) %>%
#  summarise(min=min(Avg_Temp), max=max(Avg_Temp))

#Fifth Factor is minimum and maximum temperature for the day computed/calculated to the average per month

Painter_Monthly_Extremes_MinMax <- Painter_ST %>%
  group_by(DateTime) %>%
  summarise(
    min_temp = min(Temp_C, na.rm = TRUE), max_temp = max(Temp_C, na.rm = TRUE)
  )        

Painter_Monthly_Extremes_MinMax2 <- Painter_Monthly_Extremes_MinMax %>%
  separate(DateTime, into = c("Year", "Month", "Day"), sep = "-" )

Painter_Monthly_Extremes_MinMax3 <- Painter_Monthly_Extremes_MinMax2 %>%
  group_by(Year, Month) %>%
  summarise(AverageMin=mean(min_temp), AverageMax=mean(max_temp))

######
#Don't think I need this code for days over 20C, but don't want to get rid of it yet just in case 

#lubrudate package is already installed, so need to group by year and mutate a new column to show the days over 20C fro Stream Temp 

Painter_ST <- Painter_ST %>%
  group_by(Year) %>%
  mutate(Days_Over_20C = sum(Temp_C >= 20))

#Lets do the same ting but for days greater then or equal to 24 degrees Celsius

Painter_ST <- Painter_ST %>%
  group_by(Year) %>%
  mutate(Days_Over_24C = sum(Temp_C >= 24))
