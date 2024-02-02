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



