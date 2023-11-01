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
  summarise(AvgYearMonthTemp = mean(Temp_C), sd = sd(Temp_C))

Painter_Land_Summary <- Painter_LT %>%
  select(YearMonth, Temp_C) %>%
  group_by(YearMonth) %>%
  arrange(YearMonth) %>%
  summarise(AvgYearMonthTemp = mean(Temp_C), sd = sd(Temp_C))
            
            
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


  
#Make Stream Temperature and Land Temperature all one table so can make graph with two data sets
#For Land (now called air) Temp
  
Painter_Land_Summary <- Painter_Land_Summary %>%
  mutate(Type = "Air")

#For Stream (now called water) Temp
Painter_Stream_Summary <- Painter_Stream_Summary %>%
  mutate(Type = "Water")

Painter_Master_Temp <- bind_rows(Painter_Land_Summary, Painter_Stream_Summary)

#Convert Painter_Matser_Temp to YM instead of YMD to avoid "Can't convert `x` <datetime<UTC>> to <double>" error/argument in plot your about to make. 
Summary_Plot_BRTData$Date <- format(as.Date(Summary_Plot_BRTData$Date), "%Y-%m")
summary(Summary_Plot_BRTData)  

#Now Make plot
install.packages("grDevices")
library("grDevices")

x11(height = 25, width = 25)

ggplot() +
  geom_bar(Painter_Master_Temp, mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type), stat = "identity", position = "dodge") +
  geom_errorbar(Painter_Master_Temp, mapping = aes(x = YearMonth, ymin = AvgYearMonthTemp-sd, ymax = AvgYearMonthTemp+sd, fill = Type), position = position_dodge(0.9)) +
  geom_point(Summary_Plot_BRTData, mapping = aes(x = Date, y = Count, colour = "Brook Trout"), shape =23, size = 3.5, fill = "purple", position = "dodge") +
  scale_y_continuous(name = "Average Monthly Temperature (Celsius)", sec.axis = sec_axis(~. ,name = "Number of Brook Trout")) + 
  ggtitle("Brook Trout Populations as Temperatures change at Painter Run")

#Border color of points in R argument 

#Messing with stuff bellow 

ggplot() +
  geom_bar(subset(Painter_Master_Temp, Type %in% "Air"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type), stat = "identity", position = "dodge") +
  facet_grid(~YearMonth)
  geom_bar(subset(Painter_Master_Temp, Type %in% "Water"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type), stat = "identity", position = "dodge") +
  geom_bar(Summary_Plot_BRTData, mapping = aes(x = Date, y = Count, fill = "Brook Trout"), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Monthly Temperature", sec.axis = sec_axis(~. ,name = "Number of Brook Trout")) + 
  ggtitle("Brook Trout Populations as Temperatures change at Painter Run")

ggplot() +
  geom_bar(subset(Painter_Master_Temp, Type %in% "Air"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type), stat = "identity", position = "dodge")
  geom_bar(subset(Painter_Master_Temp, Type %in% "Water"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type), stat = "identity", position = "dodge") +
  geom_bar(Summary_Plot_BRTData, mapping = aes(x = Date, y = Count, fill = "Brook Trout"), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Monthly Temperature", sec.axis = sec_axis(~. ,name = "Number of Brook Trout")) +
  ggtitle("Brook Trout Populations as Temperatures change at Painter Run")

ggplot() +
  geom_bar(subset(Painter_Master_Temp, Type %in% "Air"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type), stat = "summary", fun = "mean", position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se)
  geom_bar(subset(Painter_Master_Temp, Type %in% "Water"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type), stat = "summary", fun = "mean", position = "dodge") +
  geom_bar(Summary_Plot_BRTData, mapping = aes(x = Date, y = Count, fill = "Brook Trout"), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Average Monthly Temperature", sec.axis = sec_axis(~. ,name = "Number of Brook Trout")) + 
  ggtitle("Brook Trout Populations as Temperatures change at Painter Run")


  
ggplot() + 
  geom_bar(subset(data = Painter_Master_Temp, Type %in% "Air"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp), stat = "identity", position = "dodge")  + 
  geom_bar(subset(data = Painter_Master_Temp, Type %in% "Water"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp), stat = "identity", position = "dodge") +
  geom_bar(Summary_Plot_BRTData, mapping = aes(x = Date, y = Count), stat = "identity", position = "dodge", fill = "pink") + 
  scale_y_continuous(name = "Average Monthly Temperature", sec.axis = sec_axis(~. ,name = "Number of Brook Trout")) +
  ylim(-5,30) +
  ggtitle("Brook Trout Populations as Temperatures change at Little Painter Run")
 
ggplot(subset(Painter_Master_Temp, Type %in% "Air"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggplot(subset(Painter_Master_Temp, Type %in% "Water"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type)) +
         geom_bar(stat = "identity", position = "dodge")
         
ggplot() + geom_bar(subset(Painter_Master_Temp, Type %in% "Air"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type) stat = "identity", position = "dodge") 
  geom_bar(subset(Painter_Master_Temp, Type %in% "Water"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp), stat = "identity", position = position_dodge(width = 0.50), fill = "blue") +
  geom_bar(Summary_Plot_BRTData, mapping = aes(x = Date, y = Count), stat = "identity", position = "dodge", fill = "pink") + 
  scale_y_continuous(name = "Average Monthly Temperature", sec.axis = sec_axis(~. ,name = "Number of Brook Trout")) +
  ylim(-5,30) +
  ggtitle("Brook Trout Populations as Temperatures change at Little Painter Run")
         
  geom_bar(subset(Painter_Master_Temp, Type %in% "Water"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp, fill = Type), stat = "identity", position = "dodge2") +
  geom_bar(Summary_Plot_BRTData, mapping = aes(x = Date, y = Count), stat = "identity", position = "dodge", fill = "pink") + 
  scale_y_continuous(name = "Average Monthly Temperature", sec.axis = sec_axis(~. ,name = "Number of Brook Trout")) +
  ylim(-5,30) +
  ggtitle("Brook Trout Populations as Temperatures change at Painter Run")


#Messing with stuff bellow 

ylim.prim <- c(-4, 50)
ylim.sec <- c(5, 50)

###

Painter_Master_Temp %>%
  filter(Type %in% ("Air")) %>%
  (geom_smooth(mapping = aes(x = Type, y = AvgYearMonthTemp)))


ggplot(subset(Painter_Master_Temp, Type %in% "Air"), mapping = aes(x = YearMonth, y = AvgYearMonthTemp)) +
  geom_bar(stat = "identity", fill = "brown")  

Painter_LT$YearMonth <- format(as.Date(Painter_LT$DateTime), "%Y-%m")
summary(Painter_LT)


