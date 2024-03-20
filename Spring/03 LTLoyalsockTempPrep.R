#Long-term (LT) Loyalsock (L) Surveys
#Logger Data Prep for Loops
#Created by Sara Ashcraft 2/18/2024

#Load Library
library(tidyverse)
library(lubridate)

#Set working Directory
setwd("C:/GitHub/johnson_capstone/spring")

#Load data - Stream Temp QC only
BearT<-read_csv("Bear.Loyalsock_Stream_QC.csv")
BrunT<-read_csv("Brunnerdale.Ogdonia_Stream_QC.csv")
#CoalT<-read_csv("Coal.Loyalsock_Stream_QC.csv")
#ConkT<-read_csv("Conklin.Mill_Stream_QC.csv")
#DryHBT<-read_csv("Dry.Hoagland_Stream_QC.csv")
#DryLST<-read_csv("Dry.Loyalsock_Stream_QC.csv")
DutcT<-read_csv("Dutchman.Loyalsock_Stream_20202021_QC.csv")
#ElliT<-read_csv("Ellis.Loyalsock_Stream_QC.csv")
#FallT<-read_csv("Fall.Hoagland_Stream_QC.csv")
#FlagT<-read_csv("FlagMarsh.Pigeon_Stream_QC.csv")
GranT<-read_csv("Grandad.Hessler_Stream_QC.csv")
####HuckT<-read_csv("") #No logger retrieved
#LakeT<-read_csv("Lake.Elk_Stream_QC.csv")
#LeveT<-read_csv("Level.Lick_Stream_QC.csv")
####LickT<-read_csv("") #No logger retrieved
#MillT<-read_csv("Mill.Loyalsock_Stream_QC.csv")
PainT<-read_csv("Painter.LittleBear_Stream_20202021_QC.csv")
#PortT<-read_csv("Porter.Hoagland_Stream_QC.csv")
#RedT<-read_csv("Red.LittleBear_Stream_QC.csv")
#RockT<-read_csv("Rock.LittleLoyalsock_Stream_QC.csv")
SandT<-read_csv("Sand.Mill_Stream_QC.csv")
SaSpT<-read_csv("SandSpring.LittleBear_Stream_QC.csv")
####ScarT<-read_csv("") #No logger retrieved
#SherT<-read_csv("Sherman.Loyalsock_Stream_QC.csv")
#ShinT<-read_csv("Shingle.Bear_Stream_QC.csv")
#SnakT<-read_csv("Snake.Bear_Stream_QC.csv")
#StreT<-read_csv("Streby.Lick_Stream_QC.csv")
#SwamT<-read_csv("Swamp.Hoagland_Stream_QC.csv")
####WeedT<-read_csv("") #No logger retrieved
#YellT<-read_csv("Yellow.LittleLoyalsock_Stream_QC.csv")
######################
#Bind row so that all stream temperature data is in one tibble
StreamTemp<-rbind(BearT,BrunT,DutcT,GranT,PainT,SandT,SaSpT)
#   CoalT,ConkT,DryHBT,DryLST,   ElliT,FallT,FlagT,  LakeT,LeveT,MillT,PortT,
#   RedT,RockT,SherT,ShinT,SnakT,StreT,SwamT,YellT)
StreamTemp
#####################
#Separate out Year-Month-Day-Time so that we can create different predictors.
ST<- StreamTemp %>% #Like this code better
  mutate(
    Date = format(as.Date(DateTime),"%Y-%m-%d"),
    Year = format(as.Date(DateTime),"%Y"),
    Month = format(as.Date(DateTime),"%m"),
    Day = format(as.Date(DateTime),"%d")
    )%>%
  mutate_at(vars(Date),date)%>%
  mutate_at(vars(Year,Month,Day),factor)
ST
#QC - Date,Year,Month
QCTempDate<-ST %>%
  group_by(SiteCode,Date)%>%
  summarize(Count=n())%>%
  arrange(Date)
QCTempDate
QCTempY<-ST %>%
  group_by(SiteCode,Year)%>%
  summarize(Count=n())%>%
  arrange(Year)
QCTempY
QCTempM<-ST %>%
  group_by(SiteCode,Month)%>%
  summarize(Count=n())%>%
  arrange(Month)
QCTempM

###############################################################################
#Create Min/Max reading for each Month-Year.
Monthly_Extremes <- ST %>%
  group_by(SiteCode,Year, Month) %>%
  summarise(
    Highest_Temperature_C = max(Temp_C),
    Lowest_Temperature_C = min(Temp_C)
  )       
Monthly_Extremes # 266 x 5
###############################################################################
#Create Avg Min/Max reading for each Month-Year
Monthly_MinMax <- ST %>%
  group_by(SiteCode,Date) %>%
  summarise(
    Min_tempC = min(Temp_C), 
    Max_tempC = max(Temp_C)
  ) 
Monthly_MinMax
YM<- Monthly_MinMax %>% #Like this code better
  mutate(
    Date = format(as.Date(Date),"%Y-%m-%d"),
    Year = format(as.Date(Date),"%Y"),
    Month = format(as.Date(Date),"%m")
  )%>%
  mutate_at(vars(Date),date)%>%
  mutate_at(vars(Year,Month),factor)
YM
Monthly_AvgMinMax <- YM %>%
  group_by(SiteCode,Year, Month) %>%
  summarise(
    AvgMin=mean(Min_tempC), 
    AvgMax=mean(Max_tempC))
Monthly_AvgMinMax # 266 x 5
###############################################################################
#Number of logs per Month Year 

#For 3 degrees (Alevin: Lethal)

#Midnight - 11:59pm <3 degrees c

TempsL3MY <- ST %>%
  filter(Temp_C < 3) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp < 3 degres C

Avg_YM_TempMYL3 <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC < 3)

#YM max temp < 3 degrees C 

MaxTemp_MYL3 <- ST %>%
  group_by(SiteCode, Year, Month) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC < 3)

#For 5 degrees (Alevin and Eggs: Sub Lethal)

#Midnight - 11:59pm <5 degrees c

TempsL5MY <- ST %>%
  filter(Temp_C < 5) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp < 5 degres C

Avg_YM_TempMYL5 <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC < 5)

#YM max temp < 5 degrees C 

MaxTemp_MYL5 <- ST %>%
  group_by(SiteCode, Year, Month) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC < 5)

#For 4.5 degrees (Eggs: Lethal)

#Midnight - 11:59pm <4.5 degrees c

TempsL4.5MY <- ST %>%
  filter(Temp_C < 4.5) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp < 4.5 degres C

Avg_YM_TempMYL4.5 <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC < 4.5)

#YM max temp < 4.5 degrees C 

MaxTemp_MYL4.5 <- ST %>%
  group_by(SiteCode, Year, Month) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC < 4.5)

#Already did For 5 degrees (Eggs: Sub Lethal) so move on!

#For 20 degrees (Adults: Sub Lethal)

#Midnight - 11:59pm >20 degrees c

TempsG20MY <- ST %>%
  filter(Temp_C > 20) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp > 20 degres C

Avg_YM_TempMYL3 <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC > 20)

#YM max temp < 20 degrees C 

MaxTemp_MYL3 <- ST %>%
  group_by(SiteCode, Year, Month) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC >20)

#For 24 degrees (Adults: Lethal)

#Midnight - 11:59pm >24 degrees c

TempsG24MY <- ST %>%
  filter(Temp_C > 24) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp > 24 degres C

Avg_YM_TempMYL3 <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC > 24)

#YM max temp < 24 degrees C 

MaxTemp_MYL3 <- ST %>%
  group_by(SiteCode, Year, Month) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC >24)


# + # of logs at max temp
###############################################################################

#Join Monthly_Extremes and Monthly_AvgMinMax together
STPred<-left_join(Monthly_Extremes,Monthly_AvgMinMax, by = c("SiteCode","Year","Month"))
STPred #266 x 7
#Ivestigate Conklin Mill 2020-06
STPred2<-left_join(STPred, TempsG10MY, by = c("SiteCode","Year","Month"))
STPred2

#DO line 155 next!!!
write.csv(STPred2,"STPred.csv")
#REPLACE NA'S WITH 0'S FOR numberoflogs_MYG10 in STpred2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
STPred2[is.na(STPred2)] <- 0

STPred3<-left_join(STPred2, Avg_YM_TempMY, by = c("SiteCode","Year","Month"))
STPred3


STPred4<-left_join(STPred3, MaxTemp_MY, by = c("SiteCode","Year","Month"))
STPred4

write.csv(STPred4,"STPred.csv")

write.csv(STPred2, "STPred2.csv")
