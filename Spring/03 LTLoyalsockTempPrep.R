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
CoalT<-read_csv("Coal.Loyalsock_Stream_QC.csv")
ConkT<-read_csv("Conklin.Mill_Stream_QC.csv")
DryHBT<-read_csv("Dry.Hoagland_Stream_QC.csv")
DryLST<-read_csv("Dry.Loyalsock_Stream_QC.csv")
DutcT<-read_csv("Dutchman.Loyalsock_Stream_QC.csv")
ElliT<-read_csv("Ellis.Loyalsock_Stream_QC.csv")
FallT<-read_csv("Fall.Hoagland_Stream_QC.csv")
FlagT<-read_csv("FlagMarsh.Pigeon_Stream_QC.csv")
GranT<-read_csv("Grandad.Hessler_Stream_QC.csv")
#HuckT<-read_csv("") #No logger retrieved
LakeT<-read_csv("Lake.Elk_Stream_QC.csv")
LeveT<-read_csv("Level.Lick_Stream_QC.csv")
#LickT<-read_csv("") #No logger retrieved
MillT<-read_csv("Mill.Loyalsock_Stream_QC.csv")
PainT<-read_csv("Painter.LittleBear_Stream_QC.csv")
PortT<-read_csv("Porter.Hoagland_Stream_QC.csv")
RedT<-read_csv("")
RockT<-read_csv("")
SandT<-read_csv("")
SaSpT<-read_csv("")
#ScarT<-read_csv("") #No logger retrieved
SherT<-read_csv("")
ShinT<-read_csv("")
SnakT<-read_csv("")
StreT<-read_csv("")
SwamT<-read_csv("")
#WeedT<-read_csv("") #No logger retrieved
YellT<-read_csv("")
######################
#Bind row so that all stream temperature data is in one tibble
StreamTemp<-rbind(BearT,BrunT,CoalT,ConkT,DryHBT,DryLST,DutcT,ElliT,FallT,FlagT,MillT,PainT,PortT,LeveT,
                  GranT,LakeT)
#                 HuckT,LickT,RedT,RockT,SandT,SaSpT,
#                  ScarT,SherT,ShinT,SnakT,StreT,SwamT,WeedT,YellT)
StreamTemp
#####################
#Separate out Year-Month-Day-Time so that we can create different predictors.
#STemp<-StreamTemp%>%
#  separate(DateTime, into=c("Date","Time"),sep=" ",remove=FALSE)%>%
#  separate(Date, into=c("Year","Month","Day"),sep="-",remove=FALSE)%>%
#  mutate_at(vars(Date),date)%>%
#  mutate_at(vars(Year,Month,Day),factor)
#STemp
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
#Join Monthly_Extremes and Monthly_AvgMinMax together
STPred<-left_join(Monthly_Extremes,Monthly_AvgMinMax, by = c("SiteCode","Year","Month"))
STPred #266 x 7

write.csv(STPred,"STPred.csv")
