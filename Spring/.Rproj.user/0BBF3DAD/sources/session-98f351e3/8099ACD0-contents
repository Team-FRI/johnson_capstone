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

#####################################
#Creating Life History Columns 

LH <- ST %>%
  mutate(Eggs = between(Date, "2020-11-01", "2021-03-31"))

LH <- ST %>%
  summarise(Eggs = between(Date, "2020-11-01", "2021-03-31"))

LH <- ST %>%
  mutate(
    Eggs = filter(Date > ymd(2020-11-01) &
                    Date < ymd(2021-03-31)))

LH <- ST %>%
  summarise(
    Eggs = filter(Date > ymd(2020-11-01) &
                    Date < ymd(2021-03-31)))

LH <- ST %>%
  mutate(
    Eggs = between(Date, "2020-11-01", "2021-03-31"))

LHtable <- tibble::tribble(~ stage, ~ StartDate, ~ EndDate,
                           "Eggs", "2020-11-01", "2021-03-31")

ST2 <- ST %>%
  left_join(LHtable) %>%
  group_by()

#Bellow Worked 
Eggs <- ST %>%
  filter(Date > ymd("2020-11-01") &
           Date < ymd("2021-03-31"))

Alevin2020 <- ST %>%
  filter(Date > ymd("2020-02-01") &
           Date < ymd("2020-04-30")) 

#Bellow did not work

Alevin2021 <- ST %>%
  filter(Date < ymd("2021-02-01") & 
            Date < ymd("2021-04-30"))

EmergFry2020 <- ST %>%
  filter(Date < ymd("2020-03-01") & 
            Date < ymd("2020-04-30"))

EmergFry2021 <- ST %>%
  filter(Date < ymd("2021-03-01") & 
            Date < ymd("2021-04-30"))

YOY2020 <- ST %>%
  filter(Date < ymd("2020-05-01") & 
            Date < ymd("2020-10-31"))

YOY2021 <- ST %>%
  filter(Date < ymd("2021-05-01") & 
            Date < ymd("2021-10-31"))

Ad_Spawning2020 <- ST %>% 
  filter (Date < ymd("2020-09-15") &
            Date < ymd("2020-11-10"))
          
Ad_Spawning2021 <- ST %>% 
  filter (Date < ymd("2021-09-15") &
            Date < ymd("2021-11-10"))

Fri 2020

Fri 2021

###############################################################################
#Create Min/Max reading for Eggs!
Egg_Extremes <- Eggs %>%
  group_by(SiteCode) %>%
  summarise(
    Highest_Temperature_Eggs_Period = max(Temp_C),
    Lowest_Temperature_Eggs_Period = min(Temp_C)
  )       
Monthly_Extremes # 266 x 5
###############################################################################
#Create Avg Daily Min/Max reading for Eggs
Eggs_MinMax <- Eggs %>%
  group_by(SiteCode, Date) %>%
  summarise(
    Min_temp_Eggs_Period = min(Temp_C), 
    Max_temp_Eggs_Period = max(Temp_C)
  ) 

Eggs_AvgMinMax <- Eggs_MinMax %>%
  group_by(SiteCode) %>%
  summarise(
    AvgMin_Egg_Period=mean(Min_temp_Eggs_Period), 
    AvgMax_Egg_Period=mean(Max_temp_Eggs_Period))
###############################################################################
###############################################################################
Alevin_Extremes <- Alevin2020 & Alevin2021 %>%
  group_by(SiteCode) %>%
  summarise(
    Highest_Temperature_Eggs_Period = max(Temp_C),
    Lowest_Temperature_Eggs_Period = min(Temp_C)
  )       
Monthly_Extremes # 266 x 5
###############################################################################
#Create Avg Daily Min/Max reading for Alevin
Alevin_MinMax <- Alevin2020 %>%
  group_by(SiteCode, Date) %>%
  summarise(
    Min_temp_Eggs_Period = min(Temp_C), 
    Max_temp_Eggs_Period = max(Temp_C)
  ) 

Eggs_AvgMinMax <- Eggs_MinMax %>%
  group_by(SiteCode) %>%
  summarise(
    AvgMin_Egg_Period=mean(Min_temp_Eggs_Period), 
    AvgMax_Egg_Period=mean(Max_temp_Eggs_Period))
###############################################################################


#Number of logs per Month Year 

#For 3 degrees (Alevin: Lethal)

#Midnight - 11:59pm <3 degrees c

TempsL3MY <- Eggs %>%
  filter(Temp_C < 3) %>%
  group_by(SiteCode) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp < 3 degres C

Avg_YM_TempMYL3 <- Eggs %>%
  group_by(SiteCode) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC < 3)

#YM max temp < 3 degrees C 

MaxTemp_MYL3 <- Eggs %>%
  group_by(SiteCode) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC < 3)

#For 5 degrees (Alevin and Eggs: Sub Lethal)

#Midnight - 11:59pm <5 degrees c

TempsL5MY <- Eggs %>%
  filter(Temp_C < 5) %>%
  group_by(SiteCode) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp < 5 degres C

Avg_YM_TempMYL5 <- Eggs %>%
  group_by(SiteCode) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC < 5)

#YM max temp < 5 degrees C 

MaxTemp_MYL5 <- Eggs %>%
  group_by(SiteCode) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC < 5)

#Alevin Sub Lethal Replace with Alvein

TempsL5MY <- Eggs %>%
  filter(Temp_C < 5) %>%
  group_by(SiteCode) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp < 5 degres C

Avg_YM_TempMYL5 <- Eggs %>%
  group_by(SiteCode) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC < 5)

#YM max temp < 5 degrees C 

MaxTemp_MYL5 <- Eggs %>%
  group_by(SiteCode) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC < 5)

#For 4.5 degrees (Eggs: Lethal)

#Midnight - 11:59pm <4.5 degrees c

TempsL4.5MY <- ST %>%
  filter(Temp_C < 4.5) %>%
  group_by(SiteCode) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp < 4.5 degres C

Avg_YM_TempMYL4.5 <- ST %>%
  group_by(SiteCode) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC < 4.5)

#YM max temp < 4.5 degrees C 

MaxTemp_MYL4.5 <- ST %>%
  group_by(SiteCode) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC < 4.5)

#Already did For 5 degrees (Eggs: Sub Lethal) so move on!

#For 20 degrees (Adults: Sub Lethal)

#Midnight - 11:59pm >20 degrees c

TempsG20MY <- ST %>%
  filter(Temp_C > 20) %>%
  group_by(SiteCode) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp > 20 degres C

Avg_YM_TempMYL3 <- ST %>%
  group_by(SiteCode) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC > 20)

#YM max temp < 20 degrees C 

MaxTemp_MYL3 <- ST %>%
  group_by(SiteCode) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC >20)

#For 24 degrees (Adults: Lethal)

#Midnight - 11:59pm >24 degrees c

TempsG24MY <- ST %>%
  filter(Temp_C > 24) %>%
  group_by(SiteCode) %>%
  summarise(numberoflogs_MYG10 = n()) 

#AvgYMTemp > 24 degres C

Avg_YM_TempMYL3 <- ST %>%
  group_by(SiteCode) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC > 24)

#YM max temp < 24 degrees C 

MaxTemp_MYL3 <- ST %>%
  group_by(SiteCode) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC >24)

###################################
#Number of logs per Month Year 

#Midnight - 11:59pm >10 degrees c

TempsG10MY <- ST %>%
  filter(Temp_C >= 10) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n())

#ASL Stands for Adult Sub lethal 

TempsG20MYASL <- ST %>%
  filter(Temp_C >= 20) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 

TempsG24MYAL <- ST %>%
  filter(Temp_C >= 24) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 

TempsG24MYAlevinL <- ST %>%
  filter(Temp_C >= 24) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 

TempsG24MYAL <- ST %>%
  filter(Temp_C >= 24) %>%
  group_by(SiteCode, Year, Month) %>%
  summarise(numberoflogs_MYG10 = n()) 


#AvgYMTemp > 10 degres C

Avg_YM_TempMY <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC >= 10)

#YM max temp > 10 degrees C 

MaxTemp_MY <- ST %>%
  group_by(SiteCode, Year, Month) %>%
  summarize(
    YM_MaxTempC = max(Temp_C)) %>%
  filter(YM_MaxTempC >= 10)

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
