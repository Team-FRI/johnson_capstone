#Long-term (LT) Loyalsock (L) Surveys
#Logger Data Prep for Loops
#Created by Sara Ashcraft 2/18/2024
#Modified by Sara Ashcraft 3/26/24 - Adding files and working with predictor code.

#Load Library
library(tidyverse)
library(lubridate)

#Set working Directory
setwd("C:/GitHub/johnson_capstone/spring")

#Load data - Stream Temp QC only
BearT<-read_csv("Bear.Loyalsock_Stream_QC.csv")
BrunT<-read_csv("Brunnerdale.Ogdonia_Stream_QC.csv")
#CoalT<-read_csv("Coal.Loyalsock_Stream_QC.csv") #Data is too unusual
ConkT<-read_csv("Conklin.Mill_Stream_QC.csv")
DryHBT<-read_csv("Dry.Hoagland_Stream_QC.csv")
DryLST<-read_csv("Dry.Loyalsock_Stream_QC.csv")
DutcT<-read_csv("Dutchman.Loyalsock_Stream_20202021_QC.csv")
ElliT<-read_csv("Ellis.Loyalsock_Stream_QC.csv")
FallT<-read_csv("Fall.Hoagland_Stream_20202021QC.csv")
FlagT<-read_csv("FlagMarsh.Pigeon_Stream_20202021QC.csv")
GranT<-read_csv("Grandad.Hessler_Stream_QC.csv")
####HuckT<-read_csv("") #No logger retrieved
#LakeT<-read_csv("Lake.Elk_Stream_QC.csv")
LeveT<-read_csv("Level.Lick_Stream_QC.csv")
####LickT<-read_csv("") #No logger retrieved
#MillT<-read_csv("Mill.Loyalsock_Stream_QC.csv")
PainT<-read_csv("Painter.LittleBear_Stream_20202021_QC.csv")
#PortT<-read_csv("Porter.Hoagland_Stream_QC.csv")
RedT<-read_csv("Red.LittleBear_Stream_QC.csv")
#RockT<-read_csv("Rock.LittleLoyalsock_Stream_QC.csv")
SandT<-read_csv("Sand.Mill_Stream_QC.csv")
SaSpT<-read_csv("SandSpring.LittleBear_Stream_QC.csv")
####ScarT<-read_csv("") #No logger retrieved
SherT<-read_csv("Sherman.Loyalsock_Stream_QC.csv")
ShinT<-read_csv("Shingle.Bear_Stream_QCSR.csv")
#SnakT<-read_csv("Snake.Bear_Stream_QC.csv")
StreT<-read_csv("Streby.Lick_Stream_QC.csv")
SwamT<-read_csv("Swamp.Hoagland_Stream_QC.csv")
####WeedT<-read_csv("") #No logger retrieved
YellT<-read_csv("Yellow.LittleLoyalsock_Stream_QC.csv")
######################
#Bind row so that all stream temperature data is in one tibble
StreamTemp<-rbind(BearT,BrunT,ConkT,DryHBT,DryLST,DutcT,ElliT,FallT,FlagT,GranT,
                  LeveT,PainT,RedT,SandT,SaSpT,StreT,SherT,ShinT,SwamT,YellT)
#   CoalT,LakeT,MillT,PortT,
#   RockT,SnakT)
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
#     only 56 dates out of 7860 for the sites included that are not full.
QCTempY<-ST %>%
  group_by(SiteCode,Year)%>%
  summarize(Count=n())%>%
  arrange(Year)
QCTempY
#     Full year's worth of data ~ 35,040 logs
#     Site-Year with most logs: Dry.Hoagland-2017; Brunnerdale-2021; Sherman-2021; 
#                               Ellis-2021; Dutchman-2021; SandSpring-2021;
QCTempM<-ST %>%
  group_by(SiteCode,Year,Month)%>%
  summarize(Count=n())%>%
  arrange(Month)
QCTempM
#      51 incomplete months out of 289
###############################################################################
#Create Min/Max reading for each Month-Year.
Monthly_Extremes <- ST %>%
  group_by(SiteCode,Year, Month) %>%
  summarise(
    Highest_Temperature_C = max(Temp_C),
    Lowest_Temperature_C = min(Temp_C)
  )       
Monthly_Extremes # 289 x 5
###############################################################################
#Create Avg Min/Max reading for each Month-Year
Daily_MinMax <- ST %>%
  group_by(SiteCode,Date) %>%
  summarise(
    DailyMin_tempC = min(Temp_C), 
    DailyMax_tempC = max(Temp_C)
  ) 
Daily_MinMax #7860 x4
YM<- Daily_MinMax %>% #Like this code better
  mutate(
    Date = format(as.Date(Date),"%Y-%m-%d"),
    Year = format(as.Date(Date),"%Y"),
    Month = format(as.Date(Date),"%m")
  )%>%
  mutate_at(vars(Date),date)%>%
  mutate_at(vars(Year,Month),factor)
YM
#
Monthly_AvgMinMax <- YM %>%
  group_by(SiteCode,Year, Month) %>%
  summarise(
    AvgMin=mean(DailyMin_tempC), 
    AvgMax=mean(DailyMax_tempC))
Monthly_AvgMinMax # 289 x 5
###############################################################################
###############################################################################

#Join Monthly_Extremes and Monthly_AvgMinMax together
STPred<-left_join(Monthly_Extremes,Monthly_AvgMinMax, by = c("SiteCode","Year","Month"))
STPred #289 x 7
#write.csv(STPred,"STPred.csv")
#   looks good
###############################################################################
########Mess below
#Number of logs per Month Year, 
############################For 3 degrees (Alevin: Lethal)
#      day = Midnight - 11:59pm;  <3 degrees c
TempsL3MY <- ST %>%
  filter(Temp_C < 3) %>%
  group_by(SiteCode, Year, Month, .drop=F) %>%
  summarise(numberoflogs_MYL3 = n()) 
TempsL3MY #37 x4
#  Number of logs per Month Year less than 3 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountL3<-left_join(QCTempM,TempsL3MY, by = c("SiteCode","Year","Month"))
LogCountL3 #216 x 5
PropLogL3<-LogCountL3%>%
  mutate(
    PropLogL3 = numberoflogs_MYL3/Count
  )
PropLogL3 #289 X 6   - Will need to add to STPred

# Avg Year-Month Temp
Avg_YM_Temp <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C))
Avg_YM_Temp #289 x 4   - Will need to add to STPred

#Sites with Month-Year AvgYMTemp < 3 degrees C. Calculate the avg temp per month, 
#   which months have an average less than 3 degrees
Avg_YM_TempMYL3 <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C)) %>%
  filter(Avg_YM_TempC < 3)
Avg_YM_TempMYL3 #35 x 4

#YM min temp < 3 degrees C. Calculate the min temp per month, which months 
#   have a minimum temp less than 3 degrees
MinTemp_MYL3 <- ST %>%
  group_by(SiteCode, Year, Month) %>%
  summarize(
    YM_MinTempC = min(Temp_C)) %>%
  filter(YM_MinTempC < 3)

#For 5 degrees (Alevin and Eggs: Sub Lethal)

#Midnight - 11:59pm <5 degrees c

TempsL5MY <- ST %>%
  filter(Temp_C < 5) %>%
  group_by(SiteCode, Year, Month, .drop=F) %>%
  summarise(numberoflogs_MYL5 = n())
TempsL5MY
#  Number of logs per Month Year less than 5 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountL5<-left_join(QCTempM,TempsL5MY, by = c("SiteCode","Year","Month"))
LogCountL5 #216 x 5
PropLogL5<-LogCountL5%>%
  mutate(
    PropLogL5 = numberoflogs_MYL5/Count
  )
PropLogL5 #289 X 6   - Will need to add to STPred


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
  group_by(SiteCode, Year, Month,.drop=F) %>%
  summarise(numberoflogs_MYL4.5 = n()) 
TempsL4.5MY
#  Number of logs per Month Year less than 4.5 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountL4.5<-left_join(QCTempM,TempsL4.5MY, by = c("SiteCode","Year","Month"))
LogCountL4.5 #216 x 5
PropLogL4.5<-LogCountL4.5%>%
  mutate(
    PropLogL4.5 = numberoflogs_MYL4.5/Count
  )
PropLogL4.5 #289 X 6   - Will need to add to STPred

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
  group_by(SiteCode, Year, Month, .drop=FALSE) %>%
  summarise(numberoflogs_MYG20 = n()) 
TempsG20MY
#  Number of logs per Month Year greater than 20 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountG20<-left_join(QCTempM,TempsG20MY, by = c("SiteCode","Year","Month"))
LogCountG20 #216 x 5
LogCountG20[is.na(LogCountG20)]<-0
PropLogG20<-LogCountG20%>%
  mutate(
    PropLogG20 = numberoflogs_MYG20/Count
  )
PropLogG20 #289 X 6   - Will need to add to STPred

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
  group_by(SiteCode, Year, Month, .drop=F) %>%
  summarise(numberoflogs_MYG24 = n()) 
TempsG24MY
#  Number of logs per Month Year greater than 24 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountG24<-left_join(QCTempM,TempsG24MY, by = c("SiteCode","Year","Month"))
LogCountG24 #216 x 5
LogCountG24[is.na(LogCountG24)]<-0
PropLogG24<-LogCountG24%>%
  mutate(
    PropLogG24 = numberoflogs_MYG24/Count
  )
PropLogG24 #289 X 6   - Will need to add to STPred

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

############################################################################################
#Join dataframes together
STPred2<-left_join(STPred,Avg_YM_Temp, by = c("SiteCode","Year","Month"))
STPred2 #289 x 8
STPred3<-left_join(STPred2,PropLogL3, by = c("SiteCode","Year","Month"))
STPred3 #289 x 11
STPred3<-STPred3%>%
  select(-Count,-numberoflogs_MYL3)
STPred4<-left_join(STPred3,PropLogL4.5 , by = c("SiteCode","Year","Month"))
STPred4 #289 x 12
STPred4<-STPred4%>%
  select(-Count,-numberoflogs_MYL4.5)
STPred5<-left_join(STPred4,PropLogL5 , by = c("SiteCode","Year","Month"))
STPred5 #289 x 13
STPred5<-STPred5%>%
  select(-Count,-numberoflogs_MYL5)
STPred6<-left_join(STPred5,PropLogG20 , by = c("SiteCode","Year","Month"))
STPred6 #289 x 14
STPred6<-STPred6%>%
  select(-Count,-numberoflogs_MYG20)
STPred7<-left_join(STPred6,PropLogG24, by = c("SiteCode","Year","Month"))
STPred7 #289 x 15
STPred7<-STPred7%>%
  select(-Count,-numberoflogs_MYG24)

#write.csv(STPred7,"STPred.csv")
#   looks good
