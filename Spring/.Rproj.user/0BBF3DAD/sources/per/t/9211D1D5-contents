#Long-term (LT) Loyalsock (L) Surveys
#Logger Data Prep for Loops
#Created by Sara Ashcraft 2/18/2024
#Modified by Sara Ashcraft 4/1/24 - Adding files and working with predictor code.

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
  summarize(TNumLogs=n())%>%
  arrange(Month)
QCTempM
#      51 incomplete months out of 289
#Remove incomplete months
QCTempMU<-QCTempM%>%
  filter(!(TNumLogs<2688))
QCTempMU #238 x 4
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
#Join QCTempMU with Monthly_Extremes to reduce Site-Year-Month combos to complete
STPred<-left_join(QCTempMU,Monthly_Extremes, by = c("SiteCode","Year","Month"))
STPred #238 x 7
#write.csv(STPred,"STPred.csv")
#   looks good
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
#Join STPred with Monthly_AvgMinMax to reduce Site-Year-Month combos to complete
STPred2<-left_join(STPred,Monthly_AvgMinMax, by = c("SiteCode","Year","Month"))
STPred2 #238 x 8
#write.csv(STPred,"STPred.csv")
#   looks good
###############################################################################
# Avg Year-Month Temp
Avg_YM_Temp <- ST %>%
  group_by(SiteCode, Year, Month) %>% 
  summarise(
    Avg_YM_TempC = mean(Temp_C))
Avg_YM_Temp #289 x 4   - Will need to add to STPred
#Number of logs per Month Year, 
#################################################################################
STPred3<-left_join(STPred2,Avg_YM_Temp, by = c("SiteCode","Year","Month"))
STPred3 #289 x 9
STPred3<-STPred3%>%
  select(-TNumLogs)
STPred3
#################################################################################
#For 3 degrees (Alevin: Lethal)
#   day = Midnight - 11:59pm;  <3 degrees c
TempsL3MY <- ST %>%
  filter(Temp_C < 3) %>%
  group_by(SiteCode, Year, Month, .drop=F) %>%
  summarise(numberoflogs_MYL3 = n()) 
TempsL3MY #37 x4
#  Number of logs per Month Year less than 3 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountL3<-left_join(QCTempMU,TempsL3MY, by = c("SiteCode","Year","Month"))
LogCountL3 #238 x 5
PropLogL3<-LogCountL3%>%
  mutate(
    PropLogL3 = numberoflogs_MYL3/TNumLogs
  )
PropLogL3 #238 X 6   
#################################################################################
STPred4<-left_join(STPred3,PropLogL3, by = c("SiteCode","Year","Month"))
STPred4 #238 x 9
STPred4<-STPred4%>%
  select(-TNumLogs,-numberoflogs_MYL3)
STPred4
#################################################################################
#For 5 degrees (Alevin and Eggs: Sub Lethal)
#    Midnight - 11:59pm <5 degrees c
TempsL5MY <- ST %>%
  filter(Temp_C < 5) %>%
  group_by(SiteCode, Year, Month, .drop=F) %>%
  summarise(numberoflogs_MYL5 = n())
TempsL5MY
#  Number of logs per Month Year less than 5 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountL5<-left_join(QCTempMU,TempsL5MY, by = c("SiteCode","Year","Month"))
LogCountL5 #216 x 5
PropLogL5<-LogCountL5%>%
  mutate(
    PropLogL5 = numberoflogs_MYL5/TNumLogs
  )
PropLogL5 #289 X 6   - Will need to add to STPred
#################################################################################
STPred5<-left_join(STPred4,PropLogL5, by = c("SiteCode","Year","Month"))
STPred5 #238 x 9
STPred5<-STPred5%>%
  select(-TNumLogs,-numberoflogs_MYL5)
STPred5#238 x 10
#################################################################################
#For 4.5 degrees (Eggs: Lethal)
#  Midnight - 11:59pm <4.5 degrees c
TempsL4.5MY <- ST %>%
  filter(Temp_C < 4.5) %>%
  group_by(SiteCode, Year, Month,.drop=F) %>%
  summarise(numberoflogs_MYL4.5 = n()) 
TempsL4.5MY
#  Number of logs per Month Year less than 4.5 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountL4.5<-left_join(QCTempMU,TempsL4.5MY, by = c("SiteCode","Year","Month"))
LogCountL4.5 #238x 5
PropLogL4.5<-LogCountL4.5%>%
  mutate(
    PropLogL4.5 = numberoflogs_MYL4.5/TNumLogs
  )
PropLogL4.5 #238 x 6
#################################################################################
STPred6<-left_join(STPred5,PropLogL4.5, by = c("SiteCode","Year","Month"))
STPred6 #238 x 13
STPred6<-STPred6%>%
  select(-TNumLogs,-numberoflogs_MYL4.5)
STPred6#238 x 11
#################################################################################
#For 20 degrees (Adults: Sub Lethal)
#   Midnight - 11:59pm >20 degrees c
TempsG20MY <- ST %>%
  filter(Temp_C > 20) %>%
  group_by(SiteCode, Year, Month, .drop=FALSE) %>%
  summarise(numberoflogs_MYG20 = n()) 
TempsG20MY
#  Number of logs per Month Year greater than 20 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountG20<-left_join(QCTempMU,TempsG20MY, by = c("SiteCode","Year","Month"))
LogCountG20 #238 x 5
LogCountG20[is.na(LogCountG20)]<-0
PropLogG20<-LogCountG20%>%
  mutate(
    PropLogG20 = numberoflogs_MYG20/TNumLogs
  )
PropLogG20 #238 x 6
#################################################################################
STPred7<-left_join(STPred6,PropLogG20, by = c("SiteCode","Year","Month"))
STPred7 #238 x 14
STPred7<-STPred7%>%
  select(-TNumLogs,-numberoflogs_MYG20)
STPred7#238 x 12
#################################################################################
#For 24 degrees (Adults: Lethal)
#     Midnight - 11:59pm >24 degrees c
TempsG24MY <- ST %>%
  filter(Temp_C > 24) %>%
  group_by(SiteCode, Year, Month, .drop=F) %>%
  summarise(numberoflogs_MYG24 = n()) 
TempsG24MY
#  Number of logs per Month Year greater than 24 degrees Celsius with Year-Month 
#      combos we don't need.
#Do a join to get rid of rows where no temp data was collected
LogCountG24<-left_join(QCTempMU,TempsG24MY, by = c("SiteCode","Year","Month"))
LogCountG24 #238 x 5
LogCountG24[is.na(LogCountG24)]<-0
PropLogG24<-LogCountG24%>%
  mutate(
    PropLogG24 = numberoflogs_MYG24/TNumLogs
  )
PropLogG24 #238 X 6   - Will need to add to STPred
############################################################################################
STPred8<-left_join(STPred7,PropLogG24, by = c("SiteCode","Year","Month"))
STPred8 #238 x 15
STPred8<-STPred8%>%
  select(-TNumLogs,-numberoflogs_MYG24)
STPred8#238 x 13
#write.csv(STPred8,"STPred_completeOnly.csv")
#   looks good
#################################################################################
#Period: March-April
#######
#QC check for full March and April Site and Year combos
MarApr<-ST%>%
  filter(Month=="03"|Month=="04")%>%
  group_by(SiteCode,Year) %>%
  summarize(
    n=n()
  )
MarApr# 19 x 3
#  Mar+Apr logs should = 5856, 
#  Remove Yellow-2021, Fall-2021, Flag-2021, and Sand-2021
#Create Min/Max reading for each.
MarApr_Extremes <- ST %>%
  filter(Month=="03"|Month=="04")%>%
  filter(!(SiteCode=="Yellow.LittleLoyalsock" & Year=="2021"|
           SiteCode=="Fall.Hoagland" & Year=="2021"|
           SiteCode=="FlagMarsh.Pigeon" & Year=="2021"|
           SiteCode=="Sand.Mill" & Year=="2021"))%>%
  group_by(SiteCode,Year) %>%
  summarise(
    MarAprHighest_TempC = max(Temp_C),
    MarAprLowest_TempC = min(Temp_C)
  )       
MarApr_Extremes # 15 x 4
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
MarApr_AvgMinMax <- YM %>%
  filter(Month=="03"|Month=="04")%>%
  filter(!(SiteCode=="Yellow.LittleLoyalsock" & Year=="2021"|
             SiteCode=="Fall.Hoagland" & Year=="2021"|
             SiteCode=="FlagMarsh.Pigeon" & Year=="2021"|
             SiteCode=="Sand.Mill" & Year=="2021"))%>%
  group_by(SiteCode,Year) %>%
  summarise(
    MarAprAvgMin=mean(DailyMin_tempC), 
    MarAprAvgMax=mean(DailyMax_tempC))
MarApr_AvgMinMax # 15 x 5
###############################################################################
#Join QCTempMU with Monthly_Extremes to reduce Site-Year-Month combos to complete
MarAprSTPred<-left_join(MarApr_Extremes,MarApr_AvgMinMax, by = c("SiteCode","Year"))
MarAprSTPred #15 x 6
###############################################################################
# Avg Year-Month Temp
Avg_MarApr_Temp <- ST %>%
  filter(Month=="03"|Month=="04")%>%
  filter(!(SiteCode=="Yellow.LittleLoyalsock" & Year=="2021"|
             SiteCode=="Fall.Hoagland" & Year=="2021"|
             SiteCode=="FlagMarsh.Pigeon" & Year=="2021"|
             SiteCode=="Sand.Mill" & Year=="2021"))%>%
  group_by(SiteCode,Year) %>%
  summarise(
    Avg_MarApr_TempC = mean(Temp_C))
Avg_MarApr_Temp #15 x 3
#################################################################################
MarAprSTPred2<-left_join(MarAprSTPred,Avg_MarApr_Temp, by = c("SiteCode","Year"))
MarAprSTPred2 #15 x 7
#################################################################################
#Number of logs per MarApr, 
#For 3 degrees (Alevin: Lethal)
#   day = Midnight - 11:59pm;  <3 degrees c
TempsL3MarApr <- ST %>%
  filter(Month=="03"|Month=="04")%>%
  filter(!(SiteCode=="Yellow.LittleLoyalsock" & Year=="2021"|
             SiteCode=="Fall.Hoagland" & Year=="2021"|
             SiteCode=="FlagMarsh.Pigeon" & Year=="2021"|
             SiteCode=="Sand.Mill" & Year=="2021"))%>%
  filter(Temp_C < 3) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_MarAprL3 = n()) %>%
  mutate(TNumLogs = 5856)
TempsL3MarApr #15 x 4
PropLogL3MarApr<-TempsL3MarApr%>%
  mutate(
    PropLogL3MarApr = numberoflogs_MarAprL3/TNumLogs
  )
PropLogL3MarApr #15 x 5   
#################################################################################
MarAprSTPred3<-left_join(MarAprSTPred2,PropLogL3MarApr, by = c("SiteCode","Year"))
MarAprSTPred3 #15 x 10
MarAprSTPred3<-MarAprSTPred3%>%
  select(-TNumLogs,-numberoflogs_MarAprL3)
MarAprSTPred3# 15 x 8
#################################################################################
#For 5 degrees (Alevin and Eggs: Sub Lethal)
#    Midnight - 11:59pm <5 degrees c
TempsL5MarApr <- ST %>%
  filter(Month=="03"|Month=="04")%>%
  filter(!(SiteCode=="Yellow.LittleLoyalsock" & Year=="2021"|
             SiteCode=="Fall.Hoagland" & Year=="2021"|
             SiteCode=="FlagMarsh.Pigeon" & Year=="2021"|
             SiteCode=="Sand.Mill" & Year=="2021"))%>%
  filter(Temp_C < 5) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_MarAprL5 = n()) %>%
  mutate(TNumLogs = 5856)
TempsL5MarApr #15 x 4
PropLogL5MarApr<-TempsL5MarApr%>%
  mutate(
    PropLogL5MarApr = numberoflogs_MarAprL5/TNumLogs
  )
PropLogL5MarApr #15 x 5   
#################################################################################
MarAprSTPred4<-left_join(MarAprSTPred3,PropLogL5MarApr, by = c("SiteCode","Year"))
MarAprSTPred4 #15 x 11
MarAprSTPred4<-MarAprSTPred4%>%
  select(-TNumLogs,-numberoflogs_MarAprL5)
MarAprSTPred4# 15 x 8
#################################################################################
#For 4.5 degrees (Eggs: Lethal)
#  Midnight - 11:59pm <4.5 degrees c
TempsL4.5MarApr <- ST %>%
  filter(Month=="03"|Month=="04")%>%
  filter(!(SiteCode=="Yellow.LittleLoyalsock" & Year=="2021"|
             SiteCode=="Fall.Hoagland" & Year=="2021"|
             SiteCode=="FlagMarsh.Pigeon" & Year=="2021"|
             SiteCode=="Sand.Mill" & Year=="2021"))%>%
  filter(Temp_C < 4.5) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_MarAprL4.5 = n()) %>%
  mutate(TNumLogs = 5856)
TempsL4.5MarApr #15 x 4
PropLogL4.5MarApr<-TempsL4.5MarApr%>%
  mutate(
    PropLogL4.5MarApr = numberoflogs_MarAprL4.5/TNumLogs
  )
PropLogL4.5MarApr #15 x 5   
#################################################################################
MarAprSTPred5<-left_join(MarAprSTPred4,PropLogL4.5MarApr, by = c("SiteCode","Year"))
MarAprSTPred5 #15 x 12
MarAprSTPred5<-MarAprSTPred5%>%
  select(-TNumLogs,-numberoflogs_MarAprL4.5)
MarAprSTPred5# 15 x 10
#################################################################################
#For 20 degrees (Adults: Sub Lethal)
#   Midnight - 11:59pm >20 degrees c
TempsG20MarApr <- ST %>%
  filter(Month=="03"|Month=="04")%>%
  filter(!(SiteCode=="Yellow.LittleLoyalsock" & Year=="2021"|
             SiteCode=="Fall.Hoagland" & Year=="2021"|
             SiteCode=="FlagMarsh.Pigeon" & Year=="2021"|
             SiteCode=="Sand.Mill" & Year=="2021"))%>%
  filter(Temp_C >20) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_MarAprG20 = n()) %>%
  mutate(TNumLogs = 5856)
TempsG20MarApr #0
PropLogG20MarApr<-TempsG20MarApr%>%
  mutate(
    PropLogG20MarApr = numberoflogs_MarAprG20/TNumLogs
  )
PropLogG20MarApr #0  
#################################################################################
#No data to add
#################################################################################
#For 24 degrees (Adults: Lethal)
#     Midnight - 11:59pm >24 degrees c
TempsG24MarApr <- ST %>%
  filter(Month=="03"|Month=="04")%>%
  filter(!(SiteCode=="Yellow.LittleLoyalsock" & Year=="2021"|
             SiteCode=="Fall.Hoagland" & Year=="2021"|
             SiteCode=="FlagMarsh.Pigeon" & Year=="2021"|
             SiteCode=="Sand.Mill" & Year=="2021"))%>%
  filter(Temp_C >24) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_MarAprG24 = n()) %>%
  mutate(TNumLogs = 5856)
TempsG24MarApr #0
PropLogG24MarApr<-TempsG24MarApr%>%
  mutate(
    PropLogG20MarApr = numberoflogs_MarAprG24/TNumLogs
  )
PropLogG24MarApr #0  
#################################################################################
#No data to add
#################################################################################
MarAprSTPred5# 15 x 10
#write_csv(MarAprSTPred5,"MarAprSTPred5.csv")

#################################################################################
#Period: June-August
#######
#QC check for full March and April Site and Year combos
JunAug<-ST%>%
  filter(Month=="06"|Month=="07"|Month=="08")%>%
  group_by(SiteCode,Year) %>%
  summarize(
    n=n()
  )
JunAug# 35 x 3
#  June + July + Aug logs should = 8832, 
#  Keep 2021 for Brunnerdale, Dutchman, Ellis, Sherman, Shingle

# What if we do...July and Aug
JulAug<-ST%>%
  filter(Month=="07"|Month=="08")%>%
  group_by(SiteCode,Year) %>%
  summarize(
    TNumLogs=n()
  )
JulAug# 30 x 3
#  July + Aug logs should = 5952, 
#  Remove Level-2021, Streby-2021, Swamp-2020, SandSpring 2021
JulAugU<-JulAug%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))
JulAugU#26 x 3    

# What if we do...July and Aug and Sept
JulSep<-ST%>%
  filter(Month=="07"|Month=="08"|Month=="09")%>%
  group_by(SiteCode,Year) %>%
  summarize(
    n=n()
  )
JulSep# 31 x 3
#  July + Aug + Sept logs should = 8832, 
#  Remove Yellow-2021, Level-2021, Streby-2021, Swamp-2020, SandSpring-2021,
#         Shingle-2020, Dry.Loyalsock-2020, Shingle 2021

# What if we do...Aug and Sept
AugSep<-ST%>%
  filter(Month=="08"|Month=="09")%>%
  group_by(SiteCode,Year) %>%
  summarize(
    n=n()
  )
AugSep# 30 x 3
#  Aug + Sept logs should = 5856, 
#  Remove Yellow-2021, Streby-2021, Swamp-2020, SandSpring-2021,
#         Shingle-2020, Dry.Loyalsock-2020, Shingle 2021


#Create Min/Max reading for JulAug
JulAug_Extremes <- ST %>%
  filter(Month=="07"|Month=="08")%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))%>%
  group_by(SiteCode,Year) %>%
  summarise(
    JulAugHighest_TempC = max(Temp_C),
    JulAugLowest_TempC = min(Temp_C)
  )       
JulAug_Extremes # 26 x 4
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
JulAug_AvgMinMax <- YM %>%
  filter(Month=="07"|Month=="08")%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))%>%
  group_by(SiteCode,Year) %>%
  summarise(
    JulAugAvgMin=mean(DailyMin_tempC), 
    JulAugAvgMax=mean(DailyMax_tempC))
JulAug_AvgMinMax # 26 x 5
###############################################################################
#Join QCTempMU with Monthly_Extremes to reduce Site-Year-Month combos to complete
JulAugSTPred<-left_join(JulAug_Extremes,JulAug_AvgMinMax, by = c("SiteCode","Year"))
JulAugSTPred #26 x 6
###############################################################################
# Avg Year-Month Temp
Avg_JulAug_Temp <- ST %>%
  filter(Month=="07"|Month=="08")%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))%>%
  group_by(SiteCode,Year) %>%
  summarise(
    Avg_JulAug_TempC = mean(Temp_C))
Avg_JulAug_Temp #26 x 3
#################################################################################
JulAugSTPred2<-left_join(JulAugSTPred,Avg_JulAug_Temp, by = c("SiteCode","Year"))
JulAugSTPred2 #26 x 7
#################################################################################
#Number of logs per MarApr, 
#For 3 degrees (Alevin: Lethal)
#   day = Midnight - 11:59pm;  <3 degrees c
TempsL3JulAug <- ST %>%
  filter(Month=="07"|Month=="08")%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))%>%
  filter(Temp_C < 3) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_JulAugL3 = n()) %>%
  mutate(TNumLogs = 5856)
TempsL3JulAug #0
#No data to add 
#################################################################################
#For 5 degrees (Alevin and Eggs: Sub Lethal)
#    Midnight - 11:59pm <5 degrees c
TempsL5JulAug <- ST %>%
  filter(Month=="07"|Month=="08")%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))%>%
  filter(Temp_C < 5) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_JulAugL5 = n()) %>%
  mutate(TNumLogs = 5856)
TempsL5JulAug #0
#No data to add 
#################################################################################
#For 4.5 degrees (Eggs: Lethal)
#  Midnight - 11:59pm <4.5 degrees c
TempsL4.5JulAug <- ST %>%
  filter(Month=="07"|Month=="08")%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))%>%
  filter(Temp_C < 4.5) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_JulAugL4.5 = n()) %>%
  mutate(TNumLogs = 5856)
TempsL4.5JulAug #0
#No data to add
#################################################################################
#For 20 degrees (Adults: Sub Lethal)
#   Midnight - 11:59pm >20 degrees c
TempsG20JulAug <- ST %>%
  filter(Month=="07"|Month=="08")%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))%>%
  filter(Temp_C >20) %>%
  group_by(SiteCode, Year,.drop=F) %>%
  summarise(numberoflogs_JulAugG20 = n()) 
TempsG20JulAug #48 x 4
#Do a join to get rid of rows where no temp data was collected
LogCountG20JulAug<-left_join(JulAugU,TempsG20JulAug, by = c("SiteCode","Year"))
LogCountG20JulAug #26 x 5
LogCountG20JulAug[is.na(LogCountG20JulAug)]<-0
PropLogG20JulAug<-LogCountG20JulAug%>%
  mutate(
    PropLogG20JulAug = numberoflogs_JulAugG20/TNumLogs
  )
PropLogG20JulAug #26 x 5  
#################################################################################
##################################################################################
JulAugSTPred3<-left_join(JulAugSTPred2,PropLogG20JulAug, by = c("SiteCode","Year"))
JulAugSTPred3 #26 x 10
JulAugSTPred3<-JulAugSTPred3%>%
  select(-TNumLogs,-numberoflogs_JulAugG20)
JulAugSTPred3# 26 x 8
#################################################################################
#For 24 degrees (Adults: Lethal)
#     Midnight - 11:59pm >24 degrees c
TempsG24JulAug <- ST %>%
  filter(Month=="07"|Month=="08")%>%
  filter(!(SiteCode=="Level.Lick" & Year=="2021"|
             SiteCode=="Streby.Lick" & Year=="2021"|
             SiteCode=="Swamp.Hoagland" & Year=="2020"|
             SiteCode=="SandSpring.LittleBear" & Year=="2021"))%>%
  filter(Temp_C >24) %>%
  group_by(SiteCode, Year) %>%
  summarise(numberoflogs_JulAugG24 = n()) 
TempsG24JulAug #0
#Do a join to get rid of rows where no temp data was collected
LogCountG24JulAug<-left_join(JulAugU,TempsG24JulAug, by = c("SiteCode","Year"))
LogCountG24JulAug #26 x 4
LogCountG24JulAug[is.na(LogCountG24JulAug)]<-0
PropLogG24JulAug<-LogCountG24JulAug%>%
  mutate(
    PropLogG24JulAug = numberoflogs_JulAugG24/TNumLogs
  )
PropLogG24JulAug #26 x 5  
#################################################################################
JulAugSTPred4<-left_join(JulAugSTPred3,PropLogG24JulAug, by = c("SiteCode","Year"))
JulAugSTPred4 #26 x 11
JulAugSTPred4<-JulAugSTPred4%>%
  select(-TNumLogs,-numberoflogs_JulAugG24)
JulAugSTPred4# 26 x 9
#################################################################################
JulAugSTPred4# 15 x 10
#write_csv(JulAugSTPred4,"JulAugSTPred.csv")

