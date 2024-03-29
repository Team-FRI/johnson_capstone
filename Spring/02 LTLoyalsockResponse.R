#Long-term (LT) Loyalsock (L) Surveys
#Creating Response variables
#Created by Sara Ashcraft 2/18/2024
#Modified by Sara Ashcraft 2/19/2024
#Modified by Sara Ashcraft 3/20/2024

#Load Library
library(tidyverse)

#Set working Directory
setwd("C:/GitHub/johnson_capstone/spring")


#Load data - all LTL only
#Sites with background data
SitesLoyalU<-read_csv("SitesLoyalU.csv")
SitesLoyalU

#Events with dates
EventsLoyal<-read_csv("EventsLoyal.csv",
                      col_types = cols(
                        SiteCode = col_character(),
                        EventCode = col_character(),
                        Date = col_date(format = "%m/%d/%Y")
                      ))
EventsLoyal
#Reduce Events to only 2019, 2020, and 2021
EventsLoyal19to21<-EventsLoyal%>%
  filter(Date>"2019-01-01")
EventsLoyal19to21
#   30 sites in 2019, 30 sites in 2020, 6 sites in 2021

#Fish Survey to create site area
FishSurLoy<-read_csv("FishSurLoy.csv")
FishSurLoy
#   201306.Sherman.UpperLoyalsock - does not have area calculation because there 
#   is no Length or AveWid - Eventually need to QC, but can move on for now.
FishSurLoy19to21<-FishSurLoy%>%
  filter(str_detect(EventCode,"^2019")|str_detect(EventCode,"^2020")|str_detect(EventCode,"^2021"))
FishSurLoy19to21
#   30 sites in 2019, 30 sites in 2020, 6 sites in 2021

#Brook Trout records to create count/event,biomass/event length freq histo for 2020 data, Adult/YOY ratio
BKTRecLoy<-read_csv("BKTRecLoy.csv")
BKTRecLoy
#BKTEvtSum<-BKTRecLoy%>%
#  group_by(EventCode)%>%
#  summarise(
#    Count = n(),
#    Biomass = sum(Wt_g)
#  )
#BKTEvtSum # 302 x 3 - need to check are missing 4 surveys(probably surveys where 
# no BKT were captured in which case need to add 0s for Count and Biomass)
#All EventCode have a Count
#The following 8 EventCodes have a Count but no Biomass:               
#  201806.DryHB.Hoagland,
#  201206.FlagMarsh.Pigeon,
#  201306.Shingle.Bear,    
#  201206.Shingle.Bear,
#  201406.Mill.Loyalsock,
#  201306.Huckle.Loyalsock,
#  201206.Huckle.Loyalsock,
#  201206.Yellow.LittleLoyalsock
#     None of these Events are in 2019, 2020, or 2021 - moving on
#Still missing 4 Events and I think it is these 4 that are missing:
#  201206.Sand.MillMokoma,
#  201206.Ellis.UpperLoyalsock,
#  201306.Sherman.UpperLoyalsock
#  201306.Ellis.LoyalsockUpper
#     None of these Events are in 2019, 2020, or 2021 - moving on
BKTRecLoy19to21<-BKTRecLoy%>%
  filter(str_detect(EventCode,"^2019")|str_detect(EventCode,"^2020")|str_detect(EventCode,"^2021"))
BKTRecLoy19to21
#Then use both FishSurLoy AND BKTRecLoy to create CPUE_Count and CPUE_Biomass

################################################################################
#Create Area using FishSurLoy
FishSurLoy19to21U<-FishSurLoy19to21%>%
  mutate(
    Area = Length*AveWid)%>%
  select(!c(Length,AveWid))
FishSurLoy19to21U # 66 x 3 - all have calculated area
################################################################################
#Create Count/Event and Biomass/Event using BKTRecLoy
BKTEvtSum<-BKTRecLoy19to21%>%
  group_by(EventCode)%>%
  summarise(
    Count = n(),
    Biomass = sum(Wt_g)
  )
BKTEvtSum #66 x 3 - all have count and biomass that isn't skewed by missing species or missing wt.
################################################################################
#Find YOY-Adult threshold with Length Frequency Histogram using BKTRecLoy for 2020 data
BKT2020TL<-BKTRecLoy19to21%>%
  filter(str_detect(EventCode,"2020"))
BKT2020TL# - need to QC
hist(BKT2020TL$Length_mm,freq=T,breaks=22.32,axes=F)
maj1<-c(20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,
        210,220,230,240)# Lengths
axis(1,at=maj1,lty=1,lwd=0.5, pos=0,labels=T)
maj2<-c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700)#Freq
axis(2,at=maj2,lty=1,lwd=0.5,las=2,pos=22)#tck=1
#Threshold 75
################################################################################
#Create Adult/YOY Ratio using BKTRecLoy 
#YOY < 75 (Length_mm)
#Adult >= 75 (Length_mm)
Ad_YOY<-BKTRecLoy19to21 %>%
  group_by(EventCode)%>%
  summarise(
    Adult = sum(Length_mm > 75,.drop=F),
    YOY = sum(Length_mm <=75,.drop=F),
    TotalCount = n()
  )
Ad_YOY
#Changing strategy to "percent Adults - percent YOY"
RatioAY<-Ad_YOY %>%
  mutate(
    PerA = Adult/TotalCount,
    PerY = YOY/TotalCount,
    RatAY = PerA-PerY
  )
RatioAY #66 x 7
# So, RatAY ranges from 1.00 (when all BKT are Adults) and -1.00 (when all BKT 
#   are YOY) and 0.00 indicates when BKT Adults=YOY.

################################################################################
#Create CPUE_Count and CPUE_Biomass using FishSurLoy AND BKTRecLoy
CPUE<-left_join(FishSurLoy19to21U,BKTEvtSum,by="EventCode")
CPUE
CPUEVar<-CPUE %>%
  group_by(EventCode)%>%
  summarise(
    CPUE_Count = Count/Area *100,
    CPUE_Biomass = Biomass/Area *100
  )
CPUEVar #66 x 3
################################################################################
#Add Year to Response Output for future filtering ability
CPUEYear <- left_join(CPUEVar, EventsLoyal19to21, by =c("EventCode"))
CPUEYearU <- CPUEYear %>% 
  mutate(
    Year = format(as.Date(Date), "%Y"))
CPUEYearU
#Combine response variables to one tibble
BKTVar<-left_join(CPUEYearU,RatioAY)%>%
  select(!c(Date,Adult,YOY,TotalCount,PerA,PerY))
BKTVar# 66 x 5 - no NAs, all joins correct
BKTVarO<-BKTVar[,c(4,1,5,2:3,6)]
BKTVarO
#write.csv(BKTVarO,"BKTVar.csv")
