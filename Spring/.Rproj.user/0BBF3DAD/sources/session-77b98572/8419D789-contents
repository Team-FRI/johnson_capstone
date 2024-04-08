#Database File Prep for Long-term (LT) Loyalsock (L) Surveys
#Created by Sara Ashcraft 2/18/2024
#Modified by Sara Ashcraft 3/20/2024 - prep for modeling
#Load Library
library(tidyverse)

#Set working Directory
setwd("C:/GitHub/johnson_capstone/spring")


#Load data
Sites<-read_csv("All_Sites.csv",
                col_types = cols(
                  SiteID = col_double(),
                  ChooseMe = col_skip(),
                  SiteCode = col_character(),
                  SiteName = col_character(),
                  TribTo = col_character(),
                  RFPID = col_skip(),
                  MouthLat = col_skip(),
                  MouthLon = col_skip(),
                  SiteLat = col_double(),
                  SiteLon = col_double(),
                  ParkLat = col_skip(),
                  ParkLon = col_skip(),
                  County = col_skip(),
                  NearTown = col_skip(),
                  TownCardinal = col_skip(),
                  HUC12Watershed = col_skip(),
                  NearRoadCross = col_skip(),
                  AccessContactName = col_skip(),
                  AccessContactPhone = col_skip(),
                  AccessContactEmail = col_skip(),
                  AccessParking = col_skip(),
                  LocationNotes = col_skip(),
                  YearResto = col_skip(),
                  ExpFunction = col_skip()
                ))
Sites#  1177 x 10
#    Once reduced to LT Loyalsock sites only 2 sites out of 30 had County or HUC12 - removed from data prep.

Events<-read_csv("All_Events.csv",
                 col_types = cols(
                   EventsID = col_skip(),
                   SiteCode = col_character(),
                   EventCode = col_character(),
                   Personnel = col_skip(),
                   CrewLeader = col_skip(),
                   SiteName = col_skip(),
                   TribTo = col_skip(),
                   Date = col_character(),
                   TimeStart = col_skip(),
                   TimeEnd = col_skip(),
                   MouthLat = col_skip(),
                   MouthLon = col_skip(),
                   SiteLat = col_skip(),
                   SiteLon = col_skip(),
                   Discharge = col_skip(),
                   WaterTemp = col_skip(),
                   pH = col_skip(),
                   SpecCond = col_skip(),
                   Alk = col_skip(),
                   DO = col_skip(),
                   Hardness = col_skip(),
                   AirTemp = col_skip(),
                   Turbidity = col_skip(),
                   Format = col_skip(),
                   Unassessed = col_skip(),
                   Connectivity = col_skip(),
                   Loyalsock = col_character(),
                   FarmStreams = col_skip(),
                   Other = col_skip(),
                   Location = col_skip(),
                   EventNotes = col_skip(),
                   Weather = col_skip(),
                   WQTime = col_skip()
                 ))
Events# 1632 x 6

#Reduce Events to only events categorized as Loyalsock

EventsLoyal<-Events%>%
  
  filter(Loyalsock =="Loyalsock")%>%
  
  select(!Loyalsock)%>%# after QC - don't need this column
  
  arrange(SiteCode)

EventsLoyal#306 x 3
#    Time had only 9 surveys out of 306 with a time recorded - removed from data prep.
#QC- that all events make sense as LTL.
EvLoySum<-EventsLoyal%>%
  group_by(SiteCode)%>%
  summarise(
    LoyalEventCount = n()
  )
EvLoySum
# 24 of 30 sites have 10 events.
# Sites with 11 events need investigated: Coal, DryHB, Granddad, Painter, Red, Rock
#EventsInv<-EventsLoyal%>%
#  filter(SiteCode =="Coal.UpperLoyalsock"|SiteCode =="DryHB.Hoagland"|SiteCode =="Grandad.Hessler"|
#           SiteCode =="Painter.LittleBear"|SiteCode =="Red.LittleBear"|SiteCode =="Rock.LittleLoyalsock")%>%
#  arrange(SiteCode,EventCode)
#EventsInv# all sites surveyed in 2021 - an additional year after all other sites labeled Loyalsock.


#Reduce Sites to only Sites categorized as Loyalsock
SitesLoyal<-left_join(EvLoySum,Sites,by="SiteCode")
SitesLoyalU<-SitesLoyal[, c(1,4,5,2,6,7)]%>%
  arrange(SiteCode)
SitesLoyalU# 30 x 6

#Update SiteCodes in Sites df to match Temperature files
SitesLoyalU$SiteCode[1]<-"Bear.Loyalsock"
SitesLoyalU$SiteCode[2]<-"Brunnerdale.Ogdonia"
SitesLoyalU$SiteCode[3]<-"Coal.Loyalsock"
SitesLoyalU$SiteCode[4]<-"Conklin.Mill"
SitesLoyalU$SiteCode[5]<-"Dry.Hoagland"
SitesLoyalU$SiteCode[6]<-"Dry.Loyalsock"
SitesLoyalU$SiteCode[7]<-"Dutchman.Loyalsock"
SitesLoyalU$SiteCode[8]<-"Ellis.Loyalsock"
SitesLoyalU$SiteCode[9]<-"Fall.Hoagland"
SitesLoyalU$SiteCode[10]<-"FlagMarsh.Pigeon"
SitesLoyalU$SiteCode[11]<-"Grandad.Hessler"
SitesLoyalU$SiteCode[12]<-"Huckle.Loyalsock"
SitesLoyalU$SiteCode[13]<-"Lake.Elk"
SitesLoyalU$SiteCode[14]<-"Level.Lick"
SitesLoyalU$SiteCode[15]<-"Lick.Kings"
SitesLoyalU$SiteCode[16]<-"Mill.Loyalsock"
SitesLoyalU$SiteCode[17]<-"Painter.LittleBear"
SitesLoyalU$SiteCode[18]<-"Porter.Hoagland"
SitesLoyalU$SiteCode[19]<-"Red.LittleBear"
SitesLoyalU$SiteCode[20]<-"Rock.LittleLoyalsock"
SitesLoyalU$SiteCode[21]<-"Sand.Mill"
SitesLoyalU$SiteCode[22]<-"SandSpring.LittleBear"
SitesLoyalU$SiteCode[23]<-"Scar.Loyalsock"
SitesLoyalU$SiteCode[24]<-"Sherman.Loyalsock"
SitesLoyalU$SiteCode[25]<-"Shingle.Bear"
SitesLoyalU$SiteCode[26]<-"Snake.Bear"
SitesLoyalU$SiteCode[27]<-"Streby.Lick"
SitesLoyalU$SiteCode[28]<-"Swamp.Hoagland"
SitesLoyalU$SiteCode[29]<-"Weed.Hoagland"
SitesLoyalU$SiteCode[30]<-"Yellow.LittleLoyalsock"
SitesLoyalU
#  SiteCodes match SiteName
#write_csv(SitesLoyalU,"SitesLoyalU.csv")

#Update SiteCodes in Event df to match Temperature files
EventsLoyal$SiteCode[1:10]<-"Bear.Loyalsock"
EventsLoyal$SiteCode[11:20]<-"Brunnerdale.Ogdonia"
EventsLoyal$SiteCode[21:31]<-"Coal.Loyalsock"
EventsLoyal$SiteCode[32:41]<-"Conklin.Mill"
EventsLoyal$SiteCode[42:52]<-"Dry.Hoagland"
EventsLoyal$SiteCode[53:62]<-"Dry.Loyalsock"
EventsLoyal$SiteCode[63:72]<-"Dutchman.Loyalsock"
EventsLoyal$SiteCode[73:82]<-"Ellis.Loyalsock"
EventsLoyal$SiteCode[83:92]<-"Fall.Hoagland"
EventsLoyal$SiteCode[93:102]<-"FlagMarsh.Pigeon"
EventsLoyal$SiteCode[103:113]<-"Grandad.Hessler"
EventsLoyal$SiteCode[114:123]<-"Huckle.Loyalsock"
EventsLoyal$SiteCode[124:133]<-"Lake.Elk"
EventsLoyal$SiteCode[134:143]<-"Level.Lick"
EventsLoyal$SiteCode[144:153]<-"Lick.Kings"
EventsLoyal$SiteCode[154:163]<-"Mill.Loyalsock"
EventsLoyal$SiteCode[164:174]<-"Painter.LittleBear"
EventsLoyal$SiteCode[175:184]<-"Porter.Hoagland"
EventsLoyal$SiteCode[185:195]<-"Red.LittleBear"
EventsLoyal$SiteCode[196:206]<-"Rock.LittleLoyalsock"
EventsLoyal$SiteCode[207:216]<-"Sand.Mill"
EventsLoyal$SiteCode[217:226]<-"SandSpring.LittleBear"
EventsLoyal$SiteCode[227:236]<-"Scar.Loyalsock"
EventsLoyal$SiteCode[237:246]<-"Sherman.Loyalsock"
EventsLoyal$SiteCode[247:256]<-"Shingle.Bear"
EventsLoyal$SiteCode[257:266]<-"Snake.Bear"
EventsLoyal$SiteCode[267:276]<-"Streby.Lick"
EventsLoyal$SiteCode[277:286]<-"Swamp.Hoagland"
EventsLoyal$SiteCode[287:296]<-"Weed.Hoagland"
EventsLoyal$SiteCode[297:306]<-"Yellow.LittleLoyalsock"
EventsLoyal
#   All sitecodes match with eventcodes
#write_csv(EventsLoyal,"EventsLoyal.csv")

#Create Table for reducing other table to LTL only
EventsLoyalR<-EventsLoyal[,c(1,2)]
EventsLoyalR
################################################################################
#Fish Data

#Load Data
FishSurvey<-read_csv("All_FishSurvey.csv",
                     col_types = cols(
                       SurveyID = col_skip(),
                       SiteCode = col_skip(),
                       EventCode = col_character(),
                       Date = col_skip(),
                       Watername = col_skip(),
                       TribTo = col_skip(),
                       Time = col_skip(),
                       CrewLeader = col_skip(),
                       Comments = col_skip(),
                       Location = col_skip(),
                       Length = col_double(),
                       AveWid = col_double(),
                       Wid1 = col_skip(),
                       Wid2 = col_skip(),
                       Wid3 = col_skip(),
                       Wid4 = col_skip(),
                       Wid5 = col_skip(),
                       VoltStart = col_skip(),
                       FreqStart = col_skip(),
                       DutyStart = col_skip(),
                       Volts = col_skip(),
                       FreqEnd = col_skip(),
                       DutyEnd = col_skip(),
                       Flow = col_skip(),
                       EffortMin = col_skip(),
                       Seconds = col_skip(),
                       Pass1TimeA = col_skip(),
                       Pass2TimeA = col_skip(),
                       Pass3TimeA = col_skip(),
                       Pass1TimeB = col_skip(),
                       Pass2TimeB = col_skip(),
                       Pass3TimeB = col_skip(),
                       Pass1TimeC = col_skip(),
                       Pass2TimeC = col_skip(),
                       Pass3TimeC = col_skip(),
                       Pass1TimeD = col_skip(),
                       Pass2TimeD = col_skip(),
                       Pass3TimeD = col_skip(),
                       PEAllTrout = col_skip(),
                       PEBrook = col_skip(),
                       PEBrown = col_skip(),
                       PERainbow = col_skip(),
                       PEYOYAll = col_skip(),
                       PEYOYBrook = col_skip(),
                       PEYOYBrown = col_skip(),
                       PEYOYRainbow = col_skip(),
                       PEAdtAll = col_skip(),
                       PEAdtBrook = col_skip(),
                       PEAdtBrown = col_skip(),
                       PEAdtRainbow = col_skip(),
                       BrookP1 = col_skip(),
                       BrookP2 = col_skip(),
                       BrookP3 = col_skip(),
                       BrownP1 = col_skip(),
                       BrownP2 = col_skip(),
                       BrownP3 = col_skip(),
                       RainP1 = col_skip(),
                       RainP2 = col_skip(),
                       RainP3 = col_skip(),
                       TigerP1 = col_skip(),
                       TigerP2 = col_skip(),
                       TigerP3 = col_skip(),
                       TotalP1 = col_skip(),
                       TotalP2 = col_skip(),
                       TotalP3 = col_skip(),
                       GrandTotal = col_skip(),
                       NumShockers = col_skip(),
                       TimeStamp = col_skip(),
                       IDBy = col_skip(),
                       RecordBy = col_skip(),
                       Crew = col_skip(),
                       RFPID = col_skip(),
                       SurveyNotes = col_skip(),
                       Operators = col_skip(),
                       Netters = col_skip()
                     ))
FishSurvey
#Reduce to fish surveys completed as part of LTL
FishSurLoy<-left_join(EventsLoyalR,FishSurvey,by="EventCode")
FishSurLoy # 306 x 4
#write_csv(FishSurLoy,"FishSurLoy.csv")
######################
#Fish Records

#load data
FishRecords<-read_csv("All_FishRecords.csv",
                      col_types = cols(
                        ID = col_skip(),
                        Species = col_character(),
                        Length_mm = col_double(),
                        Wt_g = col_double(),
                        DELTM = col_skip(),
                        FishComment = col_skip(),
                        Diet = col_skip(),
                        Pass = col_skip(),
                        Date = col_skip(),
                        waterName = col_skip(),
                        TribTo = col_skip(),
                        EventCode = col_character(),
                        ScaleID = col_skip()
                      ))
FishRecords<-FishRecords[,c(4,1,2,3)]
FishRecords
#Try to Reduce records but need to QC to ensure no LTL records are lost.
FishRecLoy<-left_join(x=EventsLoyalR,y=FishRecords,by="EventCode",multiple="all")
FishRecLoy  
#QC for Events when the SiteCode is 'NA'
QCFishRec<-FishRecLoy%>%
  group_by(SiteCode,EventCode)%>%
  summarize(
    Count = n()
  )
QCFishRec # 306 events - check
#QC to see if only LTL sites are left
LTLSites<-QCFishRec%>%
  group_by(SiteCode)%>%
  summarise(count = n())
LTLSites # 30- sites - check
#QC Species list
QCSpp<-FishRecLoy%>%
  select(SiteCode,EventCode,Species)%>%
  group_by(SiteCode,EventCode,Species)%>%
  summarize(
    Count = n()
  )
QCSpp
#   4 rows with no species:
#       2012 - Ellis; 2013 - Ellis; 2012 - Sand; 2013 - Sherman

#Now reduce fish records to Brook Trout only
BKTRecLoy<-FishRecLoy%>%
  filter(Species=="Brook Trout")
BKTRecLoy
# Issues with TL or Wt:
#    2012 Huckle: 1BKT, No TL/No Wt
#    2013 Huckle: 1BKT, No TL/No Wt
#    2018 Scar:   1BKT, No TL/ Wt
#    2013 Shingle: 1BKT, No TL/No Wt
#    2013 Shingle: 18BKT,  TL/No Wt
#    2012 Yellow: 1BKT,  TL/No Wt
#  Not technically an issue for 2019-2021, our years of interest for this project.
#write.csv(BKTRecLoy,"BKTRecLoy.csv")
###############################
#Currently don't need Fish Tally data 
