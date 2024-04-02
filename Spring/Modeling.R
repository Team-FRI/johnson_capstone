#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Read Both BKTVar and STDPred csv's\

install.packages("readr")
library(readr)

BKTVar <- read_csv("BKTVar.csv") #correct
BKTVar_AllSites <- read_csv("BKTVar_AllSites.csv") #Correct 


ST <- read_csv("ST.csv")
STPred <- read_csv("STPred.csv") #correct
STPred_completeOnly <- read_csv("STPred_completeOnly.csv")
MarAprSTPred <- read_csv("MarAprSTPred5.csv")
JulAugSTpred <- read_csv("JulAugSTPred.csv")

install.packages("mgcv")
library(mgcv)

#BKTVar <- BKTVar %>%
#  filter(!(Year=="2019"))

#Data_merge <- merge(BKTVar, STPred, by = c("SiteCode", "Year"), all = FALSE)

#Data_merge_Spring <- merge(BKTVar, MarAprSTPred, by = c("SiteCode"), all = FALSE)

Data_merge_Summer <- merge(BKTVar, JulAugSTpred, by = c("SiteCode"), all = FALSE)

Data_merge_Summer <- filter(Data_merge_Summer,(!(SiteCode=="Brunnerdale.Ogdonia" & Year.x=="2020" & Year.y=="2021"|
           SiteCode=="Dutchman.Loyalsock" & Year.x=="2020" & Year.y=="2021"|
           SiteCode=="Ellis.Loyalsock" & Year.x=="2020" & Year.y=="2021"|
           SiteCode=="Sherman.Loyalsock" & Year.x=="2020" & Year.y=="2021"|
             SiteCode=="Shingle.Bear" & Year.x=="2020" & Year.y=="2021")))

Data_merge_CompleteOnly <- merge(BKTVar, STPred_completeOnly, by = c("SiteCode", "Year"), all = FALSE) 

 

#Filter out BKTVAr for 2021

BKTVar2021 <- BKTVar %>%
  filter(Year=="2021")

#Instal packages to filter STPred for 2020, idk if I need these anymore though. From when I was trying to figure out how to filter STPred for 2020. 

install.packages("dplyr")
library(dplyr)

install.packages("lubridate")
library(lubridate)

#Filter for 2020 and 2021

STPred1 <- STPred %>%
  filter(Year == "2020" | Year == "2021")



#STPred2 <- STPred1 %>%
#  filter(as.numeric(Month) >= 6)

#Next filter to keep all months in 2021 and in 2020 only months that start at June and everything after.
#Maybe change this to July (07)?

STPred2 <- STPred1 %>%
  filter((Year == "2021") | (Year == "2020" & as.numeric(Month) >= 6))

Data_merge_Model <- merge(BKTVar2021, STPred1, by = c("SiteCode", "Year"), all = FALSE)


#Filter for 6 sites in BKTVar2021!!!!!!!!
