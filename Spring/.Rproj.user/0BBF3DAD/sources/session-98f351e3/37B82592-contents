#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Read Both BKTVar and STDPred csv's

BKTVar <- read_csv("BKTVar.csv") #correct
BKTVar_AllSites <- read_csv("BKTVar_AllSites.csv") #Correct 


ST <- read_csv("ST.csv")
STPred <- read_csv("STPred.csv") #correct
STPred_completeOnly <- read_csv("STPred_completeOnly.csv")
MarAprSTPred <- read_csv("MarAprSTPred5.csv")
JulAugSTpred <- read_csv("JulAugSTPred.csv")

install.packages("mgcv")
library(mgcv)

BKTVar <- BKTVar %>%
  filter(!(Year=="2019"))

Data_merge <- merge(BKTVar, STPred, by = c("SiteCode", "Year"), all = FALSE)

#Data_merge_Spring <- merge(BKTVar, MarAprSTPred, by = c("SiteCode"), all = FALSE)

Data_merge_Summer <- merge(BKTVar, JulAugSTpred, by = c("SiteCode"), all = FALSE)

Data_merge_Summer <- filter(Data_merge_Summer,(!(SiteCode=="Brunnerdale.Ogdonia" & Year.x=="2020" & Year.y=="2021"|
           SiteCode=="Dutchman.Loyalsock" & Year.x=="2020" & Year.y=="2021"|
           SiteCode=="Ellis.Loyalsock" & Year.x=="2020" & Year.y=="2021"|
           SiteCode=="Sherman.Loyalsock" & Year.x=="2020" & Year.y=="2021"|
             SiteCode=="Shingle.Bear" & Year.x=="2020" & Year.y=="2021")))

Data_merge_CompleteOnly <- merge(BKTVar, STPred_completeOnly, by = c("SiteCode", "Year"), all = FALSE)   

BKTVar2021 <- BKTVar %>%
  filter(Year=="2021")

STPred or ST <- BKTVar %>%
  filter(Year=="2021")

#Filter for 6 sites in BKTVar2021!!!!!!!!
