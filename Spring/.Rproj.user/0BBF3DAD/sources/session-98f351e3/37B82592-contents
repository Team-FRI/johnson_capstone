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

Data_merge_Model <- merge(BKTVar2021, STPred1, by = c("SiteCode"), all = FALSE)

Data_merge_Model_Test  <- merge(BKTVar2021, STPred1, by = c("SiteCode", "Year"), all = FALSE)

Data_merge-Model2 <- left_join(BKTVar2021, STPred1, by = c("SiteCode"), all = FALSE)

#Filter for 6 sites in BKTVar2021!!!!!!!! (Dont need to do anymore)

#Now its time to make some models. 

#Intsall package that reads gams

install.packages("mgcv")
library(mgcv)


MR1 <- gam(CPUE_Count ~ Year.x+Month, data = Data_merge_Model)
summary(MR1)
AIC(MR1) #382.42

MR2 <- gam(CPUE_Count ~ Year.x*Month, data = Data_merge_Model)
summary(MR2)
AIC(MR2) #382.46

MR3 <- gam(CPUE_Count ~ Year.x+Highest_Temperature_C, data = Data_merge_Model)
summary(MR2)
AIC(MR3) #362.34

MR4 <- gam(CPUE_Count ~ Year.x*Highest_Temperature_C, data = Data_merge_Model)
summary(MR4)
AIC(MR4)#362.34

MR5 <- gam(CPUE_Count ~ Month+Highest_Temperature_C, data = Data_merge_Model)
summary(MR5)
AIC(MR5) #383.32

MR6 <- gam(CPUE_Count ~ Month*Highest_Temperature_C, data = Data_merge_Model)
summary(MR6)
AIC(MR6) # 388.17

MR7 <- gam(CPUE_Count ~ Year.x+Lowest_Temperature_C, data = Data_merge_Model)
summary(MR7)
AIC(MR7) # 388.17

MR8 <- gam(CPUE_Count ~ Month+Lowest_Temperature_C, data = Data_merge_Model)
summary(MR8)
AIC(MR8) # 377.17

MR8 <- gam(CPUE_Count ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR8)
AIC(MR8) # 392.17

MR9 <- gam(CPUE_Count ~ Year.x+AvgMax, data = Data_merge_Model)
summary(MR9)
AIC(MR9) # 362.09

MR10 <- gam(CPUE_Count ~ Year.x*AvgMax, data = Data_merge_Model)
summary(MR10)
AIC(MR10) # 362.09

MR11 <- gam(CPUE_Count ~ Month+AvgMax, data = Data_merge_Model)
summary(MR11)
AIC(MR11) # 373.09

MR12 <- gam(CPUE_Count ~ Month*AvgMax, data = Data_merge_Model)
summary(MR12)
AIC(MR12) # 382.76

MR13 <- gam(CPUE_Count ~ Year.x+AvgMin, data = Data_merge_Model)
summary(MR13)
AIC(MR13) # 362.09

MR14 <- gam(CPUE_Count ~ Year.x*AvgMin, data = Data_merge_Model)
summary(MR14)
AIC(MR14) # 362.13

MR15 <- gam(CPUE_Count ~ Month+AvgMin, data = Data_merge_Model)
summary(MR15)
AIC(MR15) # 371.48

MR16 <- gam(CPUE_Count ~ Month*AvgMin, data = Data_merge_Model)
summary(MR16)
AIC(MR16) # 386.76

MR17 <- gam(RatAY ~ Year.x+Highest_Temperature_C, data = Data_merge_Model)
summary(MR17)
AIC(MR17) # 64.76

MR18 <- gam(RatAY ~ Year.x*Highest_Temperature_C, data = Data_merge_Model)
summary(MR18)
AIC(MR18) # 64.76

MR19 <- gam(RatAY ~ Month+Highest_Temperature_C, data = Data_merge_Model)
summary(MR19)
AIC(MR19) # 85.18

MR20 <- gam(RatAY ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR20)
AIC(MR20) # 93.23

MR21 <- gam(RatAY ~ Year.x+Lowest_Temperature_C, data = Data_merge_Model)
summary(MR21)
AIC(MR21) # 64.76

MR22 <- gam(RatAY ~ Year.x*Highest_Temperature_C, data = Data_merge_Model)
summary(MR22)
AIC(MR22) # 64.76

MR23 <- gam(RatAY ~ Month+Lowest_Temperature_C, data = Data_merge_Model)
summary(MR23)
AIC(MR23) # 82.18

MR24 <- gam(RatAY ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR24)
AIC(MR24) # 93.23

MR25 <- gam(RatAY ~ Year.x+AvgMax, data = Data_merge_Model)
summary(MR25)
AIC(MR25) # 64.92

MR26 <- gam(RatAY ~ Year.x*AvgMax, data = Data_merge_Model)
summary(MR26)
AIC(MR26) # 64.92

MR27 <- gam(RatAY ~ Month+AvgMax, data = Data_merge_Model)
summary(MR27)
AIC(MR27) # 86.92

MR28 <- gam(RatAY ~ Month*AvgMax, data = Data_merge_Model)
summary(MR28)
AIC(MR28) # 87.96

MR29 <- gam(RatAY ~ Year.x+AvgMin, data = Data_merge_Model)
summary(MR29)
AIC(MR29) # 64.92

MR30 <- gam(RatAY ~ Year.x*AvgMin, data = Data_merge_Model)
summary(MR30)
AIC(MR30) # 64.92

MR31 <- gam(RatAY ~ Month+AvgMin, data = Data_merge_Model)
summary(MR31)
AIC(MR31) # 86.92

MR32 <- gam(RatAY ~ Month*AvgMin, data = Data_merge_Model)
summary(MR32)
AIC(MR32) # 94.27

MR33 <- gam(RatAY ~ Year.x+PropLogL3, data = Data_merge_Model)
summary(MR33)
AIC(MR33) #63.97

MR34 <- gam(RatAY ~ Year.x*PropLogL3, data = Data_merge_Model)
summary(MR34)
AIC(MR34) #63.97

MR35 <- gam(RatAY ~ Month+PropLogL3, data = Data_merge_Model)
summary(MR35)
AIC(MR35) #78.11

MR36 <- gam(RatAY ~ Month*PropLogL3, data = Data_merge_Model)
summary(MR36)
AIC(MR36) #81.11

MR37 <- gam(RatAY ~ Month+PropLogG20, data = Data_merge_Model)
summary(MR37)
AIC(MR37) #85.0

MR38 <- gam(RatAY ~ Month*PropLogG20, data = Data_merge_Model)
summary(MR38)
AIC(MR38) #85.4

MR39 <- gam(RatAY ~ Year.x+PropLogG20, data = Data_merge_Model)
summary(MR39)
AIC(MR39) #63.48

MR40 <- gam(RatAY ~ Year.x*PropLogG20, data = Data_merge_Model)
summary(MR40)
AIC(MR40) #63.48


MR41 <- gam(CPUE_Count ~ Year.x+Highest_Temperature_C, data = Data_merge_Model)
summary(MR41)
AIC(MR41) #382.42

MR42 <- gam(CPUE_Count ~ Month+Highest_Temperature_C, data = Data_merge_Model)
summary(MR42)
AIC(MR42) #383.23

MR43 <- gam(CPUE_Count ~ Year.x+Lowest_Temperature_C, data = Data_merge_Model)
summary(MR43)
AIC(MR43) #362.07

MR44 <- gam(CPUE_Count ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR44)
AIC(MR44) #362.07

#M44 is intersing in terms of June, July, Agust Time period!

MR45 <- gam(CPUE_Count ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR45)
AIC(MR45) #392.29

MR46 <- gam(CPUE_Count ~ Year.x+AvgMin, data = Data_merge_Model)
summary(MR46)
AIC(MR43) #362.13

MR47 <- gam(CPUE_Count ~ Month+AvgMin, data = Data_merge_Model)
summary(MR47)
AIC(MR47) #371.48

#For M47, April through November is signifcant. 

MR48 <- gam(CPUE_Count ~ Year.x+AvgMax, data = Data_merge_Model)
summary(MR48)
AIC(MR48) #362.09

MR49 <- gam(CPUE_Count ~ Month+AvgMax, data = Data_merge_Model)
summary(MR49)
AIC(MR49) #362.13

#For M49, April through October are significant.

MR50 <- gam(CPUE_Count ~ Month*AvgMax, data = Data_merge_Model)
summary(MR50)
AIC(MR50) #362.13

MR51 <- gam(CPUE_Count ~ Year.x+PropLogL3, data = Data_merge_Model)
summary(MR51)
AIC(MR51) #362.16

MR52 <- gam(CPUE_Count ~ Month+PropLogL3, data = Data_merge_Model)
summary(MR52)
AIC(MR52) #381.85

MR53 <- gam(CPUE_Count ~ Month+PropLogL5, data = Data_merge_Model)
summary(MR53)
AIC(MR53) #378.06

MR54 <- gam(CPUE_Count ~ Year.x+PropLogL5, data = Data_merge_Model)
summary(MR54)
AIC(MR54) #362.37

MR55 <- gam(CPUE_Count ~ Year.x+PropLogG20, data = Data_merge_Model)
summary(MR55)
AIC(MR55) #357.18

MR56 <- gam(CPUE_Count ~ Month+PropLogG20, data = Data_merge_Model)
summary(MR56)
AIC(MR56) #377.63


#####################

#Questions:
#When should use Year.x and Year.y?
#Any other models I should try?


mod1 <- gam(Highest_Temperature_C~Year+Month, data=STPred)
summary(mod1)
AIC(mod1)






