#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Read Both BKTVar and STDPred csv's

#BKTVar <- read_csv("BKTVar.csv") #correct
STPred <- read_csv("STPred.csv") #correct
BKTVar<- read_csv("BKTVar_AllSites.csv") #Updated; Brook Trout for all years of Long Term Loyalsock Project. #Correct 


install.packages("tidyverse")
library(tidyverse)

install.packages("broom")
library(broom)


#Separate out/Reduce to 2020 and 2021 Data 

BKTVar2020 <- BKTVar %>%
  filter(str_detect(EventCode, "^2020"))

BKTVar2021 <- BKTVar %>%
  filter(str_detect(EventCode, "^2021"))

STPred2020 <- STPred %>%
  filter(Year == "2020")

STPred2021 <- STPred %>%
  filter(Year == "2021")

#QC filter STPred for other years and see how many we get.

STPredQC <- STPred %>%
  filter(Year == "2016"| Year == "2017"| Year == "2018"| Year == "2019")
#YAY it worked! :)

#Y variable first, then x!

#First we have to merge the data frame to get all columsn in the same data frame through a unique identifier. 
#That unique identifier column is "SiteCode"

Data_merge <- left_join(STPred2020, BKTVar2020, by = c("SiteCode", "Year"))

Data_merge1 <- left_join(STPred2021, BKTVar2021, by = c("SiteCode", "Year"))

#Need to add Year to this table so we can left join by Year and SiteCode

#Now we can start building models and running stats. 

#Lets start simple with one Y and one X. Don't violate the assumptions with our data so will be linear models

#First lets try CPUE_Count against Highest_Temperature_C

cpueC_HighT <- lm(CPUE_Count ~ Highest_Temperature_C, data = Data_merge) 

summary(cpueC_HighT)

plot(cpueC_HighT)

#P Value = 0.001744. Less than 0.05 so significant!

#Next Lets try CPUE_Count against Lowest_Temperature_C

cpueC_LowT <- lm(CPUE_Count ~ Lowest_Temperature_C, data = Data_merge)
summary(cpueC_LowT)

plot(cpueC_LowT)

#P value = 0.03791. Less than 0.05 so significant!

#Next Lets do CPUE_Count against AvgMin

cpueC_AvgMin <- lm(CPUE_Count ~ AvgMin, data = Data_merge)
summary(cpueC_AvgMin)

plot(cpueC_LowT)

#P value =  0.3122. More than 0.05 so not significant!

#Next Lets do CPUE_Count against AvgMax

cpueC_AvgMax <- lm(CPUE_Count ~ AvgMax, data = Data_merge)
summary(cpueC_AvgMax)

plot(cpueC_AvgMax)

#P value = 0.01295. Less than 0.05 so significant 

#Now lets move onto a new Y variable: CPUE_Biomass
#First lets try CPUE_Biomass against Highest Temp/ Year Month 

cpueB_HighT <- lm(CPUE_Biomass ~ Highest_Temperature_C, data = Data_merge)
summary(cpueB_HighT)

plot(cpueC_HighT)

# P value = 0.008814. Less than 0.05 so significant!

#Next lets do CPUE_Biomass against Lowest Temp/ Year Month 

cpueB_LowT <- lm(CPUE_Biomass ~ Lowest_Temperature_C, data = Data_merge)
summary(cpueB_LowT)

plot(cpueC_LowT)

#P value is  0.2367. Greater then 0.05 so not significant

#Next lets do CPUE_Biomass against AvgMin

cpueB_AvgMin <- lm(CPUE_Biomass ~ AvgMin, data = Data_merge)
summary(cpueB_AvgMin)

plot(cpueB_AvgMin)

#P value = 0.1848. Greater than 0.05 so not significant 

#Lastly for this Y variable, lets do CPUE_Biomass against AvgMax

cpueB_AvgMax <- lm(CPUE_Biomass ~ AvgMax, data = Data_merge)
summary(cpueB_AvgMax)

plot(cpueB_AvgMax)

#P value = 0.02139. Less than  0.05 so  significant!

#Now lets move onto our third and final response variable, RatAY
#First lets do Rat_AY against Highest_Temperature_C

RatAY_HighT <- lm(RatAY ~ Highest_Temperature_C, data = Data_merge)
summary(RatAY_HighT)

plot(RatAY_HighT)

#P Value = 0.5589. Greater than 0.05 so not significant 

#Next lets do RatAY against Lowest_Temperature_C

RatAY_LowT <- lm(RatAY ~ Lowest_Temperature_C, data = Data_merge)
summary(RatAY_LowT)

plot(RatAY_LowT)

#P value = 0.6892. Greater than 0.05 so not significant 

#Next Lets do RatAY against AvgMin

RatAy_AvgMin <- lm(RatAY ~ AvgMin, data = Data_merge)
summary(RatAy_AvgMin)

plot(RatAy_AvgMin)

#P value = 0.8367. Greater than 0.05 so not significant

#Laslty for this Y variable, lets do RatAy against AvgMax 

RatAy_AvgMax <- lm(RatAY ~ AvgMax, data = Data_merge)
summary(RatAy_AvgMax)

plot(RatAy_AvgMax)

#P value = 0.7135. Greater than 0.05 so not significant 

#The next step is going to be trying to make a GLM. 

GLM_CPUEC_HighT <- glm(CPUE_Count ~ Highest_Temperature_C, data = Data_merge, family = binomial())

GLM_CPUEC_HighT <- glm(CPUE_Count ~ Highest_Temperature_C, data = Data_merge, family = gaussian())



cpue_BTestAvgMax <- lm(BKTVar$CPUE_Biomass ~ STPred$AvgMax)



#MW from scratch:
install.packages("mgcv")
library(mgcv)
#+s(SiteCode, bs = 're')

mod1 <- gam(Highest_Temperature_C~Year+Month, data=STPred)
summary(mod1)
AIC(mod1) #372.46; with updated QC1 550.56

mod2 <- gam(Highest_Temperature_C~Year+s(SiteCode, bs = 're'), data=STPred)#double check reason for error
summary(mod2)
AIC(mod)

mod2 <- gam(Highest_Temperature_C~Year+SiteCode, data=STPred)
summary(mod2)
AIC(mod2)#629.2585; with updated QC1 910.00


mod3 <- gam(Highest_Temperature_C~Year*SiteCode, data=STPred)
summary(mod3)
AIC(mod3)#631.99; with updated QC1 919.21


mod4 <- gam(Highest_Temperature_C~Month+SiteCode, data=STPred)
summary(mod4)
AIC(mod4)#356.3311; with updated QC1 529.76

mod5 <- gam(Highest_Temperature_C~Month*SiteCode, data=STPred)
summary(mod5)
AIC(mod5)#283.9091....super extra overfit ; with updated QC1 439.06

#March <- subset()
#boxplot(Highest_Temperature_C~Month+SiteCode, data=March)
#Look at temps for these months and compare to p-values by site interaction


mod6 <- gam(Lowest_Temperature_C~Month+SiteCode, data=STPred)
summary(mod6)
AIC(mod6)#384.78; with updated QC1 536.6349

mod7 <- gam(Lowest_Temperature_C~Month*SiteCode, data=STPred)
summary(mod7)
AIC(mod7)#427.66....super extra overfit; with updated QC1 614.4394m


mod8 <- gam(AvgMax~Month+SiteCode, data=STPred)
summary(mod8)
AIC(mod8)#346.03

mod9 <- gam(AvgMax~Month*SiteCode, data=STPred)
summary(mod9)
AIC(mod9)#278.73....super extra overfit


mod10 <- gam(AvgMin~Month+SiteCode, data=STPred)
summary(mod10)
AIC(mod10)#339.86

mod11 <- gam(AvgMin~Month*SiteCode, data=STPred)
summary(mod11)
AIC(mod11)#276.30....super extra overfit



mod12 <- gam(RatAY~Year+SiteCode, data=BKTVar)
summary(mod12)
AIC(mod12)#105.20; With all years 499.41

mod13 <- gam(RatAY~Year*SiteCode, data=BKTVar)
summary(mod13)
AIC(mod13)#68.78; With all years 537.38



Data_merge <- merge(BKTVar, STPred, by = "SiteCode", all = TRUE)

colnames(Data_merge)
mod14 <- gam(RatAY~Year.x, data=Data_merge)
summary(mod14)
AIC(mod14)#295.85; With all years 1980.35

mod15 <- gam(RatAY~Year.x*Highest_Temperature_C, data=Data_merge)
summary(mod15)
AIC(mod15)#235.28; With all years 1583.071

mod16 <- gam(RatAY~Year.x*Lowest_Temperature_C, data=Data_merge)
summary(mod16)
AIC(mod16)#249.42; With all years 1582.182


mod17 <- gam(RatAY~Year.x+Lowest_Temperature_C, data=Data_merge)
summary(mod17)
AIC(mod17)#228.26; With all years 1580.324

#scratchpad
mod18 <- gam(CPUE_Biomass~Year.x*AvgMax, data=Data_merge)
summary(mod18)
AIC(mod18)#228.29; With all years 13059.76

mod18 <- gam(CPUE_Count~Year.x*AvgMax, data=Data_merge)
summary(mod18)
AIC(mod18)#228.29; With all years 8927.68

