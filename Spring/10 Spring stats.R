#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Read Both BKTVar and STDPred csv's

BKTVar <- read.csv("BKTVar.csv") 
STPred <- read.csv("STPred.csv")


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

GLM_CPUEC_HighT <- glm(CPUE_Count ~ Highest_Temperature_C, data = Data_merge, family = gaussian()



cpue_BTestAvgMax <- lm(BKTVar$CPUE_Biomass ~ STPred$AvgMax)
