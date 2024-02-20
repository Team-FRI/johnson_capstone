#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Read Both BKTVar and STDPred csv's

BKTVar <- read.csv("BKTVar.csv") 
STPred <- read.csv("STPred.csv")

View(BKTVar)

install.packages("tidyverse")
library(tidyverse)

install.packages("broom")
library(broom)

#Y variable first, then x!

#First we have to merge the data frame to get all columsn in the same data frame through a unique identifier. 
#That unique identifier column is "SiteCode"

Data_merge <- merge(BKTVar, STPred, by = "SiteCode")

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