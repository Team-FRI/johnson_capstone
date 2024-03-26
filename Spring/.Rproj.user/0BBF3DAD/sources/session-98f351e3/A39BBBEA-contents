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

install.packages("lubridate")
library(lubridate)

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
AIC(mod1) #372.46; with updated QC1 550.56; with updated QC2 779.55

mod2 <- gam(Highest_Temperature_C~Year+s(SiteCode, bs = 're'), data=STPred)#double check reason for error
summary(mod2)
AIC(mod)

mod2 <- gam(Highest_Temperature_C~Year+SiteCode, data=STPred)
summary(mod2)
AIC(mod2)#629.2585; with updated QC1 910.00; with updated QC2 1138.74


mod3 <- gam(Highest_Temperature_C~Year*SiteCode, data=STPred)
summary(mod3)
AIC(mod3)#631.99; with updated QC1 919.21; with updated QC2 1149.46


mod4 <- gam(Highest_Temperature_C~Month+SiteCode, data=STPred)
summary(mod4)
AIC(mod4)#356.3311; with updated QC1 529.76; with updated QC2; 727.29

mod5 <- gam(Highest_Temperature_C~Month*SiteCode, data=STPred)
summary(mod5)
AIC(mod5)#283.9091....super extra overfit ; with updated QC1 439.06; with updated QC2 574.04

#March <- subset()
#boxplot(Highest_Temperature_C~Month+SiteCode, data=March)
#Look at temps for these months and compare to p-values by site interaction

#(March) April, June July August 

#Subset the data by months your interested in. 

March <- subset(STPred, Month == "03")

April <- subset(STPred, Month == "04")

March_April <- subset(STPred, Month == "03" | Month == "04")

June <- subset(STPred, Month == "06")

July <- subset(STPred, Month == "07")

August <- subset(STPred, Month == "08")

June_July_August <- subset(STPred, Month == "06" | Month == "07" | Month == "08")

#Make Box plots for Highest Temperature_C

boxplot(Highest_Temperature_C~Month+SiteCode, data=March)

boxplot(Lowest_Temperature_C~Month+SiteCode, data=March)

boxplot(Highest_Temperature_C~SiteCode, data=March_April)

boxplot(Highest_Temperature_C~Month, data=March_April)

boxplot(Lowest_Temperature_C~SiteCode, data=March_April)

boxplot(Lowest_Temperature_C~Month, data=March_April)

boxplot(Highest_Temperature_C~Month+SiteCode, data=April)

boxplot(Lowest_Temperature_C~Month+SiteCode, data=April)

boxplot(Highest_Temperature_C~Month+SiteCode, data=June)

boxplot(Lowest_Temperature_C~Month+SiteCode, data=June)

boxplot(Highest_Temperature_C~Month+SiteCode, data=July)

boxplot(Lowest_Temperature_C~Month+SiteCode, data=July)

boxplot(Highest_Temperature_C~Month+SiteCode, data=August)

boxplot(Lowest_Temperature_C~Month+SiteCode, data=August)

boxplot(Highest_Temperature_C~Month, data=June_July_August)

boxplot(Highest_Temperature_C~SiteCode, data=June_July_August)

boxplot(Lowest_Temperature_C~Month, data=June_July_August)

boxplot(Lowest_Temperature_C~SiteCode, data=June_July_August)

#Make Box plots for AvgMin and AvgMax

boxplot(AvgMax~Month+SiteCode, data=March)

#Yellow still is low. 
#All others generally between 4 and 6.

boxplot(AvgMin~Month+SiteCode, data=March)

#Yellow still low.
#Two Trends here. one (middle) line between 2 and 3. Other (Top) between 4 and 5). Maybe that telling us that sites are acting different from one another. Maybe the middle line is more groundwater fed?

boxplot(AvgMax~SiteCode, data=March_April)

#Yellow still low.
#Most overlap so its generally consistent. A few don't. 

boxplot(AvgMin~Month, data=March_April)

#March is lower than April, What you would expect.

boxplot(AvgMax~SiteCode, data=March_April)

#Yellow is still low.
#All generally in a consistent line. 

boxplot(AvgMin~Month, data=March_April)

#March is lower than April. What you would expect. 

boxplot(AvgMax~Month+SiteCode, data=April)

#A little bit all over the place. 

boxplot(AvgMin~Month+SiteCode, data=April)

#Kind of two negative sloping relationships and then the one outlier. Again maybe some sites more groundwater influcenced than others within the two lines. 

boxplot(AvgMax~Month+SiteCode, data=June)

#Some grouping at the bottom left between twelve and 16 degrees. 
#Very big outlier at 20 degrees which could be interesting cause thermal stress and Brook Trout. 

boxplot(AvgMin~Month+SiteCode, data=June)

#Literally the same thing as above....
#Some grouping at the bottom left between twelve and 14.5 degrees. 
#Very big outlier at 16 to 17 degrees and a few at 15 which could be interesting for AvgMin.

boxplot(AvgMax~Month+SiteCode, data=July)

#Most between 16 and 18, but thats still pretty high!
#One big outlier just bellow 22 degrees which is very interesting. 

boxplot(AvgMin~Month+SiteCode, data=July)

#Two main areas of Grouping. 
#Multiple just bellow 18 degrres C. Thats interesting for AvgMin. 
#One outlier down bellow 13 degrees C.

boxplot(AvgMax~Month+SiteCode, data=August)

#Again it seems like one outlier at 21 degres C is the same site consistently. Look into which site that is. 
#Rest generally between 16 and 18 degrees C. 

boxplot(AvgMin~Month+SiteCode, data=August)

#2-3 at 18 degrees C. Decent amount at 17 and 14.5 degrees C. 
#outlier ish down low. 

boxplot(AvgMax~Month, data=June_July_August)

#Pretty much what you'd expect. June lower and July and August about the same. 

boxplot(AvgMax~SiteCode, data=June_July_August)

#One big outlier at 21 degrees celcius. Thats interesting!
#Lots between 14 and 18 degrees celcius. 

boxplot(AvgMin~Month, data=June_July_August)

#Again what you'd expect for AvgMin. Lower in June. July and August. 

boxplot(AvgMin~SiteCode, data=June_July_August)

#Between 14 and 18 degrees Celsius. 
#One big outlier at 21 degrees Celsius. Very intersting and seems to be the same site in most of the graphs.


mod6 <- gam(Lowest_Temperature_C~Month+SiteCode, data=STPred) 
summary(mod6)
AIC(mod6)#384.78; with updated QC1 536.6349; with updated QC2 695.03

mod7 <- gam(Lowest_Temperature_C~Month*SiteCode, data=STPred)
summary(mod7)
AIC(mod7)#427.66....super extra overfit; with updated QC1 614.4394m; with updated QC2 768.98


mod8 <- gam(AvgMax~Month+SiteCode, data=STPred)
summary(mod8)
AIC(mod8)#346.03; with updated QC2 668.23

mod9 <- gam(AvgMax~Month*SiteCode, data=STPred)
summary(mod9)
AIC(mod9)#278.73....super extra overfit; with updated QC2 550.33


mod10 <- gam(AvgMin~Month+SiteCode, data=STPred)
summary(mod10)
AIC(mod10)#339.86; with updated QC2 618.38

mod11 <- gam(AvgMin~Month*SiteCode, data=STPred)
summary(mod11)
AIC(mod11)#276.30....super extra overfit; 505.98

M1 <- gam(Avg_YM_TempC~Month+SiteCode, data=STPred)
summary(M1)
AIC(M1) #773.31

M2 <- gam(Avg_YM_TempC~Month*SiteCode, data=STPred)
summary(M2)
AIC(M2) #602.65

M3 <- gam(Avg_YM_TempC~Year+SiteCode, data=STPred)
summary(M3)
AIC(M3) #1343.696

M4 <- gam(Avg_YM_TempC~Year*SiteCode, data=STPred)
summary(M4)
AIC(M4) #1349.25.

M7 <- gam(PropLogL3~Month+SiteCode, data=STPred)
summary(M7)
AIC(M7) #-200.53

M8 <- gam(PropLogL3~Month*SiteCode, data=STPred)
summary(M8)
AIC(M8) #-460.30 

M5 <- gam(PropLogL3~Year+SiteCode, data=STPred)
summary(M5)
AIC(M5) #121.36

M6 <- gam(PropLogL3~Year*SiteCode, data=STPred)
summary(M6)
AIC(M6) #110.36

M9 <- gam(PropLogL4.5~Month+SiteCode, data=STPred)
summary(M9)
AIC(M9) #-311.95

M10 <- gam(PropLogL4.5~Month*SiteCode, data=STPred)
summary(M10)
AIC(M10) #-706.26

M11 <- gam(PropLogL4.5~Year+SiteCode, data=STPred)
summary(M11)
AIC(M11) #174.45

M12 <- gam(PropLogL4.5~Year*SiteCode, data=STPred)
summary(M12)
AIC(M12) #179.14

M13 <- gam(PropLogL5~Month+SiteCode, data=STPred)
summary(M13)
AIC(M13) #-329.07

M14 <- gam(PropLogL5~Month*SiteCode, data=STPred)
summary(M14)
AIC(M14) #-734.91

M15 <- gam(PropLogL5~Year+SiteCode, data=STPred)
summary(M15)
AIC(M15) #194.50

M16 <- gam(PropLogL5~Year*SiteCode, data=STPred)
summary(M16)
AIC(M16) #201.90

M17 <- gam(PropLogG20~Month+SiteCode, data=STPred)
summary(M17)
AIC(M17) #-599.48

M18 <- gam(PropLogG20~Month*SiteCode, data=STPred)
summary(M18)
AIC(M18) #-493.39

M19 <- gam(PropLogG20~Year+SiteCode, data=STPred)
summary(M19)
AIC(M19) #-609.90

M20 <- gam(PropLogG20~Year*SiteCode, data=STPred)
summary(M20)
AIC(M20) #-638.58

M21 <- G24

M22

M23

M24

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
AIC(mod16)#249.42; With all years 1582.182; with updated QC2 2930.87


mod17 <- gam(RatAY~Year.x+Lowest_Temperature_C, data=Data_merge)
summary(mod17)
AIC(mod17)#228.26; With all years 1580.324; with updated QC2 2928.93

#scratchpad
mod18 <- gam(CPUE_Biomass~Year.x*AvgMax, data=Data_merge)
summary(mod18)
AIC(mod18)#228.29; With all years 13059.76; with updated QC2 23027.43

mod18 <- gam(CPUE_Count~Year.x*AvgMax, data=Data_merge)
summary(mod18)
AIC(mod18)#228.29; With all years 8927.68; with updated QC2  14498.23

mod19 <- 

