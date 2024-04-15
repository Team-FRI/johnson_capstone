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
AIC(mod1) #372.46; with updated QC1 550.56; with updated QC2 779.55: with updated QC 3 1247.12

mod2 <- gam(Highest_Temperature_C~Year+s(SiteCode, bs = 're'), data=STPred)#double check reason for error
summary(mod2)
AIC(mod)

mod2 <- gam(Highest_Temperature_C~Year+SiteCode, data=STPred)
summary(mod2)
AIC(mod2)#629.2585; with updated QC1 910.00; with updated QC2 1138.74; with updated QC 3 1847.81


mod3 <- gam(Highest_Temperature_C~Year*SiteCode, data=STPred)
summary(mod3)
AIC(mod3)#631.99; with updated QC1 919.21; with updated QC2 1149.46; with updated QC 3 1817.828


mod4 <- gam(Highest_Temperature_C~Month+SiteCode, data=STPred)
summary(mod4)
AIC(mod4)#356.3311; with updated QC1 529.76; with updated QC2; 727.29; 1179.12

mod5 <- gam(Highest_Temperature_C~Month*SiteCode, data=STPred)
summary(mod5)
AIC(mod5)#283.9091....super extra overfit ; with updated QC1 439.06; with updated QC2 574.04; 1127.78

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



#Between 14 and 18 degrees Celsius. 
#One big outlier at 21 degrees Celsius. Very intersting and seems to be the same site in most of the graphs.

#Now doing boxplots doing March,  April, March_April, June, July, August, June_July_August.
#As well as new data!

boxplot(Avg_YM_TempC~Year+SiteCode, data=March)

#Most are between 3 and 5. Some outliers down low.

boxplot(Avg_YM_TempC~Year*SiteCode, data=March)

#Exact Same as above. Most are between 3 and 5. Some outliers down low.

boxplot(Avg_YM_TempC~Month+SiteCode, data=March)

#Exact Same as above. Most are between 3 and 5. Some outliers down low.

boxplot(Avg_YM_TempC~Month*SiteCode, data=March)

#No interaction?

boxplot(Avg_YM_TempC~Year+SiteCode, data=April)

#Lots between 7 and 8. Two just above 6.5. One outlier down low. 

boxplot(Avg_YM_TempC~Year*SiteCode, data=April)

#No interaction

boxplot(Avg_YM_TempC~Month+SiteCode, data=April)

#Same as above

boxplot(Avg_YM_TempC~Year+SiteCode, data=March_April)

#Most overlap between 3 and 8. Three outliers bellow zero. 

boxplot(Avg_YM_TempC~Year*SiteCode, data=March_April)

#Same as above

boxplot(Avg_YM_TempC~Month+SiteCode, data=March_April)

boxplot(Avg_YM_TempC~Year+SiteCode, data=June)

#Three areas of groping. One between 3 and 4. One around 5 and 6. and one between 6 and 8.
#Three outliers down low. 

boxplot(Avg_YM_TempC~Month+SiteCode, data=June)

#Most between 12 and 14. Some higher at 16. 
#One really high sight at 18 which is inserting.

boxplot(Avg_YM_TempC~Year+SiteCode, data=July)

#Kind of a dowward curve and  an upward curve from 16. 
#Odd looking. 
#One outlier down low and one above 20 up high (That is very interesting)

boxplot(Avg_YM_TempC~Month+SiteCode, data=July)

#Kind of all over the place.
#Coupple between 18 and 20 could be intersting. 

boxplot(Avg_YM_TempC~Year+SiteCode, data=August)

#Three areas of grouping. Some at 15 and. Some between 16 and 18. and a few above 18. 
#one at 20 which could be interesting. 

boxplot(Avg_YM_TempC~Month+SiteCode, data=August)

#Most bewteen 15 and 18. 
#One bewten 19 and 20 which is interesting. 
#One low at 13 which is also interesting and could possibly be an example of a pristene site. 

boxplot(Avg_YM_TempC~Year+SiteCode, data=June_July_August)

#LOL this is complicated looking.
#Downward curving line from 14 to 12 between sites.
#Upward curving line from 14 to 17 between sites.
#Same site seems to be at 2o again which is interesting.

boxplot(Avg_YM_TempC~Month+SiteCode, data=June_July_August)

#All over the place. 
#Some between 18 and 20 which is interesting.
#Some down low at 13 and 12 which is also interesting and again could be pristine cold water sites during hot summer months. 

boxplot(PropLogL3~Year+SiteCode, data=March)

#Most in middle. 
#Some outloers up top and down low.

boxplot(PropLogL3~Month+SiteCode, data=March)

#Same as above

boxplot(PropLogL3~Year+SiteCode, data=April)

#Lots down low. 
#one outlier up top.

boxplot(PropLogL3~Month+SiteCode, data=April)

#Same as above

boxplot(PropLogL3~Year+SiteCode, data=March_April)

#Most between 0.0 and 0.6.
#3 outliers at 1.

boxplot(PropLogL3~Month+SiteCode, data=March_April)

#Most at 0 and bellow 0.2
#Some between 0.4 and 0.6.
#A few outliers up top.

boxplot(PropLogL3~Year+SiteCode, data=June)

#All 0's.

boxplot(PropLogL3~Month+SiteCode, data=June)

#All 0's.

boxplot(PropLogL3~Year+SiteCode, data=July)

#All 0's.

boxplot(PropLogL3~Month+SiteCode, data=July)

#All 0's.

boxplot(PropLogL3~Year+SiteCode, data=August)

#All 0's.

boxplot(PropLogL3~Year+SiteCode, data=August)

#All 0's.

boxplot(PropLogL3~Year+SiteCode, data=June_July_August)

#All 0's.

boxplot(PropLogL3~Month+SiteCode, data=August)

#All 0's.

boxplot(PropLogL4.5~Year+SiteCode, data=March)

#Most betwen 0.6 and 0.8. 
#3 at 1. 
#4 betwen 0.4 and 0.2

boxplot(PropLogL4.5~Month+SiteCode, data=March)

#Same as above. 

boxplot(PropLogL4.5~Year+SiteCode, data=April)

#Same as above.

boxplot(PropLogL4.5~Month+SiteCode, data=April)

#Same as above. 

boxplot(PropLogL4.5~Year+SiteCode, data=March_April)

#Most between 0.1 and 0.7. Mosst also overlap. 
#3 at 1.0.

boxplot(PropLogL4.5~Month+SiteCode, data=April)

#A few between 0.10 and 0.20. One outlier at 0.35 which is interesting. 

boxplot(PropLogL4.5~Year+SiteCode, data=June)

#All 0's.

boxplot(PropLogL4.5~Month+SiteCode, data=June)

#All 0's.

boxplot(PropLogL4.5~Year+SiteCode, data=July)

#All 0's.

boxplot(PropLogL4.5~Month+SiteCode, data=July)

#All 0's.

boxplot(PropLogL4.5~Year+SiteCode, data=August)

#All 0's.

boxplot(PropLogL4.5~Year+SiteCode, data=August)

#All 0's.

boxplot(PropLogL4.5~Year+SiteCode, data=August)

#All 0's.

boxplot(PropLogL4.5~Year+SiteCode, data=June_July_August)

#All 0's.

boxplot(PropLogL4.5~Month+SiteCode, data=June_July_August)

#All 0's.

boxplot(PropLogL5~Year+SiteCode, data=March)

#Kind of all over the place. 
#Three outliers up top at 1.0

boxplot(PropLogL5~Month+SiteCode, data=March)

#Line of them bweteen 0.7 and 0.8.
#Three outliers up top at 1.0

boxplot(PropLogL5~Year+SiteCode, data=April)

#One outier above 0.6. 
#Most bewteen 0.1 and 0.2. 

boxplot(PropLogL5~Year+SiteCode, data=March_April)

#Lots of overlaping between 0.2 and 0.7!
#Three outliers up top at 1.0

boxplot(PropLogL5~Month+SiteCode, data=March_April)

#All over the place.

boxplot(PropLogL5~Year+SiteCode, data=June)

#All 0's.

boxplot(PropLogL5~Month+SiteCode, data=June)

#All 0's.

boxplot(PropLogL5~Year+SiteCode, data=July)

#All 0's

boxplot(PropLogL5~Month+SiteCode, data=July)

#All 0's.

boxplot(PropLogL5~Year+SiteCode, data=August

#All 0's.

boxplot(PropLogL5~Month+SiteCode, data=August)

#All 0's.


boxplot(PropLogL5~Year+SiteCode, data=June_July_August)

#All 0's.

boxplot(PropLogL5~Month+SiteCode, data=June_July_August)

#All 0's.


boxplot(PropLogG20~Year+SiteCode, data=March)

#All 0's.

boxplot(PropLogG20~Month+SiteCode, data=March)

#All 0's.

boxplot(PropLogG20~Year+SiteCode, data=April)

#All 0's.

boxplot(PropLogG20~Month+SiteCode, data=April)

#All 0's.

boxplot(PropLogG20~Year+SiteCode, data=March_April)

#All 0's.

boxplot(PropLogG20~Month+SiteCode, data=March_April)

#All 0's.

boxplot(PropLogG20~Year+SiteCode, data=June)

#All 0's.

boxplot(PropLogG20~Month+SiteCode, data=June)

#One at 0.25 that has big spread between 0.25 and 0.75 quartiles. 
#One little itty bittty one down low. <0.5.

boxplot(PropLogG20~Year+SiteCode, data=July)

#One outliers at 0.7. Few small ones bweteen 0.0 and 0.1.

boxplot(PropLogG20~Month+SiteCode, data=July)

#One big one spaning from 0.0 to 0.7.

boxplot(PropLogG20~Year+SiteCode, data=August)

#One between 0.1 and 0.2.
#One outlier above 0.4. 

boxplot(PropLogG20~Month+SiteCode, data=August)

#One spaning from 0.2 to 0.4.

boxplot(PropLogG20~Year+SiteCode, data=June_July_August)

#One biger one ta 0.5. 
#One Smaller ones at 0.1.
#Few tiny ones on very bottom. 

boxplot(PropLogG20~Month+SiteCode, data=June_July_August)

#Same as above. 

boxplot(PropLogG24~Year+SiteCode, data=March)

#All 0's.

boxplot(PropLogG24~Month+SiteCode, data=March)

#All 0's.

boxplot(PropLogG24~Year+SiteCode, data=April)

#All 0's.

boxplot(PropLogG24~Year+SiteCode, data=April)

#All 0's.


boxplot(PropLogG24~Year+SiteCode, data=March_April)

#All 0's.


boxplot(PropLogG24~Month+SiteCode, data=March_April)

#All 0's.

boxplot(PropLogG24~Year+SiteCode, data=March_April)

#All 0's.

boxplot(PropLogG24~Year+SiteCode, data=June)

#One outlier at 0.0010 wich is interesting. 

boxplot(PropLogG24~Month+SiteCode, data=June)

#One extending from 0.00 to 0.0010. See if it is same site as one above.

boxplot(PropLogG24~Year+SiteCode, data=July)

#One outlier at 0.0010 wich is interesting. 

boxplot(PropLogG24~Month+SiteCode, data=July)

#One extending from 0.00 to 0.0010. See if it is same site as one above.

boxplot(PropLogG24~Year+SiteCode, data=August)

#One outlier above 0.03. 

boxplot(PropLogG24~Month+SiteCode, data=August)

#One extedning from bottom to 0.03.

boxplot(PropLogG24~Year+SiteCode, data=June_July_August)

#One at 0.03 that extends all the way up to 0.07.  

boxplot(PropLogG24~Month+SiteCode, data=June_July_August)

#That's really interesting that only those three sites all right next to eachohter pop out. 


mod6 <- gam(Lowest_Temperature_C ~ Month + SiteCode, data=STPred) 
summary(mod6)
AIC(mod6)#384.78; with updated QC1 536.6349; with updated QC2 695.03: With qc 3 1151.37

mod7 <- gam(Lowest_Temperature_C~Month*SiteCode, data=STPred)
summary(mod7)
AIC(mod7)#427.66....super extra overfit; with updated QC1 614.4394m; with updated QC3 768.98; 1210.68


mod8 <- gam(AvgMax~Month+SiteCode, data=STPred)
summary(mod8)
AIC(mod8)#346.03; with updated QC2 668.23; With qc 3 1016.03

mod9 <- gam(AvgMax~Month*SiteCode, data=STPred)
summary(mod9)
AIC(mod9)#278.73....super extra overfit; with updated QC2 550.33; With qc 3 889.34


mod10 <- gam(AvgMin~Month+SiteCode, data=STPred)
summary(mod10)
AIC(mod10)#339.86; with updated QC2 618.38; With qc 3 959.18

mod11 <- gam(AvgMin~Month*SiteCode, data=STPred)
summary(mod11)
AIC(mod11)#276.30....super extra overfit; 505.98; With qc 3 815.29

M1 <- gam(Avg_YM_TempC~Month+SiteCode, data=STPred)
summary(M1)
AIC(M1) #773.31; With qc 3 977.26

M2 <- gam(Avg_YM_TempC~Month*SiteCode, data=STPred)
summary(M2)
AIC(M2) #602.65; With qc 3 834.82

M3 <- gam(Avg_YM_TempC~Year+SiteCode, data=STPred)
summary(M3)
AIC(M3) #1343.696; With qc 3 1838.58

M4 <- gam(Avg_YM_TempC~Year*SiteCode, data=STPred)
summary(M4)
AIC(M4) #1349.25; With qc 3  1820

M7 <- gam(PropLogL3~Month+SiteCode, data=STPred)
summary(M7)
AIC(M7) #-200.53; With qc 3 -309.68

M8 <- gam(PropLogL3~Month*SiteCode, data=STPred)
summary(M8)
AIC(M8) #-460.30; -514.85

M5 <- gam(PropLogL3~Year+SiteCode, data=STPred)
summary(M5)
AIC(M5) #121.36; With qc 3 196.47

M6 <- gam(PropLogL3~Year*SiteCode, data=STPred)
summary(M6)
AIC(M6) #110.36; 166.24

M9 <- gam(PropLogL4.5~Month+SiteCode, data=STPred)
summary(M9)
AIC(M9) #-311.95; With qc 3 -464.15

M10 <- gam(PropLogL4.5~Month*SiteCode, data=STPred)
summary(M10)
AIC(M10) #-706.26; With qc 3 -720.79

M11 <- gam(PropLogL4.5~Year+SiteCode, data=STPred)
summary(M11)
AIC(M11) #174.45; With qc 3 276.13

M12 <- gam(PropLogL4.5~Year*SiteCode, data=STPred)
summary(M12)
AIC(M12) #179.14; With qc 3 260.14

M13 <- gam(PropLogL5~Month+SiteCode, data=STPred)
summary(M13)
AIC(M13) #-329.07; With qc 3 -488.14

M14 <- gam(PropLogL5~Month*SiteCode, data=STPred)
summary(M14)
AIC(M14) #-734.91; With qc 3 -727.74

M15 <- gam(PropLogL5~Year+SiteCode, data=STPred)
summary(M15)
AIC(M15) #194.50; With qc 3303.13

M16 <- gam(PropLogL5~Year*SiteCode, data=STPred)
summary(M16)
AIC(M16) #201.90; With qc 3 290.11

M17 <- gam(PropLogG20~Month+SiteCode, data=STPred)
summary(M17)
AIC(M17) #-599.48; With qc 3 -884.02

M18 <- gam(PropLogG20~Month*SiteCode, data=STPred)
summary(M18)
AIC(M18) #-493.39; With qc 3 -772.35

M19 <- gam(PropLogG20~Year+SiteCode, data=STPred)
summary(M19)
AIC(M19) #-609.90; With qc 3 -886.64

M20 <- gam(PropLogG20~Year*SiteCode, data=STPred)
summary(M20)
AIC(M20) #-638.58; With qc 3 -927.25

M21 <- gam(PropLogG24~Month+SiteCode, data=STPred)
summary(M21)
AIC(M21) #-1441.47; -2020.84

M22 <- gam(PropLogG24~Month*SiteCode, data=STPred)
summary(M22)
AIC(M22) #-1274.81; -1819.38

M23 <- gam(PropLogG24~Year+SiteCode, data=STPred)
summary(M23)
AIC(M23) #-1455.38; -2033.12

M24 <- gam(PropLogG24~Year*SiteCode, data=STPred)
summary(M24)
AIC(M24) #-1460.24: With qc 3 -2044.87


mod12 <- gam(RatAY~Year+SiteCode, data=BKTVar)
summary(mod12)
AIC(mod12)#105.20; With all years 499.41

mod13 <- gam(RatAY~Year*SiteCode, data=BKTVar)
summary(mod13)
AIC(mod13)#68.78; With all years 537.38



Data_merge <- merge(BKTVar, STPred, by = c("SiteCode", "Year"), all = FALSE)

colnames(Data_merge)
mod14 <- gam(RatAY~Year, data=Data_merge)
summary(mod14)
AIC(mod14)#295.85; With all years 1980.35; With 3/29 added qc sites: 4879.35: After adjusting data merge 262.69 

mod15 <- gam(RatAY~Year*Highest_Temperature_C, data=Data_merge)
summary(mod15)
AIC(mod15)#235.28; With all years 1583.071: With 3/29 added qc sites: 4707.27; After adjusting data merge 264.69 

mod16 <- gam(RatAY~Year*Lowest_Temperature_C, data=Data_merge)
summary(mod16)
AIC(mod16)#249.42; With all years 1582.182; with updated QC2 2930.87: With 3/29 added qc sites:4709.22; After adjusting data merge 266.57.

mod19 <- gam(RatAY~Year*AvgMax, data=Data_merge)
summary(mod19)
AIC(mod19) #With 3/29 added qc sites:4708.84; 265.06

mod20 <- gam(RatAY~Year+AvgMax, data=Data_merge)
summary(mod20)
AIC(mod20) #With 3/29 added qc sites:4706.889: After adjusting data merge 264.62

mod21 <- gam(RatAY~Year*AvgMin, data=Data_merge)
summary(mod21)
AIC(mod21) #With 3/29 added qc sites:4709.38: After adjusting data merge 264.96 

mod22 <- gam(RatAY~Year+AvgMin, data=Data_merge)
summary(mod22)
AIC(mod22) #With 3/29 added qc sites:4706.88: After adjusting data merge 264.51

mod23 <- gam(RatAY~Year+Avg_YM_TempC, data=Data_merge)
summary(mod23)
AIC(mod23) #With 3/29 added qc sites:4707.43: After adjusting data merge 264.59

mod24 <- gam(RatAY~Year*Avg_YM_TempC, data=Data_merge)
summary(mod24)
AIC(mod24) #With 3/29 added qc sites:4709.32: After adjusting data merge 265.01 

mod25 <- gam(RatAY~Year+PropLogL3, data=Data_merge)
summary(mod25)
AIC(mod25) #With 3/29 added qc sites:4645.63: After adjusting data merge 262.82 

mod26 <- gam(RatAY~Year*PropLogL3, data=Data_merge)
summary(mod26)
AIC(mod26) #With 3/29 added qc sites:4647.58

mod27 <- gam(RatAY~Year+PropLogG20, data=Data_merge)
summary(mod27)
AIC(mod27) #With 3/29 added qc sites:4686.89: After adjusting data merge 263.03

mod28 <- gam(RatAY~Year*PropLogG20, data=Data_merge)
summary(mod28)
AIC(mod28) #With 3/29 added qc sites:4688.85; After adjusting data merge 263.03

#With interaction, its not signifcant anymore as it was with additve for mod27.


mod17 <- gam(RatAY~Year.x+Lowest_Temperature_C, data=Data_merge)
summary(mod17)
AIC(mod17)#228.26; With all years 1580.324; with updated QC2 2928.93:  With 3/29 added qc sites:4707.402

#scratchpad
mod18 <- gam(CPUE_Biomass~Year*AvgMax, data=Data_merge)
summary(mod18)
AIC(mod18)#228.29; With all years 13059.76; with updated QC2 23027.43: After adjusting data merge 2262.35

mod29 <- gam(CPUE_Biomass~Year+AvgMax, data=Data_merge)
summary(mod29)
AIC(mod29) # After adjusting data merge 2264.35

#No longer signifcant without interaction

mod30 <- gam(CPUE_Count~Year*AvgMax, data=Data_merge)
summary(mod30)
AIC(mod30)#228.29; With all years 8927.68; with updated QC2  14498.23: After adjusting data merge 1611.74

mod31 <- gam(CPUE_Count~Year+AvgMax, data=Data_merge)
summary(mod31)
AIC(mod31)#228.29; With all years 8927.68; with updated QC2  14498.23: After adjusting data merge 1612.56



