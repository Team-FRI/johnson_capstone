#Set working Directory 

setwd("C:/GitHub/johnson_capstone/spring")

#Read Both BKTVar and STDPred csv's

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

install.packages("tidyverse")
library(tidyverse)

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

BKTVar2020_2021 <- BKTVar %>%
  filter(Year=="2020" | Year == "2021")

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

Data_merge_Model <- merge(BKTVar2021, STPred2, by = c("SiteCode"), all = FALSE)

#Data_merge_Model_Test  <- merge(BKTVar2021, STPred1, by = c("SiteCode", "Year"), all = FALSE)

#Data_merge-Model2 <- left_join(BKTVar2021, STPred1, by = c("SiteCode"), all = FALSE)

#Filter for 6 sites in BKTVar2021!!!!!!!! (Dont need to do anymore)

#Now its time to make some models. 

#Intsall package that reads gams

install.packages("mgcv")
library(mgcv)


MR1 <- gam(CPUE_Count ~ Year.x+Month, data = Data_merge_Model)
summary(MR1)
AIC(MR1) #382.42

MR2 <- gam(CPUE_Count ~ SiteCode*Highest_Temperature_C, data = Data_merge_Model)
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

#For MR8, June July August are Signifcant 

MR8.1 <- gam(CPUE_Count ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR8.1)
AIC(MR8.1) # 392.17

#Deviance explained went up from MR8.

MR8.2 <- gam(CPUE_Count ~ Lowest_Temperature_C, data = Data_merge_Model)
summary(MR8.2)
AIC(MR8.2) # 362.07

MR9 <- gam(CPUE_Count ~ Year.x+AvgMax, data = Data_merge_Model)
summary(MR9)
AIC(MR9) # 362.09

MR10 <- gam(CPUE_Count ~ AvgMax, data = Data_merge_Model)
summary(MR10)
AIC(MR10) # 362.09

MR11 <- gam(CPUE_Count ~ Month+AvgMax, data = Data_merge_Model)
summary(MR11)
AIC(MR11) # 373.09

#Lots of months for MR11 are significant for MR11 and deviance explained is 19%.

MR12 <- gam(CPUE_Count ~ Month*AvgMax, data = Data_merge_Model)
summary(MR12)
AIC(MR12) # 382.76

#MR12, deviance explained is 36.6%

MR13 <- gam(CPUE_Count ~ Year.x+AvgMin, data = Data_merge_Model)
summary(MR13)
AIC(MR13) # 362.09

MR14 <- gam(CPUE_Count ~ AvgMin, data = Data_merge_Model)
summary(MR14)
AIC(MR14) # 362.13

MR15 <- gam(CPUE_Count ~ Month+AvgMin, data = Data_merge_Model)
summary(MR15)
AIC(MR15) # 371.48

#For MR15, lots of months and average min is significant. 

MR16 <- gam(CPUE_Count ~ Month*AvgMin, data = Data_merge_Model)
summary(MR16)
AIC(MR16) # 386.76

#For MR16, Deviance explained is 31.8%

MR16.1 <- gam(CPUE_Count ~ Month, data = Data_merge_Model)
summary(MR16.1)
AIC(MR16.1) # 382.46

MR17 <- gam(RatAY ~ Year.x+Highest_Temperature_C, data = Data_merge_Model)
summary(MR17)
AIC(MR17) # 64.76

MR18 <- gam(RatAY ~ Highest_Temperature_C, data = Data_merge_Model)
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

#MR22 <- gam(RatAY ~ Year.x*Highest_Temperature_C, data = Data_merge_Model)
#summary(MR22)
#AIC(MR22) # 64.76

MR23 <- gam(RatAY ~ Month+Lowest_Temperature_C, data = Data_merge_Model)
summary(MR23)
AIC(MR23) # 82.18

#Lowest Tempt almost signifcant.

MR24 <- gam(RatAY ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR24)
AIC(MR24) # 93.23

MR25 <- gam(RatAY ~ Year.x+AvgMax, data = Data_merge_Model)
summary(MR25)
AIC(MR25) # 64.92

MR26 <- gam(RatAY ~ AvgMax, data = Data_merge_Model)
summary(MR26)
AIC(MR26) # 64.92

MR27 <- gam(RatAY ~ Month+AvgMax, data = Data_merge_Model)
summary(MR27)
AIC(MR27) # 86.92

MR28 <- gam(RatAY ~ Month*AvgMax, data = Data_merge_Model)
summary(MR28)
AIC(MR28) # 87.96

#May and June almost significant for the interaction. 

MR29 <- gam(RatAY ~ Year.x+AvgMin, data = Data_merge_Model)
summary(MR29)
AIC(MR29) # 64.92

MR30 <- gam(RatAY ~ AvgMin, data = Data_merge_Model)
summary(MR30)
AIC(MR30) # 64.92

MR31 <- gam(RatAY ~ Month+AvgMin, data = Data_merge_Model)
summary(MR31)
AIC(MR31) # 86.92

#AvgMin almost signifcant  for M31

MR32 <- gam(RatAY ~ Month*AvgMin, data = Data_merge_Model)
summary(MR32)
AIC(MR32) # 94.27

MR32.1 <- gam(RatAY ~ Month, data = Data_merge_Model)
summary(MR32.1)
AIC(MR32.1) # 84.94

MR32.2 <- gam(RatAY ~ SiteCode, data = Data_merge_Model)
summary(MR32.2)
AIC(MR32.2) # 84.94

MR33 <- gam(RatAY ~ Year.x+PropLogL3, data = Data_merge_Model)
summary(MR33)
AIC(MR33) #63.97

MR34 <- gam(RatAY ~ PropLogL3, data = Data_merge_Model)
summary(MR34)
AIC(MR34) #63.97

MR35 <- gam(RatAY ~ Month+PropLogL3, data = Data_merge_Model)
summary(MR35)
AIC(MR35) #78.11

#MR35 June, January, and February are almost significant. PropLogL3 is significant. 

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

MR40 <- gam(RatAY ~ PropLogG20, data = Data_merge_Model)
summary(MR40)
AIC(MR40) #63.48

MR41 <- gam(CPUE_Biomass ~ Year.x+Highest_Temperature_C, data = Data_merge_Model)
summary(MR41)
AIC(MR41) #668.42

MR42 <- gam(CPUE_Biomass ~ Month+Highest_Temperature_C, data = Data_merge_Model)
summary(MR42)
AIC(MR42) #688.61

MR43 <- gam(CPUE_Biomass ~ Year.x+Lowest_Temperature_C, data = Data_merge_Model)
summary(MR43)
AIC(MR43) #668.67

MR44 <- gam(CPUE_Biomass ~ Month+Lowest_Temperature_C, data = Data_merge_Model)
summary(MR44)
AIC(MR44) #685.89

#M44 is inserting in terms of lowest temp being almost significant as well as June July August
#January is significant. 

MR45 <- gam(CPUE_Biomass ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR45)
AIC(MR45) #683.93

#January is still significant, but everything else becomes less signifcant. 

MR46 <- gam(CPUE_Biomass ~ Year.x+AvgMin, data = Data_merge_Model)
summary(MR46)
AIC(MR43) #668.67

MR47 <- gam(CPUE_Biomass ~ Month+AvgMin, data = Data_merge_Model)
summary(MR47)
AIC(MR47) #668.70

#M47 is inserting in terms of lowest temp being almost significant as well as June July August. 
#January is significant.

MR48 <- gam(CPUE_Biomass~ Year.x+AvgMax, data = Data_merge_Model)
summary(MR48)
AIC(MR48) #668.94

MR49 <- gam(CPUE_Biomass ~ Month+AvgMax, data = Data_merge_Model)
summary(MR49)
AIC(MR49) #690.06

#For M49, January is significant.

MR50 <- gam(CPUE_Biomass ~ Month*AvgMax, data = Data_merge_Model)
summary(MR50)
AIC(MR50) #687.28

#For M50, jaunary, May and, June are significant. April is almost significant. 

MR51 <- gam(CPUE_Biomass ~ Year.x+PropLogL3, data = Data_merge_Model)
summary(MR51)
AIC(MR51) #667.80

MR52 <- gam(CPUE_Biomass ~ Month+PropLogL3, data = Data_merge_Model)
summary(MR52)
AIC(MR52) #680.19

#February and June are signifcant. ProplogL3 is significant. Lots of months are almost signficant. 

MR52.1 <- gam(CPUE_Biomass ~ PropLogL3, data = Data_merge_Model)
summary(MR52.1)
AIC(MR52.1) #667.80

MR53 <- gam(CPUE_Count ~ Month+PropLogL5, data = Data_merge_Model)
summary(MR53)
AIC(MR53) #378.06

#For MR53, April through November are sigifcant. ProplogL5 is also significant.

MR53.1 <- gam(CPUE_Biomass ~ Month+PropLogL5, data = Data_merge_Model)
summary(MR53.1)
AIC(MR53.1) #676.57

#For MR53.1, January and March through December are signifcant. 
#PropLogL5 is signifcant. 

MR54 <- gam(CPUE_Count ~ Year.x+PropLogL5, data = Data_merge_Model)
summary(MR54)
AIC(MR54) #362.37

MR55 <- gam(CPUE_Count ~ Year.x+PropLogG20, data = Data_merge_Model)
summary(MR55)
AIC(MR55) #357.18

MR56 <- gam(CPUE_Count ~ Month+PropLogG20, data = Data_merge_Model)
summary(MR56)
AIC(MR56) #377.63

#For MR56, January is significant as well as PropLogG20. 

MR56.1 <- gam(CPUE_Biomass ~ Month+PropLogG20, data = Data_merge_Model)
summary(MR56.1)
AIC(MR56.1) #668.97

#For MR56.1, January is significant. 


#####################

#Questions:
#When should use Year.x and Year.y?
#Any other models I should try?

#Read in table with Lat and Long in it for sites so you can make models for it. 

SitesLoyalU <- read.csv("SitesLoyalU.csv")


#Now need to delete all columns besides lat and long so we can merge it. 

SitesLoyalU3 <- SitesLoyalU[, -c(2,3,4)]

#Now merge Data_merge_Model with SitesLoaylU3

Data_merge_Model <- merge(Data_merge_Model, SitesLoyalU3, by = c("SiteCode"), all = FALSE)

#Now do more moeling to see if site lat and long is any good. 

MR57 <- gam(Highest_Temperature_C ~ SiteLat+SiteLon, data = Data_merge_Model)
summary(MR57)
AIC(MR57) #321.92

MR58 <- gam(Highest_Temperature_C ~ SiteLat*SiteLon, data = Data_merge_Model)
summary(MR58)
AIC(MR58) #321.92

MR59 <- gam(Highest_Temperature_C~ SiteLat, data = Data_merge_Model)
summary(MR59)
AIC(MR59) #320.29

MR60 <- gam(Highest_Temperature_C~ SiteLon, data = Data_merge_Model)
summary(MR60)
AIC(MR60) #321.06

MR61 <- gam(Lowest_Temperature_C~ SiteLat+SiteLon, data = Data_merge_Model)
summary(MR61)
AIC(MR61) #321.88

MR62 <- gam(Lowest_Temperature_C~ SiteLat*SiteLon, data = Data_merge_Model)
summary(MR62)
AIC(MR62) #321.88

MR63 <- gam(Lowest_Temperature_C~ SiteLat, data = Data_merge_Model)
summary(MR63)
AIC(MR63) #320.04

MR64 <- gam(Lowest_Temperature_C~ SiteLon, data = Data_merge_Model)
summary(MR64)
AIC(MR64) #319.88

MR65 <- gam(AvgMin~ SiteLat*SiteLon, data = Data_merge_Model)
summary(MR65)
AIC(MR65) #318.13

MR66 <- gam(AvgMin~ SiteLat+SiteLon, data = Data_merge_Model)
summary(MR66)
AIC(MR66) #318.13

MR66 <- gam(AvgMin~ SiteLat, data = Data_merge_Model)
summary(MR66)
AIC(MR66) #316.38

MR67 <- gam(AvgMin~ SiteLon, data = Data_merge_Model)
summary(MR67)
AIC(MR67) #316.39

MR68 <- gam(AvgMax~ SiteLat+SiteLon, data = Data_merge_Model)
summary(MR68)
AIC(MR68) #322.00

MR69 <- gam(AvgMax~ SiteLat*SiteLon, data = Data_merge_Model)
summary(MR69)
AIC(MR69) #322.00

MR70 <- gam(AvgMax~ SiteLat, data = Data_merge_Model)
summary(MR70)
AIC(MR70) #322.29

MR71 <- gam(AvgMax~ SiteLon, data = Data_merge_Model)
summary(MR71)
AIC(MR71) #320.24

MR72 <- gam(PropLogL3~ SiteLat+SiteLon, data = Data_merge_Model)
summary(MR72)
AIC(MR72) #22.69

MR73 <- gam(PropLogL3~ SiteLat+SiteLon+Month, data = Data_merge_Model)
summary(MR73)
AIC(MR73) #-78.69

MR74 <- gam(PropLogL3~ SiteLat*SiteLon, data = Data_merge_Model)
summary(MR74)
AIC(MR74) #22.69

MR75 <- gam(PropLogL3~ SiteLat, data = Data_merge_Model)
summary(MR75)
AIC(MR75) #20.69


MR76 <- gam(PropLogL3~ SiteLon, data = Data_merge_Model)
summary(MR76)
AIC(MR76) #21.28

MR77 <- gam(PropLogL5~ SiteLat*SiteLon, data = Data_merge_Model)
summary(MR77)
AIC(MR77) #53.04

MR78 <- gam(PropLogL5~ SiteLat+SiteLon, data = Data_merge_Model)
summary(MR78)
AIC(MR78) #53.04

MR79 <- gam(PropLogL5~ SiteLat, data = Data_merge_Model)
summary(MR79)
AIC(MR79) #51.06

MR80 <- gam(PropLogL5~ SiteLon, data = Data_merge_Model)
summary(MR80)
AIC(MR80) #51.09

MR81 <- gam(PropLogG20~ SiteLat+SiteLon, data = Data_merge_Model)
summary(MR81)
AIC(MR81) #-623.42

#For MR81, Intercept and SiteLon are almost significant.

MR82 <- gam(PropLogL5~ SiteLat*SiteLon, data = Data_merge_Model)
summary(MR82)
AIC(MR82) #53.04

MR83 <- gam(PropLogL5~ SiteLat, data = Data_merge_Model)
summary(MR83)
AIC(MR83) #51.05

MR84 <- gam(PropLogL5~ SiteLon, data = Data_merge_Model)
summary(MR84)
AIC(MR84) #51.09

#Now we need to make plots 
install.packages("ggplot2")
library(ggplot2)

#The first plot I will be making is looking at the Stream temp for 2020 and 2021. We need to use the ST data set for this. 
#We also need to filter the ST data set for the five sites in the reduced data and years 2020 and 2021. 

STPlot <- filter(ST,
    (SiteCode == "Dry.Hoagland" & (Year == "2020" | Year == "2021")) |
    (SiteCode == "Grandad.Hessler" & (Year == "2020" | Year == "2021")) |
    (SiteCode == "Painter.LittleBear" & (Year == "2020" | Year == "2021")) |
    (SiteCode == "Sherman.Loyalsock" & (Year == "2020" | Year == "2021")) |
    (SiteCode == "Red.LittleBear" & (Year == "2020" | Year == "2021"))
)


ggplot(STPlot, aes(x = Year, y = Temp_C)) +
  geom_line()

ggplot(STPlot, aes(x = Year, y = Temp_C, color = SiteCode)) +
  geom_line() +
  labs(x = "Year", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

ggplot(STPlot, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Create the ggplot with custom line colors for each site

site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "firebrick", "Red.LittleBear" = "slateblue2")


ggplot(STPlot, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()



#Make plot Showing Number of days over certain temperature. 



#Make Plots of signicnt Data 

model_84 <- predict.gam(M84)
model_p

cars %>%
  mutate( my_model = predict(fit) ) %>%
  ggplot() +
  geom_point( aes(speed, dist) ) +
  geom_line( aes(speed, my_model)  )


#Test 
Data_merge_Model %>%
  mutate(MP84 = predict.gam(MR84))  %>%
  ggplot() +
  geom_point(aes(SiteLon, PropLogL5)) +
  geom_line(aes(SiteLon, MP84))
#Yay it worked :)

#MP8
#Data_merge_Model %>%
#  mutate(MP8 = predict.gam(MR8))  %>%
#  ggplot() +
#  geom_point(aes(c(Month, Lowest_Temperature_C), CPUE_Count)) +
#  geom_line(aes(c(Month, Lowest_Temperature_C), MP8))

Data_merge_Model %>%
  mutate(MP8 = predict.gam(MR8))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Count, color = Month)) +
  geom_line(aes(Lowest_Temperature_C, MP8))

#MP8 <- gam(CPUE_Count ~ Month+Lowest_Temperature_C, data = Data_merge_Model)
#summary(MR8)
#AIC(MR8) # 377.17

#MP11

Data_merge_Model %>%
  mutate(MP11 = predict.gam(MR11))  %>%
  ggplot() +
  geom_point(aes(AvgMin, CPUE_Count, color = Month)) +
  geom_line(aes(AvgMin, MP11))

Data_merge_Model %>%
  mutate(MP11 = predict.gam(MR11)) %>%
  ggplot() +
  geom_boxplot(aes(x = AvgMin, y = CPUE_Count, fill = Month)) +
  geom_point(aes(x = AvgMin, y = CPUE_Count, color = Month)) +
  labs(x = "AvgMin", y = "CPUE_Count", title = "MP11") +
  theme_minimal()

#MP23

Data_merge_Model %>%
  mutate(MP23 = predict.gam(MR23))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, RatAY, color = Month)) +
  geom_line(aes(Lowest_Temperature_C, MP23))


#MP28 (Interaction)

Data_merge_Model %>%
  mutate(MP28 = predict.gam(MR28))  %>%
  ggplot() +
  geom_point(aes(AvgMax, RatAY, color = Month)) +
  geom_line(aes(AvgMax, MP28))

ggplot(Data_merge_Model,aes(y=RatAY,x=AvgMax,color=factor(Month)))+geom_point()+stat_smooth(method="MR28",se=FALSE)

ggplot(Data_merge_Model, aes(x = AvgMax, y = RatAY, color = Month)) +
  geom_point() +  # Scatter plot of actual data
  geom_line(aes(y = MP28)) +  # Line plot of predicted values
  labs(x = "Average Maximum Temperature", y = "RatAY", color = "Month") +
  theme_minimal()

#Dont undderstand how to do the interaction :(

#MR31

Data_merge_Model %>%
  mutate(MP31 = predict.gam(MR31))  %>%
  ggplot() +
  geom_point(aes(AvgMin, RatAY, color = Month)) +
  geom_line(aes(AvgMin, MP31))

#MP35

Data_merge_Model %>%
  mutate(MP35 = predict.gam(MR35))  %>%
  ggplot() +
  geom_point(aes(PropLogL3, RatAY, color = Month)) +
  geom_line(aes(PropLogL3, MP35))

#MP44

Data_merge_Model %>%
  mutate(MP44 = predict.gam(MR44))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Biomass, color = Month)) +
  geom_line(aes(Lowest_Temperature_C, MP44))

#MP45 (Interaction)

Data_merge_Model %>%
  mutate(MP45 = predict.gam(MR45))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Biomass, color = Month)) +
  geom_line(aes(Lowest_Temperature_C, MP45))

MR45 <- gam(CPUE_Biomass ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR45)
AIC(MR45) #683.93

#Don't understand how to do interaction

#MP47

Data_merge_Model %>%
  mutate(MP47 = predict.gam(MR47))  %>%
  ggplot() +
  geom_point(aes(AvgMin, CPUE_Biomass, color = Month)) +
  geom_line(aes(AvgMin, MP47))

#MP49 

Data_merge_Model %>%
  mutate(MP49 = predict.gam(MR49))  %>%
  ggplot() +
  geom_point(aes(AvgMax, CPUE_Biomass, color = Month)) +
  geom_line(aes(AvgMax, MP49))

#MP50 (Interaction)

Data_merge_Model %>%
  mutate(MP50 = predict.gam(MR50))  %>%
  ggplot() +
  geom_point(aes(AvgMax, CPUE_Biomass, color = Month)) +
  geom_line(aes(AvgMax, MP50))

MR50 <- gam(CPUE_Biomass ~ Month*AvgMax, data = Data_merge_Model)
summary(MR50)
AIC(MR50) #687.28

#MP52

Data_merge_Model %>%
  mutate(MP52 = predict.gam(MR52))  %>%
  ggplot() +
  geom_point(aes(PropLogL3, CPUE_Biomass, color = Month)) +
  geom_line(aes(PropLogL3, MP52))

#MP53

Data_merge_Model %>%
  mutate(MP53 = predict.gam(MR53))  %>%
  ggplot() +
  geom_point(aes(PropLogL5, CPUE_Count, color = Month)) +
  geom_line(aes(PropLogL5, MP53))

#MP53.1

Data_merge_Model %>%
  mutate(MP53.1 = predict.gam(MR53.1))  %>%
  ggplot() +
  geom_point(aes(PropLogL5, CPUE_Biomass, color = Month)) +
  geom_line(aes(PropLogL5, MP53.1))

#MP56

Data_merge_Model %>%
  mutate(MP56 = predict.gam(MR56))  %>%
  ggplot() +
  geom_point(aes(PropLogG20, CPUE_Count, color = Month)) +
  geom_line(aes(PropLogG20, MP56))

#This one is really interesting I think at least.....

#MP56.1

Data_merge_Model %>%
  mutate(MP56.1 = predict.gam(MR56.1))  %>%
  ggplot() +
  geom_point(aes(PropLogG20, CPUE_Biomass, color = Month)) +
  geom_line(aes(PropLogG20, MP56.1))

#Also pretty interesting 

#MP81

Data_merge_Model %>%
  mutate(MP81 = predict.gam(MR81))  %>%
  ggplot() +
  geom_point(aes(PropLogG20, SiteLat, color = SiteLon)) +
  geom_line(aes(PropLogG20, MP81))
#This one just straight up sucks.

#Plot=

x11(height=12,width=16)#brings plot outside of RStudio, controls plot window size

op<-par(mar=c(2,4,1,1), bty = "n")

plot(Rock.LittleLoyalsock_Land_Jul20$ID,Rock.LittleLoyalsock_Land_Jul20$Temp_C,
     
     type="n",xlim=c(1114,4089),   ylim=c(8,27)   ,xlab="",ylab="",axes=F,bty="n",
     
     main="Rock.LittleLoyalsock - July 2020")

points(Rock.LittleLoyalsock_Land_Jul20$ID,Rock.LittleLoyalsock_Land_Jul20$Temp_C,
       
       type="p",pch=18,cex=1.5)#solid diamonds

points(Rock.LittleLoyalsock_Stream_Jul20$ID,Rock.LittleLoyalsock_Stream_Jul20$Temp_C,
       
       type="p",pch=21,bg="blue",cex=1.5)#blue circle

lines(Rock.LittleLoyalsock_Land_Jul20$ID,Rock.LittleLoyalsock_Land_Jul20$Temp_C,
      
      type = "l",lty=1,lwd=1)

lines(Rock.LittleLoyalsock_Stream_Jul20$ID,Rock.LittleLoyalsock_Stream_Jul20$Temp_C,
      
      type = "l",lty=3,col="blue",lwd=1)

maj1<-c(1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,
        
        2500,2600,2700,2800,2900,3000,3100,3200,3300,3400,3500,3600,3700,3800,
        
        3900,4000,4100)# IDs

lab1<-c("July1","July2","July3","July4","July5","July6","July7","","","","","",
        
        "","","","","","","","","","","","","","","","","","","")

axis(1,at=maj1,lty=1,lwd=0.5,   pos=8   ,labels=lab1,tck=10)

maj2<-c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)#temps

axis(2,at=maj2,lty=1,lwd=0.5,las=2,pos=1100,tck=1)#y-axis add tck=1 for gridlines




