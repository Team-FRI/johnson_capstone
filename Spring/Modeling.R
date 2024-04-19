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

STPlot1 <- filter(ST,
                  (SiteCode == "Dry.Hoagland" & (Year == "2020" | Year == "2021")) |
                   (SiteCode == "Grandad.Hessler" & (Year == "2020" | Year == "2021")) |
                   (SiteCode == "Painter.LittleBear" & (Year == "2020" | Year == "2021")) |
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

#Bellow is plot that adds line one at a time 

# Define site colors 
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "firebrick", "Red.LittleBear" = "slateblue2")
#Fore some reason this second one works after I flipped firebrick to sheman and slateblue2 to red, even though thats backwords to what should be working...idk it worked at least :).
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "darkorange2", "Red.LittleBear" = "firebrick")


# Filter for 2020 and 2021 <7 so you can get sherman to stop going into 2022 when other data doesn't
STPlot_2020_2021 <- filter(STPlot, (year(Date) == 2020) | (year(Date) == 2021 & month(Date) < 7))

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal() +
  theme(legend.key.size = unit(2, "lines"))  # Adjust the legend key size

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}



User

ggplot(LifeHistoryTable, aes(x=LH_Stage, y=AvgTemp, fill="Eggs", "Alevin")) + 
  geom_point(stat="identity", color="black", 
             position=position_dodge()) + 
  geom_errorbar(aes(ymin=AvgTemp-SD, ymax=AvgTemp+SD), width=.2, 
                position=position_dodge(.9))
ChatGPT
User
How can I make each error bar a different color based on the stages of the life hisory?
  ChatGPT
To make each error bar a different color based on the stages of the life history in a plot, you can utilize the matplotlib library in Python. Assuming you have data for different stages of life history and their respective error bars, you can specify colors for each stage and assign them accordingly to the error bars.

Here's a basic example of how you can achieve this:

python
Copy code
import matplotlib.pyplot as plt

# Example data for life history stages
stages = ['Stage 1', 'Stage 2', 'Stage 3']
means = [10, 15, 20]  # Example mean values for each stage
errors = [1, 2, 1]    # Example error values for each stage

# Assigning colors to each stage
colors = ['red', 'green', 'blue']

# Plotting the data with error bars
plt.bar(stages, means, yerr=errors, color=colors)

# Adding labels and title
plt.xlabel('Life History Stages')
plt.ylabel('Mean Value')
plt.title('Mean Values with Error Bars for Different Life History Stages')

# Display the plot
plt.show()
In this example, each stage of the life history is assigned a different color, and these colors are used when plotting the error bars. You can modify the means, errors, and colors lists according to your actual data.

User
Im using R studio
ChatGPT
In R, you can achieve this using the ggplot2 package, which provides a powerful and flexible system for creating graphics. You can use the geom_errorbar() function along with ggplot() to create a plot with error bars, and then specify the colors for each stage of the life history using the scale_color_manual() function.

Here's an example of how you can do this in R:
  
  R
Copy code
library(ggplot2)

# Example data for life history stages
stages <- c('Stage 1', 'Stage 2', 'Stage 3')
means <- c(10, 15, 20)  # Example mean values for each stage
errors <- c(1, 2, 1)    # Example error values for each stage

# Create a data frame
data <- data.frame(stages, means, errors)

# Specify colors for each stage
colors <- c('red', 'green', 'blue')

# Plot with ggplot
p <- ggplot(data, aes(x = stages, y = means, color = stages)) +
  geom_errorbar(aes(ymin = means - errors, ymax = means + errors), width = 0.2) +
  geom_point(size = 3) +
  scale_color_manual(values = colors) +  # Assign colors
  labs(x = 'Life History Stages', y = 'Mean Value', title = 'Mean Values with Error Bars for Different Life History Stages')

# Display the plot
print(p)
In this R code:
  
  We create a data frame containing the stages, means, and errors.
We specify the colors for each stage in the colors vector.
We use ggplot() to create the plot, mapping the stages to the x-axis, means to the y-axis, and colors to the color aesthetic.
We use geom_errorbar() to add error bars to the plot.
We use scale_color_manual() to assign the specified colors to each stage.
Finally, we add labels and a title to the plot using labs().
User
ggplot(LifeHistoryTable, aes(x=LH_Stage, y=AvgTemp, color=LH_Stage)) + 
  geom_point(stat="identity", color="black", 
             position=position_dodge()) + 
  geom_errorbar(aes(ymin=AvgTemp-SD, ymax=AvgTemp+SD), width=.2, 
                position=position_dodge(.9)) How do i make the legend biger
ChatGPT
To make the legend bigger in a ggplot2 plot in R, you can adjust the size of the legend key using the theme() function. Here's how you can modify your code to increase the size of the legend:

R
Copy code
library(ggplot2)

ggplot(LifeHistoryTable, aes(x = LH_Stage, y = AvgTemp, color = LH_Stage)) + 
  geom_point(stat = "identity", color = "black", position = position_dodge()) + 
  geom_errorbar(aes(ymin = AvgTemp - SD, ymax = AvgTemp + SD), width = 0.2, position = position_dodge(0.9)) +
  theme(legend.key.size = unit(2, "lines"))  # Adjust the legend key size
In the theme() function, legend.key.size is set to unit(2, "lines"), where 2 specifies the number of lines for the legend key. You can adjust this number to make the legend key bigger or smaller as desired.

User
What if I wan't to make the color of lines biger within the legend 
ChatGPT
To make the color of lines bigger within the legend in a ggplot2 plot in R, you can adjust the size of the legend key and the size of the lines within it separately. Here's how you can modify your code to achieve this:

R
Copy code
library(ggplot2)

ggplot(LifeHistoryTable, aes(x = LH_Stage, y = AvgTemp, color = LH_Stage)) + 
  geom_point(stat = "identity", color = "black", position = position_dodge()) + 
  geom_errorbar(aes(ymin = AvgTemp - SD, ymax = AvgTemp + SD), width = 0.2, position = position_dodge(0.9)) +
  theme(legend.key.size = unit(2, "lines"),     # Adjust the legend key size
        legend.key.height = unit(2, "lines"),   # Adjust the height of the legend key
        legend.key.width = unit(5, "lines"))   # Adjust the width of the legend key
In the theme() function, legend.key.height and legend.key.width specify the height and width of the legend key, respectively. Adjust these values as needed to make the color lines within the legend bigger or smaller.

User
What it I want the size of the point and error bar to be biger?
ChatGPT
To adjust the size of the points and error bars in your ggplot, you can modify their size using the size parameter within the geom_point() and geom_errorbar() functions. Here's how you can do it:
  
  R
Copy code
library(ggplot2)

ggplot(LifeHistoryTable, aes(x = LH_Stage, y = AvgTemp, color = LH_Stage)) + 
  geom_point(stat = "identity", color = "black", size = 3, position = position_dodge()) +  # Adjust point size (e.g., size = 3)
  geom_errorbar(aes(ymin = AvgTemp - SD, ymax = AvgTemp + SD), width = 0.2, size = 1.5, position = position_dodge(0.9)) +  # Adjust error bar size (e.g., size = 1.5)
  theme(legend.key.size = unit(2, "lines"),     # Adjust the legend key size
        legend.key.height = unit(2, "lines"),   # Adjust the height of the legend key
        legend.key.width = unit(5, "lines"))   # Adjust the width of the legend key
In this code:
  
  size = 3 within geom_point() adjusts the size of the points.
size = 1.5 within geom_errorbar() adjusts the size of the error bars.
Adjust these values as needed to make the points and error bars bigger or smaller.
User
#Point line plot with biger legend 
ggplot(LifeHistoryTable, aes(x = LH_Stage, y = AvgTemp, color = LH_Stage)) + 
  geom_point(stat = "identity", color = "black", position = position_dodge()) + 
  geom_errorbar(aes(ymin = AvgTemp - SD, ymax = AvgTemp + SD), width = 0.2, position = position_dodge(0.9)) +
  theme(legend.key.size = unit(2, "lines"))  # Adjust the legend key size

What if I want to use this code. how would I make the size of the pont and bars biger?
  ChatGPT
To make the size of the points and error bars bigger in your existing code while keeping the legend key bigger, you can simply add the size parameter to both geom_point() and geom_errorbar(). Here's how you can modify your code:

R
Copy code
ggplot(LifeHistoryTable, aes(x = LH_Stage, y = AvgTemp, color = LH_Stage)) + 
  geom_point(stat = "identity", color = "black", size = 3) +  # Adjust point size (e.g., size = 3)
  geom_errorbar(aes(ymin = AvgTemp - SD, ymax = AvgTemp + SD), width = 0.2, size = 1.5) +  # Adjust error bar size (e.g., size = 1.5)
  theme(legend.key.size = unit(2, "lines"))  # Adjust the legend key size
In this modified code:

size = 3 within geom_point() adjusts the size of the points.
size = 1.5 within geom_errorbar() adjusts the size of the error bars.
You can adjust these values as needed to make the points and error bars bigger or smaller.
User
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "firebrick", "Red.LittleBear" = "slateblue2")


ggplot(STPlot, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal() What if I want to indrocude one line one color at a time
ChatGPT
If you want to introduce one line with its color at a time in the plot, you can create separate plots for each line and then combine them using a loop. Here's how you can modify your code to achieve this:
  
  R
Copy code
# Create an empty list to store individual plots
plot_list <- list()

# Loop through each site and create a plot with only one line at a time
for (site in unique(STPlot$SiteCode)) {
  # Subset the data for the current site
  site_data <- subset(STPlot, SiteCode == site)
  
  # Create a plot for the current site
  plot <- ggplot(site_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    geom_line() +
    scale_color_manual(values = site_colors[site]) +  # Apply color for the current site only
    labs(x = "Date", y = "Temperature (°C)", color = "Site") +
    theme_minimal()
  
  # Add the plot to the list
  plot_list[[site]] <- plot
}

# Combine all individual plots into one plot using patchwork
combined_plot <- patchwork::wrap_plots(plotlist = plot_list)

# Display the combined plot
print(combined_plot)
In this code:
  
  We loop through each unique site code in the data.
For each site, we subset the data to contain only observations for that site.
We create a plot for the current site using ggplot() and customize it accordingly.
Each plot is stored in the plot_list.
Finally, we combine all individual plots into one plot using the patchwork::wrap_plots() function from the patchwork package and display it.
User
What if I want to do each line one color at a time
ChatGPT
If you want to gradually introduce each line with its color one at a time in the plot, you can iteratively add lines to the plot within a loop. Here's how you can modify your code to achieve this:

R
Copy code
# Create an empty plot
p <- ggplot() +
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines with their colors
for (site in unique(STPlot$SiteCode)) {
  # Subset the data for the current site
  site_data <- subset(STPlot, SiteCode == site)
  
  # Add a line for the current site with its color
  p <- p +
    geom_line(data = site_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = site_colors[1:length(unique(site_data$SiteCode))])  # Use color for the current site only
  
  # Display the plot
  print(p)
}
In this code:

We start with an empty plot p.
We loop through each unique site code in the data.
For each site, we subset the data to contain only observations for that site.
We add a line for the current site with its color to the plot p.
Inside the loop, the print(p) function is called to display the plot after each line is added. This will gradually show the plot with each line and its corresponding color.
User
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "firebrick", "Red.LittleBear" = "slateblue2")


ggplot(STPlot, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()
What if I want to plot one line at a time and make multiple plots with one new line each time
ChatGPT
If you want to plot one line at a time and create multiple plots with one new line added each time, you can loop through each site code, subset the data for that site, and plot the data up to that point. Here's how you can modify your code to achieve this:
  
  R
Copy code
# Create an empty plot
p <- ggplot() +
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot, SiteCode %in% unique(STPlot$SiteCode)[1:i])
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = site_colors[1:i])  # Use colors up to the current site
  
  # Display the plot
  print(p)
}
In this code:
  
  We start with an empty plot p.
We loop through each site code up to the current iteration.
For each iteration, we subset the data to include observations for all sites up to the current site.
We add a line for the current subset of data to the plot p, using colors up to the current site.
Inside the loop, the print(p) function is called to display the plot after each line is added. This will create multiple plots with one new line added each time.
User
Why is their an extra grey line in adittion to the two I want on their? site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue")

ggplot(STPlot, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()
ChatGPT
The extra grey line is likely caused by the default color provided by ggplot for the first line. To avoid this, you can explicitly set the colors for all lines using scale_color_manual() and provide NA values for the colors that should not be plotted in the initial iteration. Here's how you can modify your code to address this issue:

R
Copy code
# Define site colors
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue")

# Create an empty plot
p <- ggplot() +
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot, SiteCode %in% unique(STPlot$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}
In this code:

I set the color values for all lines using scale_color_manual() within the loop, making sure to set na.value = "transparent" to avoid plotting the lines for which there are NA color values.
By initializing color_values with NA values and updating it only up to the current iteration, we ensure that the lines beyond the current iteration are not plotted.
User
What if I want to only plot months in 2020 # Create an empty plot

p <- ggplot(STPlot, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot, SiteCode %in% unique(STPlot$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}
ChatGPT
To only plot data for the months in 2020, you can filter the data before creating the plot. Here's how you can modify your code to achieve this:
  
  R
Copy code
library(dplyr)  # Load the dplyr package for data manipulation

# Filter the data to include only months in 2020
STPlot_2020 <- filter(STPlot, year(Date) == 2020)

# Create an empty plot
p <- ggplot(STPlot_2020, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020, SiteCode %in% unique(STPlot_2020$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}
In this code:
  
  We use the filter() function from the dplyr package to filter the STPlot data frame to include only months in the year 2020.
We then proceed with plotting the filtered data in the same way as before. This ensures that only data for the months in 2020 is included in the plot.
User
What if i want 2020 and 2021
ChatGPT
If you want to include data for both 2020 and 2021, you can extend the filtering condition to include both years. Here's how you can modify the code to achieve this:

R
Copy code
library(dplyr)  # Load the dplyr package for data manipulation

# Filter the data to include only months in 2020 and 2021
STPlot_2020_2021 <- filter(STPlot, year(Date) %in% c(2020, 2021))

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}
This code will filter the data to include only months from both 2020 and 2021 and then proceed with plotting as before.

User
What if I want to include the years 2020 and 2021, but not plot from August on\
ChatGPT
To include data from the years 2020 and 2021 but exclude data from August onwards, you can extend the filtering condition to include both years and then specify that the month should be less than August. Here's how you can modify the code:
  
  R
Copy code
library(dplyr)  # Load the dplyr package for data manipulation

# Filter the data to include only months in 2020 and 2021, but not from August onwards
STPlot_2020_2021 <- filter(STPlot, year(Date) %in% c(2020, 2021) & month(Date) < 8)

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}
This code filters the data to include only months from the years 2020 and 2021 but not from August onwards. Then it proceeds with plotting as before.

User
I only want <8 for 20201
ChatGPT
Apologies for the oversight. To filter data for the year 2020 entirely and only include data from January to July for the year 2021, you can adjust the filtering condition accordingly. Here's how you can modify the code:

R
Copy code
library(dplyr)  # Load the dplyr package for data manipulation

# Filter the data to include only months in 2020 and months from January to July in 2021
STPlot_2020_2021 <- filter(STPlot, (year(Date) == 2020) | (year(Date) == 2021 & month(Date) < 8))

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}
This code filters the data to include only months from January to July in the year 2021 and all months from the year 2020. Then, it proceeds with plotting as before.

User
#Define site colors 
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "firebrick", "Red.LittleBear" = "slateblue2")

#Filter for 2020 and 2021 <7 so you can get sherman to stop going into 2022 when other data doesent 

STPlot_2020_2021 <- filter(STPlot, (year(Date) == 2020) | (year(Date) == 2021 & month(Date) < 7))

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}Why is Sherman purple when i want it to be firebrick?
ChatGPT
It seems like the issue might be with the way the color values are assigned to the site_colors vector. In your code, you've defined the color for "Sherman.Loyalsock" as "firebrick" in the site_colors vector. However, when using it in the plot, it appears as purple instead.

This discrepancy could occur if the data associated with "Sherman.Loyalsock" is not present in the filtered dataset STPlot_2020_2021, or if there's an issue with the mapping between the SiteCode values and the colors.

To troubleshoot this, you can check the following:

Ensure that the data associated with "Sherman.Loyalsock" is present in the filtered dataset STPlot_2020_2021. You can do this by printing the unique SiteCode values in STPlot_2020_2021 and verifying that "Sherman.Loyalsock" is included.

Verify that the SiteCode values in STPlot_2020_2021 match exactly with the keys in the site_colors vector. Even a slight difference in spelling or case sensitivity can cause the color mapping to fail.

Here's how you can check the unique SiteCode values in STPlot_2020_2021:
  
  R
Copy code
unique(STPlot_2020_2021$SiteCode)
If "Sherman.Loyalsock" is not included in the output, it means that there might be a discrepancy in the SiteCode values between the data and the site_colors vector. You would need to adjust either the SiteCode values in the data or the keys in the site_colors vector to match exactly.

Once you ensure that the SiteCode values and the site_colors vector are correctly aligned, "Sherman.Loyalsock" should be plotted with the specified "firebrick" color.

User
#Define site colors 
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "firebrick", "Red.LittleBear" = "slateblue2")

#Filter for 2020 and 2021 <7 so you can get sherman to stop going into 2022 when other data doesent 

STPlot_2020_2021 <- filter(STPlot, (year(Date) == 2020) | (year(Date) == 2021 & month(Date) < 7))

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal()

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
} How to make legend biger
ChatGPT
To make the legend bigger in a ggplot2 plot in R, you can adjust the size of the legend key using the theme() function. Here's how you can modify your code to make the legend bigger:

R
Copy code
# Define site colors 
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "firebrick", "Red.LittleBear" = "slateblue2")

# Filter for 2020 and 2021 <7 so you can get sherman to stop going into 2022 when other data doesn't
STPlot_2020_2021 <- filter(STPlot, (year(Date) == 2020) | (year(Date) == 2021 & month(Date) < 7))

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal() +
  theme(legend.key.size = unit(2, "lines"))  # Adjust the legend key size

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}
In this code:
  
  I added theme(legend.key.size = unit(2, "lines")) to the plot p. This line adjusts the size of the legend key to be bigger. You can adjust the value 2 as needed to make the legend key larger or smaller.
User
> Data_merge_Model %>%
  +   mutate(MP23 = predict.gam(MR23))  %>%
  +   ggplot() +
  +   geom_point(aes(Lowest_Temperature_C, RatAY, color = Month)) +
  +   geom_smooth(aes(Lowest_Temperature_C, MP23)) +
  +   scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                    +                                 "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                    +                                 "09" = 15, "10" = 15, "11" = 15, "12" = 17))
`geom_smooth()` using method = 'loess' and formula = 'y ~ x'
Warning message:
  No shared levels found between `names(values)` of the manual scale and the data's shape values.
ChatGPT
The warning message you received indicates that there is a mismatch between the levels specified in scale_shape_manual() and the shape values present in your data. This mismatch can occur if the levels specified in scale_shape_manual() do not match the values present in your data.

To resolve this issue, you need to ensure that the levels specified in scale_shape_manual() match the values present in your data's Month variable. Here's how you can do it:

R
Copy code
Data_merge_Model %>%
  mutate(MP23 = predict.gam(MR23)) %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, RatAY, color = Month, shape = Month)) +
  geom_smooth(aes(Lowest_Temperature_C, MP23)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                 "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                 "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()
In this code:

I added shape = Month inside aes() in geom_point() to map the Month variable to the shape aesthetic.
I specified values in scale_shape_manual() corresponding to the unique levels of the Month variable in your data. Make sure these values match the levels present in your data's Month variable exactly. Adjust them as needed based on your data.
By including shape = Month inside aes() in geom_point(), the Month variable is mapped to both color and shape aesthetics, ensuring consistency between the levels specified in scale_shape_manual() and the data's shape values.
User
> Data_merge_Model %>%
+   mutate(MP11 = predict.gam(MR11))  %>%
+   ggplot() +
+   geom_point(aes(AvgMin, CPUE_Count, color = Month)) +
+   geom_smooth(aes(AvgMin, MP11)) +
+   scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
+                                 "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
+                                 "09" = 15, "10" = 15, "11" = 15, "12" = 17))
`geom_smooth()` using method = 'loess' and formula = 'y ~ x'
Warning message:
No shared levels found between `names(values)` of the manual scale and the data's shape values. 
ChatGPT
The warning message you received indicates that there is a mismatch between the levels specified in scale_shape_manual() and the shape values present in your data. This mismatch can occur if the levels specified in scale_shape_manual() do not match the values present in your data's Month variable.

To resolve this issue, you need to ensure that the levels specified in scale_shape_manual() match the values present in your data's Month variable. Here's how you can do it:

R
Copy code
Data_merge_Model %>%
  mutate(MP11 = predict.gam(MR11)) %>%
  ggplot() +
  geom_point(aes(AvgMin, CPUE_Count, color = Month, shape = Month)) +
  geom_smooth(aes(AvgMin, MP11)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                 "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                 "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()
In this code:

I added shape = Month inside aes() in geom_point() to map the Month variable to the shape aesthetic.
I specified values in scale_shape_manual() corresponding to the unique levels of the Month variable in your data. Make sure these values match the levels present in your data's Month variable exactly. Adjust them as needed based on your data.
By including shape = Month inside aes() in geom_point(), the Month variable is mapped to both color and shape aesthetics, ensuring consistency between the levels specified in scale_shape_manual() and the data's shape values.
User
> Data_merge_Model %>%
+   mutate(MP28 = predict.gam(MR28))  %>%
+   ggplot() +
+   geom_point(aes(AvgMax, RatAY, color = Month)) +
+   geom_smooth(aes(AvgMax, MP28)) +
+   scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
+                                 "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
+                                 "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
+   theme_minimal()
`geom_smooth()` using method = 'loess' and formula = 'y ~ x'
Warning message:
No shared levels found between `names(values)` of the manual scale and the data's shape values. 
ChatGPT
The warning message you received indicates that there is a mismatch between the levels specified in scale_shape_manual() and the shape values present in your data. This mismatch can occur if the levels specified in scale_shape_manual() do not match the values present in your data's Month variable.

To resolve this issue, you need to ensure that the levels specified in scale_shape_manual() match the values present in your data's Month variable. Here's how you can do it:

R
Copy code
Data_merge_Model %>%
  mutate(MP28 = predict.gam(MR28)) %>%
  ggplot() +
  geom_point(aes(AvgMax, RatAY, color = Month, shape = Month)) +
  geom_smooth(aes(AvgMax, MP28)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                 "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                 "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()
In this code:

I added shape = Month inside aes() in geom_point() to map the Month variable to the shape aesthetic.
I specified values in scale_shape_manual() corresponding to the unique levels of the Month variable in your data. Make sure these values match the levels present in your data's Month variable exactly. Adjust them as needed based on your data.
By including shape = Month inside aes() in geom_point(), the Month variable is mapped to both color and shape aesthetics, ensuring consistency between the levels specified in scale_shape_manual() and the data's shape values.
User
site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "darkorange2", "Red.LittleBear" = "firebrick")


# Filter for 2020 and 2021 <7 so you can get sherman to stop going into 2022 when other data doesn't
STPlot_2020_2021 <- filter(STPlot, (year(Date) == 2020) | (year(Date) == 2021 & month(Date) < 7))

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line() +
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal() +
  theme(legend.key.size = unit(2, "lines"))  # Adjust the legend key size

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode)) +
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}

site_colors <- c("Dry.Hoagland" = "gray1", "Grandad.Hessler" = "blue", "Painter.LittleBear" = "forestgreen", "Sherman.Loyalsock" = "darkorange2", "Red.LittleBear" = "firebrick")

# Filter for 2020 and 2021 <7 so you can get sherman to stop going into 2022 when other data doesn't
STPlot_2020_2021 <- filter(STPlot, (year(Date) == 2020) | (year(Date) == 2021 & month(Date) < 7))

# Create an empty plot
p <- ggplot(STPlot_2020_2021, aes(x = Date, y = Temp_C, color = SiteCode)) +
  geom_line(size = 0.9) +  # Adjust line size here
  scale_color_manual(values = site_colors) +  # Apply custom color palette
  labs(x = "Date", y = "Temperature (°C)", color = "Site") +
  theme_minimal() +
  theme(legend.key.size = unit(2, "lines"), 
        axis.text = element_text(size = 13),  # Adjust the size of axis text here
        axis.title = element_text(size = 13))  # Adjust the size of axis labels here

# Loop through each site and gradually add lines one at a time
for (i in 1:length(unique(STPlot_2020_2021$SiteCode))) {
  # Subset the data up to the current site
  sub_data <- subset(STPlot_2020_2021, SiteCode %in% unique(STPlot_2020_2021$SiteCode)[1:i])
  
  # Create a color vector
  color_values <- rep(NA, length(unique(STPlot_2020_2021$SiteCode)))
  color_values[1:i] <- site_colors[1:i]
  
  # Add a line for the current site
  p <- p +
    geom_line(data = sub_data, aes(x = Date, y = Temp_C, color = SiteCode), size = 0.9) +  # Adjust line size here
    scale_color_manual(values = color_values, na.value = "transparent")  # Use colors up to the current site
  
  # Display the plot
  print(p)
}

#Making legend size bigger. 



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

Data_merge_Model %>%
  mutate(MP8 = predict.gam(MR8))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Count, color = Month)) +
  geom_smooth(aes(Lowest_Temperature_C, MP8))

Data_merge_Model %>%
  mutate(MP8 = predict.gam(MR8)) %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Count, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(Lowest_Temperature_C, MP8)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17))

?points

#From here on out:
#Winter Months: December January, February. Triangles. 
#Spring and Fall Months: March, April, May, September, October, November. Squares.
#Summer Months: June, July, August. Circles. 


#MP8 <- gam(CPUE_Count ~ Month+Lowest_Temperature_C, data = Data_merge_Model)
#summary(MR8)
#AIC(MR8) # 377.17

#MP11
?shape
Data_merge_Model %>%
  mutate(MP11 = predict.gam(MR11))  %>%
  ggplot() +
  geom_point(aes(AvgMin, CPUE_Count, color = Month)) +
  geom_line(aes(AvgMin, MP11))

Data_merge_Model %>%
  mutate(MP11 = predict.gam(MR11)) %>%
  ggplot() +
  geom_point(aes(AvgMin, CPUE_Count, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(AvgMin, MP11)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

Data_merge_Model %>%
  mutate(MP11 = predict.gam(MR11)) %>%
  ggplot() +
  geom_boxplot(aes(x = AvgMin, y = CPUE_Count, fill = Month)) +
  geom_point(aes(x = AvgMin, y = CPUE_Count, color = Month)) +
  labs(x = "AvgMin", y = "CPUE_Count", title = "MP11") +
  theme_minimal()

#MP23 *

Data_merge_Model %>%
  mutate(MP23 = predict.gam(MR23))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, RatAY, color = Month)) +
  geom_line(aes(Lowest_Temperature_C, MP23))

Data_merge_Model %>%
  mutate(MP23 = predict.gam(MR23))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, RatAY, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(Lowest_Temperature_C, MP23)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

Data_merge_Model %>%
  mutate(MP23 = predict.gam(MR23))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, RatAY, color = SiteCode)) +
  geom_line(aes(Lowest_Temperature_C, MP23))


#MP28 (Interaction) *

Data_merge_Model %>%
  mutate(MP28 = predict.gam(MR28))  %>%
  ggplot() +
  geom_point(aes(AvgMax, RatAY, color = Month)) +
  geom_line(aes(AvgMax, MP28))

Data_merge_Model %>%
  mutate(MP28 = predict.gam(MR28)) %>%
  ggplot() +
  geom_point(aes(AvgMax, RatAY, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(AvgMax, MP28)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

ggplot(Data_merge_Model,aes(y=RatAY,x=AvgMax,color=factor(Month)))+geom_point()+stat_smooth(method="MR28",se=FALSE)

ggplot(Data_merge_Model, aes(x = AvgMax, y = RatAY, color = Month)) +
  geom_point() +  # Scatter plot of actual data
  geom_line(aes(y = MP28)) +  # Line plot of predicted values
  labs(x = "Average Maximum Temperature", y = "RatAY", color = "Month") +
  theme_minimal()

#Dont undderstand how to do the interaction :(

#MR31 Maybe more intersting- SA

Data_merge_Model %>%
  mutate(MP31 = predict.gam(MR31))  %>%
  ggplot() +
  geom_point(aes(AvgMin, RatAY, color = Month)) +
  geom_line(aes(AvgMin, MP31))

Data_merge_Model %>%
  mutate(MP31 = predict.gam(MR31))  %>%
  ggplot() +
  geom_point(aes(AvgMin, RatAY, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(AvgMin, MP31))+
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

#MP35

Data_merge_Model %>%
  mutate(MP35 = predict.gam(MR35))  %>%
  ggplot() +
  geom_point(aes(PropLogL3, RatAY, color = Month)) +
  geom_line(aes(PropLogL3, MP35))

Data_merge_Model %>%
  mutate(MP35 = predict.gam(MR35))  %>%
  ggplot() +
  geom_point(aes(PropLogL3, RatAY, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(PropLogL3, MP35))+
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

#MP44 **

Data_merge_Model %>%
  mutate(MP44 = predict.gam(MR44))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Biomass, color = Month)) +
  geom_line(aes(Lowest_Temperature_C, MP44))

Data_merge_Model %>%
  mutate(MP44 = predict.gam(MR44))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Biomass, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(Lowest_Temperature_C, MP44)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

#Has a lot more dots that you can see.

#MP45 (Interaction)

Data_merge_Model %>%
  mutate(MP45 = predict.gam(MR45))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Biomass, color = Month)) +
  geom_line(aes(Lowest_Temperature_C, MP45))

Data_merge_Model %>%
  mutate(MP45 = predict.gam(MR45))  %>%
  ggplot() +
  geom_point(aes(Lowest_Temperature_C, CPUE_Biomass, color = SiteCode, shape = Month), size = 2) +
  geom_smooth(aes(Lowest_Temperature_C, MP45)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

MR45 <- gam(CPUE_Biomass ~ Month*Lowest_Temperature_C, data = Data_merge_Model)
summary(MR45)
AIC(MR45) #683.93

#Don't understand how to do interaction

#MP47 **

Data_merge_Model %>% 
  mutate(MP47 = predict.gam(MR47))  %>%
  ggplot() +
  geom_point(aes(AvgMin, CPUE_Biomass, color = Month)) +
  geom_line(aes(AvgMin, MP47))

Data_merge_Model %>% 
  mutate(MP47 = predict.gam(MR47))  %>%
  ggplot() +
  geom_point(aes(AvgMin, CPUE_Biomass, color = Month)) +
  geom_smooth(aes(AvgMin, MP47))

Data_merge_Model %>% 
  mutate(MP47 = predict.gam(MR47))  %>%
  ggplot() +
  geom_point(aes(AvgMin, CPUE_Biomass, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(AvgMin, MP47)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()


#MP49 

Data_merge_Model %>%
  mutate(MP49 = predict.gam(MR49))  %>%
  ggplot() +
  geom_point(aes(AvgMax, CPUE_Biomass, color = Month)) +
  geom_line(aes(AvgMax, MP49))

Data_merge_Model %>%
  mutate(MP49 = predict.gam(MR49))  %>%
  ggplot() +
  geom_point(aes(AvgMax, CPUE_Biomass, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(AvgMax, MP49)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

MP49 <- predict.gam(MR49) 
MP49 <- fortify(MP49)
MP49 %>%
  ggplot(aes(AvgMax, fit)) + geom_smooth(Month)


#MP50 (Interaction) * Most interesting one for biomass so far.

Data_merge_Model %>%
  mutate(MP50 = predict.gam(MR50))  %>%
  ggplot() +
  geom_point(aes(AvgMax, CPUE_Biomass, color = Month)) +
  geom_line(aes(AvgMax, MP50))

Data_merge_Model %>%
  mutate(MP50 = predict.gam(MR50))  %>%
  ggplot() +
  geom_point(aes(AvgMax, CPUE_Biomass, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(AvgMax, MP50)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()

MR50 <- gam(CPUE_Biomass ~ Month*AvgMax, data = Data_merge_Model)
summary(MR50)
AIC(MR50) #687.28

#MP52

Data_merge_Model %>%
  mutate(MP52 = predict.gam(MR52))  %>%
  ggplot() +
  geom_point(aes(PropLogL3, CPUE_Biomass, color = Month)) +
  geom_line(aes(PropLogL3, MP52))

Data_merge_Model %>%
  mutate(MP52 = predict.gam(MR52))  %>%
  ggplot() +
  geom_point(aes(PropLogL3, CPUE_Biomass, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(PropLogL3, MP52)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()


#MP53

Data_merge_Model %>%
  mutate(MP53 = predict.gam(MR53))  %>%
  ggplot() +
  geom_point(aes(PropLogL5, CPUE_Count, color = Month)) +
  geom_line(aes(PropLogL5, MP53))

Data_merge_Model %>%
  mutate(MP53 = predict.gam(MR53))  %>%
  ggplot() +
  geom_point(aes(PropLogL5, CPUE_Count, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(PropLogL5, MP53)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()



Data_merge_Model %>%
  mutate(MP53 = predict.gam(MR53))  %>%
  ggplot() +
  geom_point(aes(PropLogL5, CPUE_Count, color = SiteCode)) +
  geom_line(aes(PropLogL5, MP53))

#MP53.1

Data_merge_Model %>%
  mutate(MP53.1 = predict.gam(MR53.1))  %>%
  ggplot() +
  geom_point(aes(PropLogL5, CPUE_Biomass, color = Month)) +
  geom_line(aes(PropLogL5, MP53.1))

Data_merge_Model %>%
  mutate(MP53.1 = predict.gam(MR53.1))  %>%
  ggplot() +
  geom_point(aes(PropLogL5, CPUE_Biomass, color = Month, shape = Month), size = 2) +
  geom_smooth(aes(PropLogL5, MP53.1)) +
  scale_shape_manual(values = c("01" = 17, "02" = 17, "03" = 15, "04" = 15, 
                                "05" = 15, "06" = 16, "07" = 16, "08" = 16, 
                                "09" = 15, "10" = 15, "11" = 15, "12" = 17)) +
  theme_minimal()


#MP56

Data_merge_Model %>%
  mutate(MP56 = predict.gam(MR56))  %>%
  ggplot() +
  geom_point(aes(PropLogG20, CPUE_Count, color = Month)) +
  geom_line(aes(PropLogG20, MP56))

#Smooth does not work for this one 

#Data_merge_Model %>%
#  mutate(MP56 = predict.gam(MR56))  %>%
#  ggplot() +
#  geom_point(aes(PropLogG20, CPUE_Count, color = Month)) +
#  geom_smooth(aes(PropLogG20, MP56))

#MP56.1

Data_merge_Model %>%
  mutate(MP56.1 = predict.gam(MR56.1))  %>%
  ggplot() +
  geom_point(aes(PropLogG20, CPUE_Biomass, color = Month)) +
  geom_line(aes(PropLogG20, MP56.1))

Data_merge_Model %>%
  mutate(MP56.1 = predict.gam(MR56.1))  %>%
  ggplot() +
  geom_point(aes(PropLogG20, CPUE_Biomass, color = Month)) +
  geom_smooth(aes(PropLogG20, MP56.1))


#MP81

Data_merge_Model %>%
  mutate(MP81 = predict.gam(MR81))  %>%
  ggplot() +
  geom_point(aes(PropLogG20, SiteLat, color = SiteLon)) +
  geom_line(aes(PropLogG20, MP81))

Data_merge_Model %>%
  mutate(MP56.1 = predict.gam(MR56.1))  %>%
  ggplot() +
  geom_point(aes(PropLogG20, CPUE_Biomass, color = Month)) +
  geom_smooth(aes(PropLogG20, MP56.1))
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

#Making and Extract data frames/tables for thesis 

#Make Tables for life History

Eggs <- STPlot1 %>%
  filter(Month == "01"| Month == "02" | Month == "03" | Month == "11" | Month == "12") %>%
  summarise(
  MinTemp = min(Temp_C),
  MaxTemp = max(Temp_C),
  AvgTemp = mean(Temp_C),
  SD = sd(Temp_C)
)

#write.csv(Eggs, "Eggs.csv")

Alevin <- STPlot1 %>%
  filter(Month == "02" | Month == "03" | Month == "04") %>%
  summarise(
    MinTemp = min(Temp_C),
    MaxTemp = max(Temp_C),
    AvgTemp = mean(Temp_C),
    SD = sd(Temp_C)
  )

#write.csv(Alevin, "Alevin.csv")

YOY2020 <- STPlot1 %>%
  filter(Month == "06" & Year == "2020"| Month == "07" | Month == "08" | Month == "09" | Month == "10") %>%
  summarise(
    MinTemp = min(Temp_C),
    MaxTemp = max(Temp_C),
    AvgTemp = mean(Temp_C),
    SD = sd(Temp_C)
  )

#write.csv(YOY2020, "YOY2020.csv")

YOY2021 <- STPlot1 %>%
  filter(Month == "04" | Month == "05" | Month == "06" & Year == "2021") %>%
  summarise(
    MinTemp = min(Temp_C),
    MaxTemp = max(Temp_C),
    AvgTemp = mean(Temp_C),
    SD = sd(Temp_C)
  )

#write.csv(YOY2021, "YOY2021.csv")

Spawning <- STPlot1 %>%
  filter(Month == "09"| Month == "10" | Month == "11" ) %>%
  summarise(
    MinTemp = min(Temp_C),
    MaxTemp = max(Temp_C),
    AvgTemp = mean(Temp_C),
    SD = sd(Temp_C)
  )

#write.csv(Spawning, "Spawning.csv")

Adult_Survival <- STPlot1 %>%
  filter(Month == "06" & Year == "2020"| Month == "07" | Month == "08" ) %>%
  summarise(
    MinTemp = min(Temp_C),
    MaxTemp = max(Temp_C),
    AvgTemp = mean(Temp_C),
    SD = sd(Temp_C)
  )
#write.csv(Adult_Survival, "Adult_Survival.csv")

#Read in Life History Table

install.packages("readxl")
library(readxl)

LifeHistoryTable <- read_xlsx("LifeHistoryTable.xlsx")

#Stuff that works 

LifeHistoryTable$LH_Stage <- factor(LifeHistoryTable$LH_Stage, levels = unique(LifeHistoryTable$LH_Stage))

#Bar plot 
ggplot(LifeHistoryTable, aes(x=LH_Stage, y=AvgTemp, color=LH_Stage)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) + 
  geom_errorbar(aes(ymin=AvgTemp-SD, ymax=AvgTemp+SD), width=.2, 
                position=position_dodge(.9))

#Point line plot
ggplot(LifeHistoryTable, aes(x=LH_Stage, y=AvgTemp, color=LH_Stage)) + 
  geom_point(stat="identity", color="black", 
           position=position_dodge()) + 
  geom_errorbar(aes(ymin=AvgTemp-SD, ymax=AvgTemp+SD), width=.2, 
                position=position_dodge(.9))

#Point line plot with biger legend 
ggplot(LifeHistoryTable, aes(x = LH_Stage, y = AvgTemp, color = LH_Stage)) + 
  geom_point(stat = "identity", color = "black", position = position_dodge()) + 
  geom_errorbar(aes(ymin = AvgTemp - SD, ymax = AvgTemp + SD), width = 0.2, position = position_dodge(0.9)) +
  theme(legend.key.size = unit(2, "lines"))  # Adjust the legend key size

ggplot(LifeHistoryTable, aes(x = LH_Stage, y = AvgTemp, color = LH_Stage)) + 
  geom_point(stat = "identity", color = "black", size = 3) +  # Adjust point size (e.g., size = 3)
  geom_errorbar(aes(ymin = AvgTemp - SD, ymax = AvgTemp + SD), width = 0.2, size = 1.0) +  # Adjust error bar size (e.g., size = 1.5)
  theme(legend.key.size = unit(2, "lines"))  # Adjust the legend key size

#Trying to get things to work bellow, but its not. 

LifeHistoryTable$LH_Stage <- factor(LifeHistoryTable$LH_Stage, levels = unique(LifeHistoryTable$LH_Stage))

LifeHistoryTable$Month <- NA  # Initialize the "Month" column

# Convert the DateColumn to Date object
LifeHistoryTable$Month <- as.Date(LifeHistoryTable$Month)

LifeHistoryTable$Month[2] <- c("01", "02")

LifeHistoryTable$Month[2] <- list(c("01", "02"))

LifeHistoryTable$Month[c(1, 3, 5)] <- list(c("January", "February"))

# Extract the month from the DateColumn
LifeHistoryTable$Month <- month(LifeHistoryTable$Month)

LifeHistoryTable <- read_excel("LifeHistoryTable.xlsx", col_types$Month = "numeric")

LifeHistoryTable <- read_excel("LifeHistoryTable.xlsx", col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric"))

new_column <- c("A", "B", "C", "D", "E")  # Example new column data



LHP<- ggplot(df2, aes(x=dose, y=len, color=Month)) + geom_bar(stat="identity", color="black", 
                                                              position=position_dodge()) + geom_errorbar(aes(ymin=AvgTemp-sd, ymax=AvgTemp+sd), width=.2, 
                                                                                                         position=position_dodge(.9))
LifeHistoryTable$Month <- as.numeric(LifeHistoryTable$Month)

ggplot(LifeHistoryTable, aes(x=LH_Stage, y=AvgTemp, color="black")) + geom_bar(stat="identity", color="black", 
                                                              position=position_dodge()) + geom_errorbar(aes(ymin=AvgTemp-SD, ymax=AvgTemp+SD), width=.2, 
                                                                                                         position=position_dodge(.9))

ggplot(LifeHistoryTable, aes(x = LH_Stage, y = AvgTemp)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = AvgTemp - sd, ymax = AvgTemp + sd), width = 0.2) +
  labs(x = "LH Stage", y = "Avg Temperature")
