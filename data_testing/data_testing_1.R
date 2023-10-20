#Set working directory 

setwd("C:/GitHub/johnson_capstone/data_testing")

getwd()
dir()

#Download Read xl package so R can read in your data
install.packages("readxl")
library(readxl)

#Read in water temp data for Longterm Loyalsock Project 

Data1.xlsx.tibble <- read_excel("Data.xlsx")
