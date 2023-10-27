#Set working directory 

setwd("C:/GitHub/johnson_capstone/data_testing")

getwd()
dir()

#All 3 excell files I'm going to read in are CSV's. 

Painter_LT <- read.csv("Painter_Land_X_QC.csv")

Painter_ST00 <- read.csv("Painter_Stream_X_QC.csv")

Painter_ST01 <- read.csv("Painter_Stream0_X_QC.csv")


#The problem is the stream temperature data is split up into two data sets, and I want them as 1 data set.
#So lets merge the two. 

Painter_ST <- merge(Painter_ST00, Painter_ST01, all = TRUE)
                    
#This works, but the dtaes are not in order??????????
        
