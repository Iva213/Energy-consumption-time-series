## Time series analysis -- Dealing with the NAs 

## Set working directory 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/IoT Analytics")

#Load required packages
library(imputeTS)
library(tidyr)
library(missForest)

################################################

## Introduce NAs in the dataset 

#Adjust above with start datetime 16/12/2006;17:24:00 and end datetime 26/11/2010;21:02:00 -- THIAGO'S CODE
#energy.NA <- energy %>% 
  #complete(Year, nesting(Month, Week, Day, Hour, Minute)) %>% 
  #mutate(Date = paste(Year, Month, Day)) 

reg.timeser <- seq.POSIXt(as.POSIXct("2007-01-01 00:00:00", tz = "CET"), 
                          as.POSIXct("2009-12-31 23:59:00", tz = "CET"), by = "1 min")

reg.timeser <- data.frame(reg.timeser)

colnames(reg.timeser) <- "DateTime"

#missing_minutes<- RegularTimeSeries[!RegularTimeSeries %in% allYears$DateTime]

energy.merged <- merge(energy, reg.timeser, by = "DateTime", all = TRUE)

#Overall energy dataset
statsNA(energy.ts)
plotNA.distribution(energy.ts)

#Create TS object
energy.ts1 <- ts(energy.merged$Sub_metering_1, frequency = 525620, start = c(2007,1))
energy.ts2 <- ts(energy.merged$Sub_metering_2, frequency = 525620, start = c(2007,1))
energy.ts3 <- ts(energy.merged$Sub_metering_3, frequency = 525620, start = c(2007,1))

#Impute missing values for the 3 sub-meters 
impute.ts1 <- na_interpolation(energy.ts1, option = "spline")
impute.ts2 <- na_interpolation(energy.ts2, option = "spline")
impute.ts3 <- na_interpolation(energy.ts3, option = "spline")
  
  
  
  













