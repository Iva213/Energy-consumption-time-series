## Time series analysis -- Dealing with the NAs 

## Set working directory 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/IoT Analytics")

#Load required packages
library(imputeTS)
library(tidyr)
library(missForest)

################################################

## Introduce NAs in the dataset 

reg.timeser <- seq.POSIXt(as.POSIXct("2007-01-01 00:00:00", tz = "CET"), 
                          as.POSIXct("2009-12-31 23:59:00", tz = "CET"), by = "1 min")

reg.timeser <- data.frame(reg.timeser)
colnames(reg.timeser) <- "DateTime"

################################################

## Merge energy dataset with full time stamps -- 2007-2009

energy.merged <- merge(energy, reg.timeser, by = "DateTime", all = TRUE)

#Re-create Min:Year columns to get rid of extra NAs
energy.merged <- energy.merged[-c(2:7)] #Remove original times tamps 
energy.merged$Year <- year(energy.merged$DateTime)
energy.merged$Month <- month(energy.merged$DateTime)
energy.merged$Week <- week(energy.merged$DateTime)
energy.merged$Day <- weekdays(energy.merged$DateTime, abbreviate = TRUE)
energy.merged$Hour <- hour(energy.merged$DateTime)
energy.merged$Minute <- minute(energy.merged$DateTime)

#Re-order columns
energy.merged <- energy.merged[, c(1, 10, 9, 8, 7, 6, 5, 2:4)]

#I have no idea how to remove the 180 NAs which arise from time changes 
#So, I used na.omit()
for (i in 1:seq_along(energy.merged)) {
  #energy.merged[[i]][energy.merged[[i]] %in% NA] <- 
    energy.merged[[i]][is.na(reg.timeser[[i]])] <- energy.merged[[i]]
  }

energy.merged <- na.omit(energy.merged)

energy.merged$Day <- factor(energy.merged$Day, levels = c("Mon", "Tue", "Wed",
                                            "Thu", "Fri", "Sat",
                                            "Sun"))

################################################

#Impute missing values for the 3 sub-meters 
energy.merged$Sub_metering_1 <- na_interpolation(energy.merged$Sub_metering_1, option = "spline")
energy.merged$Sub_metering_2 <- na_interpolation(energy.merged$Sub_metering_2, option = "spline")
energy.merged$Sub_metering_3 <- na_interpolation(energy.merged$Sub_metering_3, option = "spline")

################################################

## Weekly avg sub-meter reading 

weekly.meter <- weekly.grouping(energy.merged)
energy.merged.weekly.sub1 <- data.frame(weekly.meter$sub1)
energy.merged.weekly.sub2 <- data.frame(weekly.meter$sub2)
energy.merged.weekly.sub3 <- data.frame(weekly.meter$sub3)

## Monthly avg sub-meter reading 

monthly.meter <- monthly.grouping(energy.merged)
energy.merged.monthly.sub1 <- data.frame(monthly.meter$sub1)
energy.merged.monthly.sub2 <- data.frame(monthly.meter$sub2)
energy.merged.monthly.sub3 <- data.frame(monthly.meter$sub3)

################################################

## Time series analysis 

#Weekly
sub1.ts <- time.series.analysis(energy.merged.weekly.sub1$avg.sub1)
sub2.ts <- time.series.analysis(energy.merged.weekly.sub2$avg.sub2)
sub3.ts <- time.series.analysis(energy.merged.weekly.sub3$avg.sub3)

#Monthly
sub1.ts.monthly <- time.series.analysis(energy.merged.monthly.sub1$avg.sub1)
sub2.ts.monthly <- time.series.analysis(energy.merged.monthly.sub1$avg.sub1)
sub3.ts.monthly <- time.series.analysis(energy.merged.monthly.sub3$avg.sub3)

