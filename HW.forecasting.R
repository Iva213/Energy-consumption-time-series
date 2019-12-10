## Time series analysis - linear and Holt-Winters forecasting 

## Set working directory 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/IoT Analytics")

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
