## IoT Analytics - Visualisation & time series regression modelling 

## Set working directory 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/IoT Analytics")

#Load required packages
library(plotly)
library(dplyr)
library(stats)
library(ggplot2)
library(ggfortify)
library(forecast)

################################################

## Initial visualisations -- Adjusting granularity 

#Subset the 9th day of January 2008 - All observations
house.day <- filter(energy, Year == 2008 & Month == 1 & Week == 2 & Day == "Wed")

#Plot sub-meter 1
plot_ly(house.day, x = ~house.day$DateTime, y = ~house.day$Sub_metering_1, type = 'scatter', mode = 'lines')

#Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(house.day, x = ~house.day$DateTime, y = ~house.day$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~house.day$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~house.day$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#Subset the 9th day of January 2008 - 10 Minute frequency
house.day10 <- filter(energy, Year == 2008 & Month == 1 & Week == 2 & Day == "Wed" & 
                        (Minute == 0 | Minute == 10 | Minute == 20 | Minute == 30 | Minute == 40 | Minute == 50))

#Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(house.day10, x = ~house.day10$DateTime, y = ~house.day10$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~house.day10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~house.day10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#Subset 1 week - 1st week of August - 6 hr frequency 
house.week <- filter(energy, Year == 2009 & Month == 8 & Week == 31 & 
                       (Hour == 0 | Hour == 6 | Hour == 12 | Hour == 18))

#Plot sub-meter 1, 2 and 3 with title, legend and labels 
plot_ly(house.week, x = ~house.week$DateTime, y = ~house.week$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~house.week$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~house.week$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption during first week of August, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#Subset 12 hrs -- 8am to 10pm; 1st Feb 2007 
house.hr <- filter(energy, Year == 2007 & Month == 2 & Week == 5 & Day == "Thu" &
                        (Hour >= 10 & Hour <= 22))

#Plot sub-meter 1, 2 and 3 with title, legend and labels 
plot_ly(house.hr, x = ~house.hr$DateTime, y = ~house.hr$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~house.hr$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~house.hr$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption over 12 hours, 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

################################################

## Time series analysis 

################################################

#Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house.weekly <- filter(energy, Day == "Mon" & Hour == 20 & Minute == 1)

#Create TS object 
house.weekly.ts.sub3 <- ts(house.weekly$Sub_metering_3, frequency = 52, start = c(2007, 1))
house.weekly.ts.sub2 <- ts(house.weekly$Sub_metering_2, frequency = 52, start = c(2007, 1))
house.weekly.ts.sub1 <- ts(house.weekly$Sub_metering_1, frequency = 52, start = c(2007, 1))

#Visualise
autoplot(house.weekly.ts.sub3, ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", 
         main = "Heater & A/C sub-meter power usage")

autoplot(house.weekly.ts.sub2, ts.colour = 'orange', xlab = "Time", ylab = "Watt Hours", 
         main = "Laundry room sub-meter power usage")

autoplot(house.weekly.ts.sub1, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", 
         main = "Kitchen sub-meter power usage")

################################################

## Apply time series linear regression 

fit.sm3 <- tslm(house.weekly.ts.sub3 ~ trend + season) 
fit.sm2 <- tslm(house.weekly.ts.sub2 ~ trend + season)
fit.sm1 <- tslm(house.weekly.ts.sub1 ~ trend + season)

################################################

## Forecasting 

#Forecast ahead 20 time periods; confidence levels 80 & 90
forecast.sm3 <- forecast(fit.sm3, h = 20, level = c(80, 90))
forecast.sm2 <- forecast(fit.sm2, h = 20, level = c(80, 90))
forecast.sm1 <- forecast(fit.sm1, h = 20, level = c(80, 90))

#Plot forecasts
plot(forecast.sm3, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time")
plot(forecast.sm2, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time")
plot(forecast.sm1, ylim = c(0, 20), ylab = "Watt-Hours", xlab = "Time")

################################################

## Decomposition -- trend, seasonal, & remainder

decom.sub3 <- decompose(house.weekly.ts.sub3)
decom.sub2 <- decompose(house.weekly.ts.sub2)
decom.sub1 <- decompose(house.weekly.ts.sub1)

#Plot decomposed 
plot(decom.sub3)
plot(decom.sub2)
plot(decom.sub1)

################################################

## Holt-Winter forecasting

#Seasonal adjusting of sub-meters  by subtracting the seasonal component & plot
house.weekly.ts.sub3.adj <- house.weekly.ts.sub3 - decom.sub3$seasonal
house.weekly.ts.sub2.adj <- house.weekly.ts.sub2 - decom.sub2$seasonal
house.weekly.ts.sub1.adj <- house.weekly.ts.sub1 - decom.sub1$seasonal
autoplot(house.weekly.ts.sub3.adj)

#Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(house.weekly.ts.sub3.adj))
plot(decompose(house.weekly.ts.sub2.adj))
plot(decompose(house.weekly.ts.sub1.adj))

#Holt Winters Exponential Smoothing & Plot
holtwint.sm3 <- HoltWinters(house.weekly.ts.sub3.adj, beta = FALSE, gamma = FALSE)
holtwint.sm2 <- HoltWinters(house.weekly.ts.sub2.adj, beta = FALSE, gamma = FALSE)
holtwint.sm1 <- HoltWinters(house.weekly.ts.sub1.adj, beta = FALSE, gamma = FALSE)
plot(holtwint.sm3, ylim = c(0, 25))

#HoltWinters forecast & plot
holtwint.forecast.sub3 <- forecast(holtwint.sm3, h = 25, level = c(10, 25))
holtwint.forecast.sub2 <- forecast(holtwint.sm2, h = 25, level = c(10, 25))
holtwint.forecast.sub1 <- forecast(holtwint.sm1, h = 25, level = c(10, 25))

plot(holtwint.forecast.sub3, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
plot(holtwint.forecast.sub2, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
plot(holtwint.forecast.sub1, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

#Only forecasted area
plot(holtwint.forecast.sub3, ylim = c(0, 20), ylab= "Watt-Hours", xlab = "Time - Sub-meter 3", start(2010))
plot(holtwint.forecast.sub2, ylim = c(0, 20), ylab= "Watt-Hours", xlab = "Time - Sub-meter 2", start(2010))
plot(holtwint.forecast.sub1, ylim = c(0, 20), ylab= "Watt-Hours", xlab = "Time - Sub-meter 1", start(2010))


