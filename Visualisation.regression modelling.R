## IoT Analytics - Visualisation & time series regression modelling 

## Set working directory 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/IoT Analytics")

#Load required packages
library(plotly)
library(dplyr)

################################################

## Adjusting granularity 

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
