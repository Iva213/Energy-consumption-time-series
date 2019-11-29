## IoT Analytics - Energy consumption analysis 

## Set working directory 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/IoT Analytics")

#Load required packages
library(RMySQL)
library(dplyr)
library(ggplot2)
library(lubridate)

################################################

## Initial pre-processing 

#Load in data --
#sub-meter 1: kitchen (dishwasher, oven, microwave); 
#sub-meter 2: laundry room (washing-machine, tumble-drier, refrigerator, light); 
#sub-meter 3: water-heater & air-conditioner (Wh)
#Date: dd/mm/yyyy --> Dec. 2006 - Nov. 2010
#SQL removes all NAs

#Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

#List the tables contained in the database 
dbListTables(con)

#Download data using attribute names to specify specific attributes needed
#1 month of data
yr.2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2006")

yr.2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2007")

yr.2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2008")

yr.2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2009")
#11 months of data 
yr.2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2010")
#Bind df with 1 year worth of data
energy <- bind_rows(yr.2007, yr.2008, yr.2009)

#Combine Date and Time attribute values in a new attribute column
energy <-cbind(energy, paste(energy$Date, energy$Time), stringsAsFactors=FALSE)
colnames(energy)[6] <- "DateTime"
#Move the DateTime attribute within the dataset
energy <- energy[, c(ncol(energy), 1:(ncol(energy)-1))] ##Makes this happen --> 6 1 2 3 4 5 

#Convert DateTime from POSIXlt to POSIXct 
energy$DateTime <- as.POSIXct(energy$DateTime, "%Y-%m-%d %H:%M:%S", tz = "CET")
#Add time zone
#attr(energy$DateTime, "tzone") <- "Europe/Paris"

#Create attributes
energy$Year <- year(energy$DateTime)
energy$Month <- month(energy$DateTime)
energy$Week <- week(energy$DateTime)
energy$Day <- weekdays(energy$DateTime, abbreviate = TRUE)
energy$Hour <- hour(energy$DateTime)
energy$Minute <- minute(energy$DateTime)

energy <- energy[, c(1, 10, 9, 8, 7, 6, 5, 2:4)] ##Re-order
energy <- energy[, -c(2:3)] ##Remove Date and Time cols 


energy$Day <- factor(energy$Day, levels = c("Mon", "Tue", "Wed",
                                                  "Thu", "Fri", "Sat",
                                                  "Sun"))
################################################

## Visualisation

#2007
filt.2007 <- energy %>%
  #group_by(Day, Month) %>%
  #mutate(Average = rowMeans(energy[, 8:10])) %>%
  filter(Year == 2007) 
  #summarise_all(funs(mean))

filt.2007 <- aggregate(cbind(Sub_metering_1, Sub_metering_2, Sub_metering_3) ~ Day + Month, filt.2007, mean, na.rm=TRUE) 

ggplot(filt.2007, aes(Day, Sub_metering_1, col = "red")) + 
  geom_line(group = 1) +
  geom_line(aes(y = Sub_metering_2, col = "blue"), group = 1) + 
  geom_line(aes(y = Sub_metering_3, col = "black"), group = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  facet_grid(. ~ Month) + 
  ylim(0, 10) +
  ylab("Meter reading (Wh)") +
  scale_color_discrete(name = "Sub-meter", labels = c("3", "2", "1"))

#2008
filt.2008 <- energy %>%
  filter(Year == 2008) 

filt.2008 <- aggregate(cbind(Sub_metering_1, Sub_metering_2, Sub_metering_3) ~ Day + Month, filt.2008, mean, na.rm=TRUE) 

ggplot(filt.2008, aes(Day, Sub_metering_1, col = "red")) + 
  geom_line(group = 1) +
  geom_line(aes(y = Sub_metering_2, col = "blue"), group = 1) + 
  geom_line(aes(y = Sub_metering_3, col = "black"), group = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  facet_grid(. ~ Month) + 
  ylim(0, 10) +
  ylab("Meter reading (Wh)") +
  scale_color_discrete(name = "Sub-meter", labels = c("3", "2", "1"))

#Look at August -- b/c of very low meter readings 
filt.2008.8 <- energy %>%
  filter(Year == 2007 & Month == 8) 

filt.2008.8 <- aggregate(cbind(Sub_metering_1, Sub_metering_2, Sub_metering_3) ~ Hour + Day, filt.2008.8, mean, na.rm=TRUE) 

ggplot(filt.2008.8, aes(Hour, Sub_metering_1, col = "red")) +
  geom_line(group = 1) +
  geom_line(aes(y = Sub_metering_2, col = "blue"), group = 1) + 
  geom_line(aes(y = Sub_metering_3, col = "black"), group = 1) +
  facet_grid(. ~ Day) +
  ylab("Meter reading (Wh)") +
  scale_color_discrete(name = "Sub-meter", labels = c("3", "2", "1"))

#2009
filt.2009 <- energy %>%
  filter(Year == 2009) 

filt.2009 <- aggregate(cbind(Sub_metering_1, Sub_metering_2, Sub_metering_3) ~ Day + Month, filt.2009, mean, na.rm=TRUE) 

ggplot(filt.2009, aes(Day, Sub_metering_1, col = "red")) + 
  geom_line(group = 1) +
  geom_line(aes(y = Sub_metering_2, col = "blue"), group = 1) + 
  geom_line(aes(y = Sub_metering_3, col = "black"), group = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  facet_grid(. ~ Month) + 
  ylim(0, 10) +
  ylab("Meter reading (Wh)") +
  scale_color_discrete(name = "Sub-meter", labels = c("3", "2", "1"))

