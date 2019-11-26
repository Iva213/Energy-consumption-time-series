## IoT Analytics - Energy consumption analysis 

## Set working directory 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/IoT Analytics")

#Load required packages
library(RMySQL)
library(dplyr)

################################################

## Load in data 
#Sub-meter 1: kitchen; sub-meter 2: laundry room; sub-meter 3: water-heater & air-conditioner (W/hr)
#Date: dd/mm/yyyy --> Dec. 2006 - Nov. 2010

#Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

#List the tables contained in the database 
dbListTables(con)

#Download data using attribute names to specify specific attributes needed
yr.2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2006")

yr.2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2007")

yr.2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2008")

yr.2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2009")

yr.2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 
                      FROM yr_2010")

energy <- bind_rows(yr.2006, yr.2007, yr.2008, yr.2009, yr.2010)

################################################











