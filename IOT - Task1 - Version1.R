### ----------------------- IOT ANALYTICS -------------------------- ###
### -------- Domain Research and Exploratory Data Analysis --------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### --- Version 1: Obtaining Data Using SQL Query & Time Series ---- ###

## Libraries ----
library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)

## Importing Data ----
# Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List the tables contained in the database
dbListTables(con)

# Lists attributes contained in a table
dbListFields(con,"yr_2006")

# Use asterisk to specify all attributes or attribute names to specify attributes for download
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

# Combine tables into one dataframe using dplyr
EnergyConsumptionFullData <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)
summary(EnergyConsumptionFullData)


## Preprocessing and Feature Engineering ----
# Reserve the original data for later inspection
EnergyConsumption <- EnergyConsumptionFullData

# Combine Date and Time attribute values in a new attribute column
EnergyConsumption <- cbind(EnergyConsumption, 
                           paste(EnergyConsumption$Date,EnergyConsumption$Time), 
                           stringsAsFactors = FALSE)

# Give the new attribute in the 6th column a header name
colnames(EnergyConsumption)[7] <-"DateTime"

# Remove unnecessary attributes
EnergyConsumption[, c(1,2)] <- NULL

# Move the DateTime attribute within the dataset
EnergyConsumption <- EnergyConsumption[,c(ncol(EnergyConsumption), 1:(ncol(EnergyConsumption)-1))]
head(EnergyConsumption)

# Convert DateTime from POSIXlt to POSIXct 
EnergyConsumption$DateTime <- as.POSIXct(EnergyConsumption$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
attr(EnergyConsumption$DateTime, "tzone") <- "Europe/Paris"

# Inspect the data types
str(EnergyConsumption)

# Create new time period attributes with lubridate
EnergyConsumption$Year <- year(EnergyConsumption$DateTime)
EnergyConsumption$Quarter <- quarter(EnergyConsumption$DateTime)
EnergyConsumption$Month <- month(EnergyConsumption$DateTime)
EnergyConsumption$Week <- week(EnergyConsumption$DateTime)
EnergyConsumption$Weekday <- weekdays(EnergyConsumption$DateTime)
EnergyConsumption$Day <- day(EnergyConsumption$DateTime)
EnergyConsumption$Hour <- hour(EnergyConsumption$DateTime)
EnergyConsumption$Minute <- minute(EnergyConsumption$DateTime)

str(EnergyConsumption)
summary(EnergyConsumption)

