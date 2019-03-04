### ----------------------- IOT ANALYTICS -------------------------- ###
### -------- Domain Research and Exploratory Data Analysis --------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### -- Version 2: Obtaining Original Data & Time Series Treatment -- ###

## Libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)

## Importing The Data ----
ConsumptionData <- read.table("household_power_consumption.txt",
                              header = T,
                              sep = ";")

## Data Exploration ----
summary(ConsumptionData)
head(ConsumptionData)
tail(ConsumptionData)
names(ConsumptionData)
ConsumptionData <- ConsumptionData %>% rename(GAP = Global_active_power,
                                              GRP = Global_reactive_power,
                                              GI = Global_intensity,
                                              SM1 = Sub_metering_1,
                                              SM2 = Sub_metering_2,
                                              SM3 = Sub_metering_3)
names(ConsumptionData)
str(ConsumptionData)
ConsumptionData <- ConsumptionData %>% mutate_at(c("GAP",
                                                   "GRP",
                                                   "Voltage",
                                                   "GI",
                                                   "SM1",
                                                   "SM2",
                                                   "SM3"), as.numeric)

ConsumptionData$Date <- as.Date('16/12/2006',format='%d/%m/%Y')

ConsumptionData$Time <- as.character(ConsumptionData$Time)
ConsumptionData$Date <- as.character(ConsumptionData$Date)

str(ConsumptionData)

## Preprocessing ----
# Exluding NA's from the data
ConsumptionData <- na.omit(ConsumptionData)

# Combine Date and Time attribute values in a new attribute column
ConsumptionData2 <- cbind(ConsumptionData,
                       paste(ConsumptionData$Date,
                             ConsumptionData$Time), 
                       stringsAsFactors=FALSE)

# Give the new attribute in the 9th column a header name 
colnames(ConsumptionData2)[10] <-"DateTime"

# Remove unnecessary attributes
ConsumptionData2[, c(1,2)] <- NULL

# Move the DateTime attribute within the dataset
ConsumptionData2 <- ConsumptionData2[,c(ncol(ConsumptionData2), 
                                        1:(ncol(ConsumptionData2)-1))]
head(ConsumptionData2)

# Convert DateTime from POSIXlt to POSIXct 
ConsumptionData2$DateTime <- as.POSIXct(ConsumptionData2$DateTime, "%Y-%m-%d %H:%M:%S")

# Add the time zone
attr(ConsumptionData2$DateTime, "tzone") <- "Europe/Paris"

# Inspect the data types
str(ConsumptionData2)

# Create new time period attributes with lubridate
ConsumptionData2$year <- year(ConsumptionData2$DateTime)
ConsumptionData2$Quarter <- quarter(ConsumptionData2$DateTime)
ConsumptionData2$Month <- month(ConsumptionData2$DateTime)
ConsumptionData2$Week <- week(ConsumptionData2$DateTime)
ConsumptionData2$Weekday <- weekdays(ConsumptionData2$DateTime)
ConsumptionData2$Day <- day(ConsumptionData2$DateTime)
ConsumptionData2$Hour <- hour(ConsumptionData2$DateTime)
ConsumptionData2$Minute <- minute(ConsumptionData2$DateTime)
str(ConsumptionData2)
summary(ConsumptionData2)