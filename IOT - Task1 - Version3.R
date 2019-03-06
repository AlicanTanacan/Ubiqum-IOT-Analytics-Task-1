### ----------------------- IOT ANALYTICS -------------------------- ###
### -------- Domain Research and Exploratory Data Analysis --------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ------ Version 3: Feature Engineering and Data Cleaning -------- ###

## Libraries ----
library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)

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

# Combine tables into one data frame using dplyr
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
EnergyConsumption$Month <- month(EnergyConsumption$DateTime)
EnergyConsumption$MonthName <- month(EnergyConsumption$DateTime, label = T)

# Inspect the data types
str(EnergyConsumption)
summary(EnergyConsumption)

# Change the name of variables
EnergyConsumption <- EnergyConsumption %>% rename(GAP = Global_active_power,
                                                  SM1 = Sub_metering_1,
                                                  SM2 = Sub_metering_2,
                                                  SM3 = Sub_metering_3)

# Define active power, which represents the active energy consumed every minute 
# (in watt hour) in the household by electrical equipment not measured in 
# sub-meterings 1, 2 and 3
EnergyConsumption$AP_kWh <- ((EnergyConsumption$GAP * 1000/60) - 
                                    EnergyConsumption$SM1 - 
                                    EnergyConsumption$SM2 - 
                                    EnergyConsumption$SM3)

EnergyConsumption$GAP_kWh <- EnergyConsumption$GAP * 1000/60

# Create another variable that shows year and month in one column
EnergyConsumption$Date <- as.yearmon(paste(EnergyConsumption$Year, 
                                           EnergyConsumption$Month,
                                           sep = "-"))

# Change Data Types
str(EnergyConsumption)

EnergyConsumption <- 
  EnergyConsumption %>% 
  mutate_at(c("Year", 
              "Date", 
              "Month",
              "MonthName"), 
            as.factor)

# Create Season variable for each year and month
EnergyConsumption <- 
  EnergyConsumption %>% 
  mutate(
    Seasons = 
           case_when(Date %in% c("Dec 2006", "Jan 2007", "Feb 2007") ~ "Winter 2007",
                     Date %in% c("Mar 2007", "Apr 2007", "May 2007") ~ "Spring 2007",
                     Date %in% c("Jun 2007", "Jul 2007", "Aug 2007") ~ "Summer 2007",
                     Date %in% c("Sep 2007", "Oct 2007", "Nov 2007") ~ "Fall 2007",
                     Date %in% c("Dec 2007", "Jan 2008", "Feb 2008") ~ "Winter 2008",
                     Date %in% c("Mar 2008", "Apr 2008", "May 2008") ~ "Spring 2008",
                     Date %in% c("Jun 2008", "Jul 2008", "Aug 2008") ~ "Summer 2008",
                     Date %in% c("Sep 2008", "Oct 2008", "Nov 2008") ~ "Fall 2008",
                     Date %in% c("Dec 2008", "Jan 2009", "Feb 2009") ~ "Winter 2009",
                     Date %in% c("Mar 2009", "Apr 2009", "May 2009") ~ "Spring 2009",
                     Date %in% c("Jun 2009", "Jul 2009", "Aug 2009") ~ "Summer 2009",
                     Date %in% c("Sep 2009", "Oct 2009", "Nov 2009") ~ "Fall 2009",
                     Date %in% c("Dec 2009", "Jan 2010", "Feb 2010") ~ "Winter 2010",
                     Date %in% c("Mar 2010", "Apr 2010", "May 2010") ~ "Spring 2010",
                     Date %in% c("Jun 2010", "Jul 2010", "Aug 2010") ~ "Summer 2010",
                     Date %in% c("Sep 2010", "Oct 2010", "Nov 2010") ~ "Fall 2010" ))

EnergyConsumption <-
  EnergyConsumption %>% 
  mutate(
    SeasonName = ifelse(MonthName %in% c("Dec", "Jan", "Feb"), "Winter",
                        ifelse(MonthName %in% c("Mar", "Apr", "May"), "Spring",
                               ifelse(MonthName %in% c("Jun", "Jul", "Aug"), "Summer", 
                                      "Fall"))))

# Change data types
str(EnergyConsumption)

EnergyConsumption$SeasonName <- as.factor(EnergyConsumption$SeasonName)
            
EnergyConsumption$Seasons <- factor(EnergyConsumption$Seasons, levels =c("Winter 2007",
                                       "Spring 2007",
                                       "Summer 2007",
                                       "Fall 2007",
                                       "Winter 2008",
                                       "Spring 2008",
                                       "Summer 2008",
                                       "Fall 2008",
                                       "Winter 2009",
                                       "Spring 2009",
                                       "Summer 2009",
                                       "Fall 2009",
                                       "Winter 2010",
                                       "Spring 2010",
                                       "Summer 2010",
                                       "Fall 2010"), ordered = T)
levels(EnergyConsumption$Seasons)
head(EnergyConsumption$Seasons)

# Define ready data
EnergyConsumptionReady <- subset(EnergyConsumption, select = -c(Year:MonthName))
EnergyConsumptionReady$GAP <- NULL

# To save the clean data as rds
saveRDS(EnergyConsumptionReady, file = "EnergyCleanData.rds")
