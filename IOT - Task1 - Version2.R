#### IOT ANALYTICS
### Domain Research and Exploratory Data Analysis
## by Alican Tanaçan
# Version 2: Data Exploration

# Libraries ----

# Importing The Data ----
ConsumptionData <- read.table("household_power_consumption.txt",
                              header = T,
                              sep = ";")
summary(ConsumptionData)

head(ConsumptionData)

tail(ConsumptionData)

str(ConsumptionData)


