### ----------------------- IOT ANALYTICS -------------------------- ###
### -------- Domain Research and Exploratory Data Analysis --------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ----------- Version 4: Clean Data Visualization ---------------- ###

## Libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(plotly)
library(gridExtra)

## Importing The Clean Data ----
CleanData <- readRDS(file = "EnergyCleanData.rds")

## Feature Engineering (run if you need certain time variables) ----
# CleanData$Year <- year(CleanData$DateTime)
# CleanData$Quarter <- quarter(CleanData$DateTime)
# CleanData$Month <- month(CleanData$DateTime)
# CleanData$Week <- week(CleanData$DateTime)
# CleanData$Weekday <- weekdays(CleanData$DateTime)
# CleanData$Day <- day(CleanData$DateTime)
# CleanData$Hour <- hour(CleanData$DateTime)
# CleanData$Minute <- minute(CleanData$DateTime)
str(CleanData)
summary(CleanData)
head(CleanData$Seasons)

## Data Exploration by Dplyr ----
SeasonsbyAP <- CleanData %>% 
  group_by(Seasons) %>%  
  summarise(meanAP = mean(AP_kWh))

SeasonsbyGAP <- CleanData %>% 
  group_by(Seasons) %>%  
  summarise(meanGAP = mean(GAP_kWh))

SeasonsbySM1 <- CleanData %>% 
  group_by(Seasons) %>%  
  summarise(meanSM1 = mean(SM1))

SeasonsbySM2 <- CleanData %>% 
  group_by(Seasons) %>%  
  summarise(meanSM2 = mean(SM2))

SeasonsbySM3 <- CleanData %>% 
  group_by(Seasons) %>%  
  summarise(meanSM3 = mean(SM3))

YearsbyAP <- CleanData %>%
  group_by(year(DateTime)) %>% 
  summarise(meanAP = mean(AP_kWh))

MonthsbyAP <- CleanData %>%
  group_by(month(DateTime)) %>% 
  summarise(meanAP = mean(AP_kWh))

DaysbyAP <- CleanData %>%
  group_by(date(DateTime)) %>% 
  summarise(meanAP = mean(AP_kWh))

DatesbyAP <- CleanData %>% 
  group_by(Date) %>% 
  summarise(meanAP = mean(AP_kWh))

DatesbyAP2007 <- CleanData %>% 
  filter(year(DateTime) == 2007) %>% 
  group_by(Date) %>% 
  summarise(meanAP = mean(AP_kWh))

DatesbyAP2008 <- CleanData %>% 
  filter(year(DateTime) == 2008) %>% 
  group_by(Date) %>% 
  summarise(meanAP = mean(AP_kWh))

DatesbyAP2009 <- CleanData %>% 
  filter(year(DateTime) == 2009) %>% 
  group_by(Date) %>% 
  summarise(meanAP = mean(AP_kWh))

DatesbyAP2010 <- CleanData %>% 
  filter(year(DateTime) == 2010) %>% 
  group_by(Date) %>% 
  summarise(meanAP = mean(AP_kWh))
  
## Data Visualization ----
# Bar graphs for "global active power" and "active power" in each season
ggplot(CleanData, aes(x = factor(Seasons), y = GAP_kWh)) + 
  stat_summary(fun.y = "mean", geom = "bar")

ggplot(CleanData, aes(x = factor(Seasons), y = AP_kWh)) + 
  stat_summary(fun.y = "mean", geom = "bar")

# Smooth line graph for average sub meter measurements and electic consumptions
CleanData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanAP = mean(AP_kWh)) %>%
  ggplot(aes(DATE, MeanAP)) + 
  geom_line(color = "red") + 
  geom_smooth(se = F) +
  labs(title = "Average Active Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot1
ggplotly(plot1)

CleanData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP_kWh)) %>%
  ggplot(aes(DATE, MeanGAP)) + 
  geom_line(color = "orange") + 
  geom_smooth(se = F) +
  labs(title = "Average Global Active Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot2
ggplotly(plot2)

CleanData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanSM1 = mean(SM1)) %>%
  ggplot(aes(DATE, MeanSM1)) + 
  geom_line(color = "green") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Metering 1 Power Metered by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot3
ggplotly(plot3)

CleanData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanSM2 = mean(SM2)) %>%
  ggplot(aes(DATE, MeanSM2)) + 
  geom_line(color = "pink") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Metering 2 Power Metered by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot4
ggplotly(plot4)

CleanData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanSM3 = mean(SM3)) %>%
  ggplot(aes(DATE, MeanSM3)) + 
  geom_line(color = "yellow") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Metering 3 Power Metered by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot5
ggplotly(plot5)

CleanData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanAP = mean(AP_kWh),
            MeanGAP = mean(GAP_kWh),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
    geom_smooth(aes(y = MeanAP), se = T, color = "red") +
    geom_smooth(aes(y = MeanGAP), se = T, color = "orange") +
    geom_smooth(aes(y = MeanSM1), se = T, color = "green") +
    geom_smooth(aes(y = MeanSM2), se = T, color = "pink") +
    geom_smooth(aes(y = MeanSM3), se = T, color = "yellow") +
    labs(title = "Average Energy Power Used by Day") + 
    ylab("kWh") + 
    xlab("Time") + 
    theme_light() -> plot6
ggplotly(plot6)

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)