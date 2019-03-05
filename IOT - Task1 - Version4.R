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

## Importing The Clean Data ----
Data <- readRDS(file = "EnergyCleanData.rds")
str(Data)

## Data Visualization ----
ggplot(Data, aes(x = factor(Seasons), y = GAP)) + 
  stat_summary(fun.y = "mean", geom = "bar")

ggplot(Data, aes(x = factor(Seasons), y = ActivePower)) + 
  stat_summary(fun.y = "mean", geom = "bar")

Data %>% 
  group_by(date(DateTime)) %>% 
  summarise(mean = mean(ActivePower)) %>%
  ggplot(aes("date(DateTime)", mean)) + 
  geom_line(color = "firebrick1") + 
  geom_smooth(se = F) + 
  labs(title = "Mean active energy consumed by day") + 
  ylab("Watt/h") + xlab("Time") + theme_light() -> g1
ggplotly(g1)
