#### Data Munging & Wrangling
### Introduction to Dypler - 1
## Nycflights13 Tutorial 
## Taken From: http://www.rpubs.com/howelb/32599
# Alican Tanaçan

# Install and load the "nycflights13" dataset.
install.packages("nycflights13",
                 repos = 'http://cran.us.r-project.org')

# Library and Import the data.
library("nycflights13",
        lib.loc = "D:/R-3.5.2/library")
library(rlang)
data_raw <- flights

# Display the "head" and "tail" of the dataset.
head(data_raw)
tail(data_raw)

# Display the summary statistics of the data.
summary(data_raw)

# Display the names found in "data_raw".
names(data_raw)

# Display the structure of "data_raw".
str(data_raw)

# Categorize 'origin' as a factor and display its resulting levels.
data_raw$origin = as.factor(data_raw$origin)
levels(data_raw$origin)

# Categorize 'dest' as a factor and display its resulting levels.
data_raw$dest = as.factor(data_raw$dest)
levels(data_raw$dest)

# Remove any rows that contain "NA" in "data_raw", creating "data_clean".
data_clean<-na.omit(data_raw)

# Randomly take a sample of 5000 observations from "data_clean", creating "nycflights".
flight.index = sample(1:nrow(data_clean),5000,replace=FALSE)
nycflights<-data_clean[flight.index,]

# Display the summary statistics of "nycflights".
summary(nycflights)

# Display the names found in "nycflights".
names(nycflights)

# Display the structure of "nycflights".
str(nycflights)

# Display the head and tail of "nycflights".
head(nycflights)
tail(nycflights)

# Display the levels of 'origin' and 'dest' within "nycflights".
levels(nycflights$origin)
levels(nycflights$dest)

par(mfrow=c(1,1))
# Create a histogram of Arrival Time Delays ('arr_delay') across all 2013 flights.
hist(nycflights$arr_delay, main = "Arrival Time Delays [in minutes]")

par(mfrow=c(1,1))
# Create a boxplot of Arrival Time Delays ('arr_delay') at each 
# destination airport ('dest') [ylim = c(min|'arr_delay',max|'arr_delay')].
boxplot(nycflights$arr_delay~nycflights$dest, 
        main = "Arrival Time Delays [in minutes]", 
        ylim = c(min(nycflights$arr_delay),
                 max(nycflights$arr_delay)), 
        ylab = "Minutes")

par(mfrow=c(1,1))
# Create a boxplot of Arrival Time Delays ('arr_delay') at each 
# destination airport ('dest') [ylim = c(~1st Quarter|'arr_delay',~3rd Quarter|'arr_delay')].
# (Gives better indication of median differences among destination airport locations.)
boxplot(nycflights$arr_delay~nycflights$dest, 
        main = "Arrival Time Delays [in minutes]", 
        ylim = c(-20,20), 
        ylab = "Minutes")

# Perform an analysis of variance (ANOVA) for the different
# mean values observed for the delays in arrival time, given the factor 'origin'.
model_origin <- aov(arr_delay~origin,nycflights)
anova(model_origin)

# Perform an analysis of variance (ANOVA) for the different
# mean values observed for the delays in arrival time, given the factor 'dest'.
model_dest <- aov(arr_delay~dest,nycflights)
anova(model_dest)

# Perform an analysis of variance (ANOVA) for the different
# mean values observed for the delays in arrival time, given the interaction
# of 'origin' and 'dest'.
model_interaction <- aov(arr_delay~origin*dest,nycflights)
anova(model_interaction)

par(mfrow=c(1,1))
# Create an interaction plot that plots the mean values ofthe delays
# in arrival time('arr_delay') against the interaction of both 'origin' and 'dest'.
interaction.plot(nycflights$origin,nycflights$dest,nycflights$arr_delay)

# Display summary statistics of nycflights$arr_delay.
summary(nycflights$arr_delay)
str(nycflights)

# Display standard deviation of nycflights$arr_delay.
sd(nycflights$arr_delay, na.rm = FALSE)

# Create a Normal Q-Q Plot for Delay in Arrival Time Data.
qqnorm(nycflights$arr_delay)
qqline(nycflights$arr_delay)

# Create a Normal Q-Q Plot of the residuals for "model_origin".
qqnorm(residuals(model_origin))
qqline(residuals(model_origin))

# Create a Normal Q-Q Plot of the residuals for "model_dest".
qqnorm(residuals(model_dest))
qqline(residuals(model_dest))

# Create a Normal Q-Q Plot of the residuals for "model_interaction".
qqnorm(residuals(model_interaction))
qqline(residuals(model_interaction))

# Perform Shapiro-Wilk Test of Normality on Delay in Arrival Time Data
# within "nycflights" dataset (normality is assummed if p > 0.1).
shapiro.test(nycflights$arr_delay)

# Perform Kruskal-Wallis Rank Sum Test on Delay in Arrival Time Data
# within "nycflights" dataset for both 'origin' and 'dest'
# (identical populations is assummed if p > 0.05).
kruskal.test(nycflights$arr_delay, 
             nycflights$origin)
kruskal.test(nycflights$arr_delay, 
             nycflights$dest)

# Create a "Quality of Fit Model" that plots the residuals of "model_origin"
# against its fitted model.
plot(fitted(model_origin),residuals(model_origin))

# Create a "Quality of Fit Model" that plots the residuals of "model_dest"
# against its fitted model.
plot(fitted(model_dest),residuals(model_dest))

# Create a "Quality of Fit Model" that plots the residuals of "model_interaction"
# against its fitted model.
plot(fitted(model_interaction),residuals(model_interaction))