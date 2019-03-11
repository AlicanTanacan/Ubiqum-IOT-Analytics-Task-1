### ----------------------- IOT ANALYTICS -------------------------- ###
### ------------- Ubiqum Dplyr Workshop for Students --------------- ###
### --------------------- by Alican Tanaçan ------------------------ ###

library(dplyr)

msleep <- readRDS(file = "msleep_ggplot2.rds")

summary(msleep)

# Exercises ####

# 1. Select the "name" column and all columns starting with "sl"
msleepdata1 <- select(msleep, name, starts_with("sl"))

# 2. Let's take a look at the average sleep time of the animals according to their eating habits.
# Create a new datafame with two columns: "vore" and "mean_sleep"
msleepdata2 <- 
  msleep %>% 
  na.omit(msleep$vore) %>% 
  group_by(vore) %>% 
  summarise(mean_sleep = mean(sleep_total))
  
# 3. Build again the same df, but this time we want to exclude animals that sleep less than 2 hours or more than 19
msleepdata3 <- 
  msleep %>% 
  na.omit(msleep$vore) %>% 
  filter(sleep_total >= 2, 
         sleep_total <= 19) %>% 
  group_by(vore) %>% 
  summarise(mean_sleep = mean(sleep_total))
  
# 4. Same df as before, but don't want domesticated animals in our table
# Note: we do want animals that have NA in the conservation column
msleepdata4 <- 
  msleep %>% 
  na.omit(msleep$vore) %>%
  na.omit(msleep$conservation) %>% 
  filter(sleep_total > 2, 
         sleep_total < 19, 
         conservation != "domesticated") %>% 
  group_by(vore) %>% 
  summarise(mean_sleep = mean(sleep_total))

# Another option to remove domesticated animals:
# filter(sleep_total > 2, 
#        sleep_total < 19,
#        !conservation %in% "domesticated") %>% 

# 5. Now, exclude NAs from your df

# 6. Add a column to your df with their brain-to-body mass ratio
msleepdata6 <- 
  msleep %>% 
  mutate(braintobody = brainwt / bodywt)

# 7. Add a column to your dataframe with the count of animals for each row
msleepdata7 <-
  msleep %>%
  filter(sleep_total > 2, 
         sleep_total < 19,
         !is.na(vore),
         conservation != "domesticated") %>%
  group_by(vore) %>%
  mutate(braintobody = brainwt / bodywt) %>%
  summarise(mean_sleep = mean(sleep_total),
            mean_btb = round(mean(braintobody, na.rm = T), 3),
            count = n())

# 8. Order your df by the count column in descending order
msleepdata8 <- 
  msleep %>% 
  filter(sleep_total > 2, 
         sleep_total < 19, 
         !is.na(vore), 
         conservation != "domesticated") %>% 
  group_by(vore) %>% 
  mutate(brain_to_body = brainwt / bodywt) %>% 
  summarise(mean_sleep = mean(sleep_total), 
            mean_btb = round(mean(brain_to_body, na.rm = TRUE), 3), 
            count = n()) %>% 
  arrange(count)
