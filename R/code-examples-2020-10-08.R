
# Code from the seminar 2020-10-08 ###############



#### Load packages ###########

library(tidyverse)
# Contains almost everything we need!

#### Import online data sets  ####

# Alternative 1: Store the file


download.file(url = "https://ndownloader.figstatic.com/files/14702420", 
              destfile = "./data/hypertrophy.csv")



hypertrophy <- read_csv("./data/hypertrophy.csv")
# read_csv is from the package readr


# Alternative 2: Direct download

hypertrophy <- read_csv("https://ndownloader.figstatic.com/files/14702420")


##### Explore the data set ################

# View the full data set 
View(hypertrophy)

# Show summary statistics 
summary(hypertrophy)

# From the dplyr package, glimpse the data
glimpse(hypertrophy)


##### Variables in the data set that we are interested in 

var_interest <- c("SUB_ID", "GROUP", "CLUSTER", "AGE", "T1_BODY_MASS",
                  "PERCENT_TYPE_II_T1", "Squat_3RM_kg", "DXA_LBM_1", 
                  "DXA_FM_T1", "SQUAT_VOLUME")

hyp2 <- hypertrophy %>%
  select(all_of(var_interest)) %>%
  print()
# Specify all_of(var_interest) to selet variables correctly

# Check the new data set
glimpse(hyp2)

#### Group by and summarise ############

hyp2 %>%
  group_by(CLUSTER) %>%
  print()

# The data contains NA, remember the study design

# Filter away NA

# When data contains NA, we can use the function is.na() to test
# if we have NA.

var <- c("LOW", "HIGH", NA) 

# Test if any are NA
is.na(var)

# Reverese the test with !
!is.na(var)


hyp2 %>%
  filter(is.na(CLUSTER)) %>%
  print()

# reverse the test to filter away
hyp2 %>%
  filter(!is.na(CLUSTER)) %>%
  print()


# Now the data set only contains participants from 
# the two groups (clusters)

hyp2 %>%
  filter(!is.na(CLUSTER)) %>%

  select(-GROUP) %>%
  
  pivot_longer(names_to = "variable", 
               values_to = "value", 
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  summarise(m = mean(value), 
            s = sd(value)) %>% 
  mutate(ms = paste(round(m, 1), 
                    " (", 
                    round(s, 1), 
                    ")", sep = ""),
         
         CLUSTER = factor(CLUSTER, levels = c("LOW", "HIGH"), 
                          labels = c("LOW (n = 10)", 
                                     "HIGH (n = 10)")),
         
         variable = factor(variable, 
                           levels = c("AGE", 
                                      "T1_BODY_MASS", 
                                      "DXA_LBM_1", 
                                      "DXA_FM_T1", 
                                      "PERCENT_TYPE_II_T1", 
                                      "Squat_3RM_kg", 
                                      "SQUAT_VOLUME"), 
                           labels = c("Age (years)", 
                                      "Body mass (kg)", 
                                      "LBM (kg)", 
                                      "FM (kg)", 
                                      "Type II (%)", 
                                      "Squat 3RM (kg)", 
                                      "Total training volume (kg)"))) %>%
  select(-m, -s) %>%

  pivot_wider(names_from = CLUSTER, 
              values_from = ms) %>%
  arrange(variable) %>%
  select(variable, `LOW (n = 10)`, `HIGH (n = 10)`) %>%
  
  print()














