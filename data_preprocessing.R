# Import libraries 

library(dplyr)
library(tidyr)
library("ggpubr")

# Complete.csv Pre-processing -------------------------------------------------

# Import complete.csv

complete_df <- read.csv("C:/Users/james/Desktop/honours_largefiles/complete.csv")

n_distinct(complete_df$PIN) #531 distinct pins 

# Filtering complete participants + changing RL1 to VVR1 

complete_df_2 <- complete_df %>%
  filter(complete == "y") %>%
  mutate(stage = ifelse(stage == "RL1", "VVR1", stage))

n_distinct(complete_df_2$PIN) #320 completers 

# Counting VVR1 completers 

distinct_VVR1 <- complete_df_2 %>%
  filter(stage == "VVR1") %>%
  summarise(distinct_pins = n_distinct(PIN)) #320 patients completed + did VVR1 stage

# Save as .csv 

write.csv(complete_df_2, file = "C:/Users/james/Desktop/honours/complete_df.csv", row.names = FALSE)

# PHQ9 Pre-processing --------------------------------------------------------------------

PHQ9 <- read.csv("C:/Users/James/Desktop/honours/PHQ-9.csv")

n_distinct(PHQ9$PIN) #308 distinct PINS  

# Classify based on score 

PHQ9_df <- PHQ9 %>%
  select(PIN, item, response) %>%
  pivot_wider(names_from = "item", 
              values_from = "response") %>%
  mutate_at(vars(2:10), ~as.numeric(.)) %>%
  mutate(total_score = rowSums(select(., 2:10))) %>%
  mutate(PHQ_classification = ifelse(total_score < 10, "Subclinical", "Major Depression")) %>%
  select(PIN, PHQ_classification)

# Save as .csv

write.csv(PHQ9_df, file = "C:/Users/james/Desktop/honours/PHQ9_df.csv", row.names = FALSE)

# Demographics Pre-processing ---------------------------------------------

# Importing csv file into data frame named "demographics" 
demographics <- read.csv("C:/Users/James/Desktop/honours/demographics.csv")

# Homogenising variable names 
demographics$item <- gsub("^Gender$", "1. Gender", demographics$item)
demographics$item <- gsub("^1. Gênero", "1. Gender", demographics$item)
demographics$item <- gsub("1. Gender $", "1. Gender", demographics$item)
demographics$item <- gsub("^Age$", "2. Age", demographics$item)
demographics$item <- gsub("^5. Nível de educação", "5. Education level", demographics$item)
demographics$item <- gsub("^Education level$", "5. Education level", demographics$item)
demographics$item <- gsub("^5. Education level $", "5. Education level", demographics$item)
demographics$item <- gsub("^6. Estado civil $", "6. Marital status", demographics$item)
demographics$item <- gsub("^Marital status$", "6. Marital status", demographics$item)
demographics$item <- gsub("^7. Status de emprego$", "7. Employment status", demographics$item)
demographics$item <- gsub("^7. Status de emprego $", "7. Employment status", demographics$item)
demographics$item <- gsub("^Employment status$", "7. Employment status", demographics$item)
demographics$item <- gsub("^8. Renda familiar anual bruta \\(dólares americanos\\)$", "8. Gross annual household income \\(US dollars\\)", demographics$item)
demographics$item <- gsub("^8. Renda familiar anual bruta \\(dólares americanos\\) $", "8. Gross annual household income \\(US dollars\\)", demographics$item)
demographics$item <- gsub("^Gross annual household income \\(US dollars\\)$", "8. Gross annual household income \\(US dollars\\)", demographics$item)

# Isolate PIN, item and response columns 
demographics1 <- select(demographics, PIN, item, response)

# Create new columns based on each unique variable under "item" 
demographics2 <- demographics %>%
  select(PIN, item, response) %>%
  pivot_wider(names_from = "item", 
              values_from = "response")

# Convert age values to numeric 
demographics2$`2. Age (years)` <- as.numeric(demographics2$`2. Age (years)`)

# Convert height values to numeric
demographics2$`3a. Height (cm)` <- as.numeric(demographics2$`3a. Height (cm)`)
demographics2$`3b. Height (feet)` <- as.numeric(demographics2$`3b. Height (feet)`)
demographics2$`3c. Height (inches)` <- as.numeric(demographics2$`3c. Height (inches)`)

# Replace na values with 0 
demographics2$`3a. Height (cm)` <- replace(demographics2$`3a. Height (cm)`, is.na(demographics2$`3a. Height (cm)`), 0)
demographics2$`3b. Height (feet)` <- replace(demographics2$`3b. Height (feet)`, is.na(demographics2$`3b. Height (feet)`), 0)
demographics2$`3c. Height (inches)` <- replace(demographics2$`3c. Height (inches)`, is.na(demographics2$`3c. Height (inches)`), 0)

# Make height entries where inches > 11 equal to NA, so they can be excluded from mean and sd 
demographics2$`3a. Height (cm)`[demographics2$`3c. Height (inches)` > 11] <- NA
demographics2$`3b. Height (feet)`[demographics2$`3c. Height (inches)` > 11] <- NA
demographics2$`3c. Height (inches)`[demographics2$`3c. Height (inches)` > 11] <- NA

# Replace "0" values in the height(cm) column with conversions from feet and inches, with addition
demographics2$`3a. Height (cm)` <- demographics2$`3a. Height (cm)` + (((demographics2$`3b. Height (feet)` * 12) + demographics2$`3c. Height (inches)`) * 2.54)

# Convert weight values to numeric
demographics2$`4a Weight(kg)` <- as.numeric(demographics2$`4a Weight(kg)`)
demographics2$`4b Weight (lbs)` <- as.numeric(demographics2$`4b Weight (lbs)`)

#Replace na values with 0
demographics2$`4a Weight(kg)` <- replace(demographics2$`4a Weight(kg)`, is.na(demographics2$`4a Weight(kg)`), 0)
demographics2$`4b Weight (lbs)` <- replace(demographics2$`4b Weight (lbs)`, is.na(demographics2$`4b Weight (lbs)`), 0)

# Replace "0" values in weight(kg) column with conversions from pound, with addition
demographics2$`4a Weight(kg)` <- demographics2$`4a Weight(kg)` + (demographics2$`4b Weight (lbs)` * 0.453592)

demographics3 <- demographics2 %>%
  select(-"English language", -"3b. Height (feet)", -"3c. Height (inches)", -"4b Weight (lbs)", -"6. Estado civil")

write.csv(demographics3, file = "C:/Users/james/Desktop/honours/demographics_df.csv", row.names = FALSE)
