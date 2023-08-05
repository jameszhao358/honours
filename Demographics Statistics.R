library(tidyr)
library(dplyr)

# Importing csv file into data frame named "demographics" 
demographics <- read.csv("C:/Users/James/Desktop/R Stuff/demographics.csv")

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

# Counting and Calculation ------------------------------------------------

# Counting gender 
gender_count <- table(demographics2$`1. Gender`)
print(gender_count)
# Female 99 (32%), Male 209 (67.6%), Other 1 (0.3%)

# Convert age values to numeric 
demographics2$`2. Age (years)` <- as.numeric(demographics2$`2. Age (years)`)

# Calculate age mean and sd 
mean_age <- mean(demographics2$`2. Age (years)`, na.rm = TRUE)
sd_age <- sd(demographics2$`2. Age (years)`)
# mean = 35.3, sd = 9.66

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

# Calculate height mean and sd 
mean_height <- mean(demographics2$`3a. Height (cm)`, na.rm = TRUE)
sd_height <- sd(demographics2$`3a. Height (cm)`, na.rm = TRUE)
# mean = 169.4, sd = 12 

# Convert weight values to numeric
demographics2$`4a Weight(kg)` <- as.numeric(demographics2$`4a Weight(kg)`)
demographics2$`4b Weight (lbs)` <- as.numeric(demographics2$`4b Weight (lbs)`)

#Replace na values with 0
demographics2$`4a Weight(kg)` <- replace(demographics2$`4a Weight(kg)`, is.na(demographics2$`4a Weight(kg)`), 0)
demographics2$`4b Weight (lbs)` <- replace(demographics2$`4b Weight (lbs)`, is.na(demographics2$`4b Weight (lbs)`), 0)

# Replace "0" values in weight(kg) column with conversions from pound, with addition
demographics2$`4a Weight(kg)` <- demographics2$`4a Weight(kg)` + (demographics2$`4b Weight (lbs)` * 0.453592)

# Calculate weight mean and sd
mean_weight <- mean(demographics2$`4a Weight(kg)`)
sd_weight <- sd(demographics2$`4a Weight(kg)`)
# mean = 69.5, sd = 15.88

# Calculate age mean and sd 
mean_age <- mean(demographics2$`2. Age (years)`)
sd_age <- sd(demographics2$`2. Age (years)`)
# mean = 35.3, sd = 9.66

# Counting education level
education_count <- table(demographics2$`5. Education level`)
print(education_count)

# Counting marital status 
marital_count <- table(demographics2$`6. Marital status`)
print(marital_count)

# Counting employment status 
employment_count <- table(demographics2$`7. Employment status`)
print(employment_count)

# Counting yearly income 
income_count <- table(demographics2$`8. Gross annual household income (US dollars)`)
print(income_count)

# Preparing Data for ML ---------------------------------------------------

library(caret)

demographics_encoded <- demographics2 %>%
  select(-"English language", -"3b. Height (feet)", -"3c. Height (inches)", -"4b Weight (lbs)")

dummy <- dummyVars(~ . - PIN, data = demographics_encoded)

demographics_encoded2 <- cbind(PIN = demographics_encoded$PIN,
                               data.frame(predict(dummy, newdata = demographics_encoded)))

demographics3 <- demographics2 %>%
  select(-"3b. Height (feet)", -"3c. Height (inches)", - "4b Weight (lbs)", -"English language") %>%
  mutate(`1. Gender` = ifelse(`1. Gender` == "Male", 0, ifelse(`1. Gender` == "Female", 1, 2))) %>%
  mutate(`5. Education level` = case_when(
    `5. Education level` == "Primary" ~ 0,
    `5. Education level` == "Secondary" ~ 1, 
    `5. Education level` == "Associate or vocational education" ~ 2,
    `5. Education level` == "Bachelor" ~ 3,
    `5. Education level` == "Masters or above" ~4
    )) %>%
  mutate(`6. Marital status` = case_when(
    `6. Marital status` == "Never married" ~ 0,
    `6. Marital status` == "Married" ~ 1, 
    `6. Marital status` == "Separated" ~ 2,
    `6. Marital status` == "Divorced" ~ 3,
    )) %>%
  mutate(`7. Employment status` = case_when(
    `7. Employment status` == "Student" ~ 0,
    `7. Employment status` == "Unemployed" ~ 1,
    `7. Employment status` == "Self-employed" ~ 2,
    `7. Employment status` == "Employed (part-time)" ~ 3,
    `7. Employment status` == "Employed (full-time)" ~ 4,
    `7. Employment status` == "Retired" ~ 5
    )) %>%
  mutate(`8. Gross annual household income (US dollars)` = case_when(
    `8. Gross annual household income (US dollars)` == "< $15,000" ~ 0,
    `8. Gross annual household income (US dollars)` == "$15,000 - $24,999" ~ 1,
    `8. Gross annual household income (US dollars)` == "$25,000 - $74,999" ~ 2,
    `8. Gross annual household income (US dollars)` == "> $75,000" ~ 3
  )) %>%
  left_join(PHQ9_df, by = "PIN")

# Comparison Investigations

gender_table <- table(demographics3$`1. Gender`, demographics3$PHQ_classification)
print(chisq.test(gender_table))
# p-value = 0.4887

education_table <- table(demographics3$`5. Education level`, demographics3$PHQ_classification)
print(chisq.test(education_table))
# p-value = 9.803e-09

marital_table <- table(demographics3$`6. Marital status`, demographics3$PHQ_classification)
print(chisq.test(marital_table))
# p-value = 1.923e-08

employment_table <- table(demographics3$`7. Employment status`, demographics3$PHQ_classification)
print(chisq.test(employment_table))
# p-value = 0.00625

income_table <- table(demographics3$`8. Gross annual household income (US dollars)`, demographics3$PHQ_classification)
print(chisq.test(income_table))
# p-value = 0.371

wilcox.test(`2. Age (years)` ~ PHQ_classification, data = demographics3)
# p-value = 0.7119

wilcox.test(`3a. Height (cm)` ~ PHQ_classification, data = demographics3)
# p-value = 0.0002776

wilcox.test(`4a Weight(kg)` ~ PHQ_classification, data = demographics3)
# p-value = 0.002592


# Building the Master Database --------------------------------------------

master <- demographics_encoded2 %>%
  select(-X.1..Gender.Female, -X.1..Gender.Male, -X.1..Gender.Other, -X.2..Age..years..) %>%
  left_join(non_behavioural, by = "PIN") %>%
  left_join(behaviouralft, by = "PIN") %>%
  left_join(transferft, by = "PIN") %>%
  left_join(VVRduration, by = "PIN") %>%
  left_join(transferq_khtft, by = "PIN") %>%
  left_join(overallkht, by = "PIN") %>%
  left_join(PHQ9_df, by = "PIN")

write.csv(master, file = "C:/Users/james/Desktop/R Stuff/master.csv", row.names = FALSE)

# Graphing ----------------------------------------------------------------

# Visualising gender counts

library(tidyverse)

gender_df <- as.data.frame(gender_count)
ggplot(gender_df, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity") +
  xlab("Gender") + ylab("Frequency") +
  labs(title = "Gender Count")

# Visualising age data

age_df <- data.frame("Age_years" = demographics2$`2. Age (years)`)

ggplot(age_df, aes(y = Age_years)) +
  geom_boxplot(fill = "yellow", alpha = 0.1) +
  ylab("Age (years)") +
  labs(title = "Age Distribution")

# Visualising height data 

height_df <- data.frame("Height_cm" = demographics2$`3a. Height (cm)`)

ggplot(height_df, aes(y = Height_cm)) +
  geom_boxplot(fill = "blue", alpha = 0.1) +
  ylab("Height (cm)") +
  labs(title = "Height Distribution")

# Visualising weight data 

weight_df <- data.frame("Weight_kg" = demographics2$`4a Weight(kg)`)

ggplot(weight_df, aes(y = Weight_kg)) +
  geom_boxplot(fill = "red", alpha = 0.1) +
  ylab("Weight (kg)") +
  labs(title = "Weight Distribution")

# Visualising education counts 

education_df <- as.data.frame(education_count) 
ggplot(education_df, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity") +
  xlab("Education level") + ylab("Frequency") +
  labs(title = "Education Level")

# Visualising marital status counts 

marital_df <- as.data.frame(marital_count) 
ggplot(marital_df, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity") +
  xlab("Marital status") + ylab("Frequency") +
  labs(title = "Marital status")

# Visualising employment counts 

employment_df <- as.data.frame(employment_count) 
ggplot(employment_df, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity") +
  xlab("Employment status") + ylab("Frequency") +
  labs(title = "Employment status")

# Visualising income counts

income_df <- as.data.frame(income_count) 

income_df$Var1 <- factor(income_df$Var1, levels = c("< $15,000", "$15,000 - $24,999", "$25,000 - $74,999", "> $75,000"))

ggplot(income_df, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity") +
  xlab("Annual income range") + ylab("Frequency") +
  labs(title = "Income range")









