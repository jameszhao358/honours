library(dplyr)
library(tidyverse)

# PHQ9 --------------------------------------------------------------------

PHQ9 <- read.csv("C:/Users/James/Desktop/R Stuff/PHQ-9.csv")
PHQ9_1 <- select(PHQ9, PIN, item, response)
PHQ9_2 <- PHQ9_1 %>%
  pivot_wider(names_from = "item", 
              values_from = "response")

# Convert all values to numeric so they can be added
PHQ9_2 <- PHQ9_2 %>%
  mutate_at(vars(2:10), ~as.numeric(.))

# Compute sums 
PHQ9_2 <- PHQ9_2 %>%
  mutate(total_score = rowSums(select(., 2:10)))

# Categorise scores 
PHQ_9_subclinical <- sum(PHQ9_2$total_score < 10)
major_depression <- sum(PHQ9_2$total_score >= 10)

# Visualise with bar graph 
PHQ9_df <- data.frame(
  category = c("Subclinical", "Major depression"),
  frequency = c(132, 176)
)

ggplot(PHQ9_df, aes(x = category, y = frequency)) + 
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Frequency") +
  labs(title = "PHQ9 Questionnaire Results")

# GAD-7 -------------------------------------------------------------------

GAD7 <- read.csv("C:/Users/James/Desktop/R Stuff/GAD-7.csv")
GAD7_1 <- select(GAD7, PIN, item, response)
GAD7_2 <- GAD7_1 %>%
  pivot_wider(names_from = "item", 
              values_from = "response")

# Convert all values to numeric so they can be added
GAD7_2 <- GAD7_2 %>%
  mutate_at(vars(2:8), ~as.numeric(.))

# Compute sums 
GAD7_2 <- GAD7_2 %>%
  mutate(total_score1 = rowSums(select(., 2:8)))

# Categorise scores 
GAD7_subclinical <- sum(GAD7_2$total_score1 < 10) #160
GAD7_GAD <- sum(GAD7_2$total_score1 >= 10) #148

# Visualise with bar graph 

GAD7_df <- data.frame(
  category = c("Subclinical", "GAD"),
  frequency = c(160, 148)
)

ggplot(GAD7_df, aes(x = category, y = frequency)) + 
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Frequency") +
  labs(title = "GAD7 Questionnaire Results")


# DASS-21 -----------------------------------------------------------------

DASS21 <- read.csv("C:/Users/James/Desktop/R Stuff/DASS.csv")
DASS21_1 <- select(DASS21, PIN, item, response)
DASS21_2 <- DASS21_1 %>%
  pivot_wider(names_from = "item", 
              values_from = "response")

DASS21_2 <- DASS21_2 %>%
  mutate_at(vars(2:22), ~as.numeric(.))

DASS21_2 <- DASS21_2 %>%
  mutate(stress_score = rowSums(select(., 2, 7, 9, 12, 13, 15, 19))) %>%
  mutate(anxiety_score = rowSums(select(., 3, 5, 8, 10, 16, 20, 21))) %>%
  mutate(depression_score = rowSums(select(., 4, 6, 11, 14, 17, 18, 22)))

# Visualise DASS21 Stress

DASS21_stress_normal <- sum(DASS21_2$stress_score <=7)
DASS21_stress_mild <- sum(DASS21_2$stress_score >=8 & DASS21_2$stress_score <= 9)
DASS21_stress_moderate <- sum(DASS21_2$stress_score >= 10 & DASS21_2$stress_score <= 12)
DASS21_stress_severe <- sum(DASS21_2$stress_score >= 13 & DASS21_2$stress_score <= 16)
DASS21_stress_extremelysevere <- sum(DASS21_2$stress_score >= 17)

DASS21_stress_df <- data.frame(
  category = c("Normal", "Mild", "Moderate", "Severe", "Extremely Severe"),
  frequency = c(DASS21_stress_normal, DASS21_stress_mild, DASS21_stress_moderate, DASS21_stress_severe, DASS21_stress_extremelysevere)
)

# Without this step, the order of categories in the bar graph x axis is nonsensical 
DASS21_stress_df$category <- factor(DASS21_stress_df$category, levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely Severe"))

ggplot(DASS21_stress_df, aes(x = category, y = frequency)) + 
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Frequency") +
  labs(title = "DASS 21: Stress")


