# Import Libraries --------------------------------------------------------

library(dplyr)
library(tidyr)
library("ggpubr")

# Import complete.csv

complete_df <- read.csv("C:/Users/james/Desktop/R Stuff/complete.csv")

n_distinct(complete_df$PIN) #531 distinct pins 
  
# Filtering complete participants + changing RL1 to VVR1 

complete_df_2 <- complete_df %>%
  filter(complete == "y") %>%
  mutate(stage = ifelse(stage == "RL1", "VVR1", stage))

# Counting VVR1 completers 

distinct_VVR1 <- complete_df_2 %>%
  filter(stage == "VVR1") %>%
  summarise(distinct_pins = n_distinct(PIN)) #320 patients completed + did VVR1 stage

# PHQ9 --------------------------------------------------------------------

PHQ9 <- read.csv("C:/Users/James/Desktop/R Stuff/PHQ-9.csv")

n_distinct(PHQ9$PIN) #308 distinct PINS  

PHQ9_df <- PHQ9 %>%
  select(PIN, item, response) %>%
  pivot_wider(names_from = "item", 
              values_from = "response") %>%
  mutate_at(vars(2:10), ~as.numeric(.)) %>%
  mutate(total_score = rowSums(select(., 2:10))) %>%
  mutate(PHQ_classification = ifelse(total_score < 10, "Subclinical", "Major Depression")) %>%
  select(PIN, PHQ_classification)

# Overall KHT and FT ------------------------------------------------------

overall_df <- complete_df_2 %>%
  filter(grepl("press|release", event_type) & !grepl('mouse', event_converted)) %>%
  mutate(
    key_hold_time = ifelse(event_type == "key release", timestamp - lag(timestamp), NA),
    flight_time = ifelse(event_type == "key press", timestamp - lag(timestamp, n = 2), NA)
  )
    
overall_summary <- overall_df %>%
  group_by(PIN) %>%
  summarize(overall_kht = median(key_hold_time, na.rm = TRUE),
            overall_ft = median(flight_time, na.rm = TRUE)) %>%
  left_join(PHQ9_df, by = "PIN")

overallkht <- overall_summary %>%
  select(PIN, overall_kht)

wilcox.test(median_kht ~ PHQ_classification, data = overall_summary)
# p-value = 0.003056

wilcox.test(median_ft ~ PHQ_classification, data = overall_summary)
# p-value = 0.1653

# Transfer Q KHT and FT ---------------------------------------------------

transferq_df <- complete_df_2 %>%
  filter(stage == "transfer_q" & grepl("press|release", event_type) & !grepl('mouse', event_converted)) %>%
  mutate(
    key_hold_time = ifelse(event_type == "key release", timestamp - lag(timestamp), NA),
    flight_time = ifelse(event_type == "key press", timestamp - lag(timestamp, n = 2), NA),
    previous_key = lag(event_converted)
  ) %>%
  mutate(
    flight_time = ifelse(event_converted == "spacebar key pressed" | previous_key == "spacebar key pressed", NA, flight_time)
  ) %>%
  mutate(
    flight_time = ifelse(PIN != lag(PIN), NA, flight_time)
  ) 

transferq_summary <- transferq_df %>%
  group_by(PIN) %>%
  summarize(transferq_kht = median(key_hold_time, na.rm = TRUE),
            transferq_ft = median(flight_time, na.rm = TRUE)) %>%
  left_join(PHQ9_df, by = "PIN")

transferq_khtft <- transferq_summary %>%
  select(PIN, transferq_ft, transferq_kht)

wilcox.test(median_kht ~ PHQ_classification, data = transferq_summary)
# p-value = 4.985e-06

wilcox.test(median_ft ~ PHQ_classification, data = transferq_summary)
# p-value = 3.075e-10

# VVR1 KHT, FT and Duration ------------------------------------------------------

VVR_df <- complete_df_2 %>%
  filter(stage == "VVR1" & grepl("leftarrow|rightarrow", event_converted)) %>% 
  mutate(key_hold_time = ifelse(event_type == "key release", timestamp - lag(timestamp), NA),
         flight_time = ifelse(event_type == "key press", timestamp - lag(timestamp, n = 2), NA))

VVR_blocks <- complete_df_2 %>%
  filter(stage == "VVR1") %>%
  group_by(PIN) %>%
  summarize(blocks = sum(event_raw == "Submit"))

VVR_summary <- VVR_df %>%
  group_by(PIN) %>%
  summarize(VVR_duration = max(timestamp) - min(timestamp),
            VVR_median_kht = median(key_hold_time, na.rm = TRUE),
            VVR_median_ft = median(flight_time, na.rm = TRUE)) %>%
  left_join(PHQ9_df, by = "PIN")

VVRduration <- VVR_summary %>%
  select(PIN, VVR_duration)

cor(VVR_summary$duration, VVR_summary$VVR_median_kht)
# Correlation coefficient = -0.1286716

cor(VVR_summary$duration, VVR_summary$VVR_median_ft)
# Correlation coefficient = -0.1674709

wilcox.test(duration ~ PHQ_classification, data=VVR_summary)
# p-value = 1.429e-07

wilcox.test(VVR_median_kht ~ PHQ_classification, data=VVR_summary)
# p-value = 0.2511

wilcox.test(VVR_median_ft ~ PHQ_classification, data=VVR_summary)
# p-value = 0.01564

# Left / right arrow (behavioural) KHT and FT across stages ------------------------------------------------

LR_df <- complete_df_2 %>%
  filter(grepl("leftarrow|rightarrow", event_converted)) %>%
  group_by(PIN, stage) %>%
  mutate(
    key_hold_time = ifelse(event_type == "key release", timestamp - lag(timestamp), NA),
    flight_time = ifelse(event_type == "key press", timestamp - lag(timestamp, n = 2), NA))

LR_summary <- LR_df %>%
  filter(stage %in% c("VVR1", "pav_con", "transfer1", "deval_test")) %>%
  group_by(PIN, stage) %>%
  summarize(
    median_kht = median(key_hold_time, na.rm = TRUE),
    median_ft = median(flight_time, na.rm = TRUE),
    min_timestamp = min(timestamp)
  ) %>%
  ungroup() %>%
  arrange(PIN, min_timestamp)

LR_summary_kht <- LR_summary %>%
  select(PIN, stage, median_kht) %>%
  pivot_wider(., names_from = stage, values_from = median_kht) %>%
  mutate(
    kht_change = deval_test - VVR1,
    behavioural_kht = rowMeans(select(., VVR1, pav_con, transfer1, deval_test), na.rm = TRUE)
  ) %>%
  left_join(PHQ9_df, by = "PIN") 

wilcox.test(behavioural_kht ~ PHQ_classification, data = LR_summary_kht)
# p-value = 0.1103

t.test(LR_summary_kht$behavioural_kht ~ LR_summary_kht$PHQ_classification)
# p-value = 0.4044

LR_summary_kht %>%
  filter(PHQ_classification == "Major Depression") %>%
  summarise(median_kht_change = median(kht_change, na.rm = TRUE))
# Median KHT change is -2 

LR_summary_kht %>%
  filter(PHQ_classification == "Subclinical") %>%
  summarise(median_kht_change = median(kht_change, na.rm = TRUE))
# Median KHT change is -1 

LR_summary_ft <- LR_summary %>%
  select(PIN, stage, median_ft) %>%
  pivot_wider(., names_from = stage, values_from = median_ft) %>%
  mutate(
    ft_change = deval_test - VVR1,
    behavioural_ft = rowMeans(select(., VVR1, pav_con, transfer1, deval_test), na.rm = TRUE)
  ) %>%
  left_join(PHQ9_df, by = "PIN")

behaviouralft <- LR_summary_ft %>%
  select(PIN, behavioural_ft)

LR_summary_ft %>%
  filter(PHQ_classification == "Major Depression") %>%
  summarise(median_ft_change = median(ft_change, na.rm = TRUE))
# Median FT change is -24.8

LR_summary_ft %>%
  filter(PHQ_classification == "Subclinical") %>%
  summarise(median_ft_change = median(ft_change, na.rm = TRUE))
# Median FT change is -39.5

t.test(LR_summary_ft$VVR1, LR_summary_ft$deval_test)
# p-value = 0.02518

wilcox.test(behavioural_ft ~ PHQ_classification, data = LR_summary_ft)
# p-value = 0.0001606

cor(LR_summary_kht$behavioural_kht, LR_summary_ft$behavioural_ft)
# Correlation coefficient = 0.26

# Non left/right arrow (non-behavioural) KHT and FT -----------------------

non_LR_df <- complete_df_2 %>%
  filter(!grepl("leftarrow|rightarrow|left_mouse|right_mouse", event_converted) & grepl("pressed|released", event_converted) & stage != "transfer_q") %>%
  group_by(PIN, stage) %>%
  mutate(
    key_hold_time = ifelse(event_type == "key release", timestamp - lag(timestamp), NA),
    flight_time = ifelse(event_type == "key press", timestamp - lag(timestamp, n = 2), NA))

non_LR_summary <- non_LR_df %>%
  group_by(PIN, stage) %>%
  summarize(
    median_kht = median(key_hold_time, na.rm = TRUE),
    median_ft = median(flight_time, na.rm = TRUE),
    min_timestamp = min(timestamp)
  ) %>%
  ungroup() %>%
  arrange(PIN, min_timestamp)

non_LR_summary_kht <- non_LR_summary %>%
  select(PIN, stage, median_kht) %>%
  pivot_wider(., names_from = stage, values_from = median_kht) %>%
  mutate(
    non_behavioural_kht = rowMeans(select(., -PIN), na.rm = TRUE)
  ) %>%
  left_join(PHQ9_df, by = "PIN") %>%
  select(PIN, non_behavioural_kht, PHQ_classification)

non_behavioural <- non_LR_summary_kht %>%
  select(-PHQ_classification)

wilcox.test(non_behavioural_kht ~ PHQ_classification, data = non_LR_summary_kht)
# p-value = 0.002281

t.test(LR_summary_kht$behavioural_kht, non_LR_summary_kht$non_behavioural_kht)
# p-value = 0.05019

# Transfer  ---------------------------------------------------------------

library(zoo)

transfer_df <- read.csv("C:/Users/James/Desktop/R Stuff/transfer_dataset.csv")

n_distinct(transfer_df$PIN)

transfer_df <- transfer_df %>%
  mutate(snack_status2 = if_else(snack_status != "", snack_status, NA_character_)) %>%
  mutate(snack_status2 = na.locf(snack_status2, na.rm = FALSE)) %>%
  filter(grepl("leftarrow|rightarrow", event_converted)) %>%
  group_by(PIN) %>%
  mutate(key_hold_time = ifelse(event_type == "key release", timestamp - lag(timestamp), NA),
         flight_time = ifelse(event_type == "key press", timestamp - lag(timestamp, n = 2), NA))

transfer_summary <- transfer_df %>%
  group_by(PIN, snack_status2) %>%
  summarize(median_kht = median(key_hold_time, na.rm = TRUE),
            median_ft = median(flight_time, na.rm = TRUE))

transfer_summary_kht <- transfer_summary %>%
  select(PIN, snack_status2, median_kht) %>%
  pivot_wider(., names_from = snack_status2, values_from = median_kht) %>%
  mutate(transfer_kht = rowMeans(across(-1), na.rm = TRUE)) %>%
  left_join(PHQ9_df, by = "PIN") %>%
  filter(!is.na(PHQ_classification))

n_distinct(transfer_summary_kht$PIN) #308 participants transfer completers with PHQ diagnosis 

median(transfer_summary_kht$CS, na.rm = TRUE)
median(transfer_summary_kht$plus, na.rm = TRUE)  
median(transfer_summary_kht$minus, na.rm = TRUE)
median(transfer_summary_kht$blank, na.rm = TRUE)
median(transfer_summary_kht$devalued, na.rm = TRUE)
# Median KHT ranging from 126 to 128 ms

mean(transfer_summary_kht$CS, na.rm = TRUE)
mean(transfer_summary_kht$plus, na.rm = TRUE)  
mean(transfer_summary_kht$minus, na.rm = TRUE)
mean(transfer_summary_kht$blank, na.rm = TRUE)
mean(transfer_summary_kht$devalued, na.rm = TRUE)
# Mean KHT ranging from 135 to 139 ms 

# Mean and median values very similar for all vending machine statuses 

transfer_summary_ft <- transfer_summary %>%
  select(PIN, snack_status2, median_ft) %>%
  pivot_wider(., names_from = snack_status2, values_from = median_ft) %>%
  mutate(transfer_ft = rowMeans(across(-1), na.rm = TRUE)) %>%
  left_join(PHQ9_df, by = "PIN") %>%
  filter(!is.na(PHQ_classification))

transferft <- transfer_summary_ft %>%
  select(PIN, transfer_ft)

median(transfer_summary_ft$CS, na.rm = TRUE)
median(transfer_summary_ft$plus, na.rm = TRUE)  
median(transfer_summary_ft$minus, na.rm = TRUE)
median(transfer_summary_ft$blank, na.rm = TRUE)
median(transfer_summary_ft$devalued, na.rm = TRUE)
# Median FT ranging from 321 to 337 

mean(transfer_summary_ft$CS, na.rm = TRUE)
mean(transfer_summary_ft$plus, na.rm = TRUE)  
mean(transfer_summary_ft$minus, na.rm = TRUE)
mean(transfer_summary_ft$blank, na.rm = TRUE)
mean(transfer_summary_ft$devalued, na.rm = TRUE)
# Mean FT ranging from 946 (devalued) to 1481 (minus) 

t.test(transfer_summary_ft$devalued, transfer_summary_ft$minus)
# p-value = 0.1366

wilcox.test(transfer_kht ~ PHQ_classification, data = transfer_summary_kht)
# p-value = 0.3098

wilcox.test(transfer_ft ~ PHQ_classification, data = transfer_summary_ft)
# p-value = 0.008758

# Testing

