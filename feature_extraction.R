# Import packages

library(zoo)
library(dplyr)
library(tidyr)

# Read processed data files 

complete_df_2 <- read.csv("C:/Users/james/Desktop/honours_largefiles/complete_df.csv")
PHQ9_df <- read.csv("C:/Users/james/Desktop/honours/processed_data/PHQ9_df.csv")

# Overall KHT and FT ------------------------------------------------------

overall_df <- complete_df_2 %>%
  filter(grepl("press|release", event_type) & !grepl('mouse', event_converted)) %>%
  mutate(
    key_hold_time = ifelse(event_type == "key release", timestamp - lag(timestamp), NA),
    flight_time = ifelse(event_type == "key press", timestamp - lag(timestamp, n = 2), NA)
  )

# Start of stack containing all extracted features
overall_summary <- overall_df %>%
  group_by(PIN) %>%
  summarize(overall_kht = median(key_hold_time, na.rm = TRUE),
            overall_ft = median(flight_time, na.rm = TRUE))

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
  # Adding transferq to the stack
  left_join(overall_summary, by = "PIN")

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
  # Adding VVR to the stack
  left_join(transferq_summary, by = "PIN")

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

# Saving left/right KHT data as .csv for analysis of change between stages (in inferential_statistics.R)
write.csv(LR_summary_kht, file = "C:/Users/james/Desktop/honours/processed_data/LR_summary_kht.csv", row.names = FALSE)

LR_summary_kht2 <- LR_summary_kht %>%
  select(PIN, behavioural_kht) %>%
  # Adding LR KHT to the stack 
  left_join(VVR_summary, by = "PIN")

LR_summary_ft <- LR_summary %>%
  select(PIN, stage, median_ft) %>%
  pivot_wider(., names_from = stage, values_from = median_ft) %>%
  mutate(
    ft_change = deval_test - VVR1,
    behavioural_ft = rowMeans(select(., VVR1, pav_con, transfer1, deval_test), na.rm = TRUE)
  ) %>%
  left_join(PHQ9_df, by = "PIN")

write.csv(LR_summary_ft, file = "C:/Users/james/Desktop/honours/processed_data/LR_summary_ft.csv", row.names = FALSE)

LR_summary_ft2 <- LR_summary_ft %>%
  select(PIN, behavioural_ft) %>%
  # Adding LR FT to the stack
  left_join(LR_summary_kht2, by = "PIN")

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
  select(PIN, non_behavioural_kht) %>%
  # Adding non-behavioural KHT to the stack
  left_join(LR_summary_ft2, by = "PIN")

# Transfer Devalue vs Value  ---------------------------------------------------------------

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

write.csv(transfer_summary_kht, file = "C:/Users/james/Desktop/honours/processed_data/transfer_summary_kht.csv", row.names = FALSE)

transfer_summary_kht2 <- transfer_summary_kht %>%
  select(PIN, transfer_kht) %>%
  # Adding transfer KHT to the stack
  left_join(non_LR_summary_kht, by = "PIN")

n_distinct(transfer_summary_kht$PIN) #308 participants transfer completers with PHQ diagnosis 

transfer_summary_ft <- transfer_summary %>%
  select(PIN, snack_status2, median_ft) %>%
  pivot_wider(., names_from = snack_status2, values_from = median_ft) %>%
  mutate(transfer_ft = rowMeans(across(-1), na.rm = TRUE)) %>%
  left_join(PHQ9_df, by = "PIN") %>%
  filter(!is.na(PHQ_classification))

transferft <- transfer_summary_ft %>%
  select(PIN, transfer_ft)

write.csv(transfer_summary_ft, file = "C:/Users/james/Desktop/honours/processed_data/transfer_summary_ft.csv", row.names = FALSE)

extracted_features <- transfer_summary_ft %>%
  select(PIN, transfer_ft) %>%
  left_join(transfer_summary_kht2, by = "PIN") %>%
  left_join(PHQ9_df, by = "PIN")

write.csv(extracted_features, file = "C:/Users/james/Desktop/honours/processed_data/extracted_features.csv", row.names = FALSE)