# Import libraries

library(dplyr)
library(tidyr)

# Import extracted features

features <- read.csv("C:/Users/james/Desktop/honours/processed_data/extracted_features.csv")

# Overall summary table 

custom_summary <- function(data, feature, PHQ) {
  result_table <- data %>%
    group_by({{ PHQ }}) %>%
    summarise(
      median = median({{ feature }}, na.rm = TRUE),
      sd = sd({{ feature }}, na.rm = TRUE)
    )
  return(as.data.frame(result_table))
}

custom_summary(features, transfer_ft, PHQ_classification)
custom_summary(features, transfer_kht, PHQ_classification)
custom_summary(features, non_behavioural_kht, PHQ_classification)
custom_summary(features, VVR_duration, PHQ_classification)
custom_summary(features, behavioural_kht, PHQ_classification)
custom_summary(features, behavioural_ft, PHQ_classification)
custom_summary(features, VVR_median_kht, PHQ_classification)
custom_summary(features, VVR_median_ft, PHQ_classification)  
custom_summary(features, transferq_ft, PHQ_classification)
custom_summary(features, transferq_kht, PHQ_classification)
custom_summary(features, total_keypresses, PHQ_classification)
custom_summary(features, overall_kht, PHQ_classification)
custom_summary(features, overall_kht_sd, PHQ_classification)
custom_summary(features, overall_ft, PHQ_classification)
custom_summary(features, overall_ft_sd, PHQ_classification)
custom_summary(features, total_keypresses, PHQ_classification)

# Overall KHT, FT, variability and SD ------------------------------------------------------

wilcox.test(overall_kht ~ PHQ_classification, data = features)
# p-value = 0.003328

wilcox.test(overall_ft ~ PHQ_classification, data = features)
# p-value = 0.1112

wilcox.test(overall_kht_sd ~ PHQ_classification, data = features)
# p-value = 0.007223

wilcox.test(overall_ft_sd ~ PHQ_classification, data = features)
# p-value = 0.3521

wilcox.test(total_keypresses ~ PHQ_classification, data = features)
# p-value = 0.0005029

# Transfer_q KHT and FT ---------------------------------------------------

wilcox.test(transferq_kht ~ PHQ_classification, data = features)
# p-value = 4.985e-06

wilcox.test(transferq_ft ~ PHQ_classification, data = features)
# p-value = 3.343e-10

# VVR KHT, FT and Duration ------------------------------------------------

cor(features$VVR_duration, features$VVR_median_kht)
# Correlation coefficient = -0.126261

cor(features$VVR_duration, features$VVR_median_ft)
# Correlation coefficient = -0.1624507

wilcox.test(VVR_duration ~ PHQ_classification, data=features)
# p-value = 1.429e-07

wilcox.test(VVR_median_kht ~ PHQ_classification, data=features)
# p-value = 0.2511

wilcox.test(VVR_median_ft ~ PHQ_classification, data=features)
# p-value = 0.01496

# Behavioural KHT Change---------------------------------------------------------

LR_summary_kht <- read.csv("C:/Users/james/Desktop/honours/processed_data/LR_summary_kht.csv")

LR_summary_kht %>%
  filter(PHQ_classification == "Major Depression") %>%
  summarise(median_kht_change = median(kht_change, na.rm = TRUE))
# Median KHT change is -2 

LR_summary_kht %>%
  filter(PHQ_classification == "Subclinical") %>%
  summarise(median_kht_change = median(kht_change, na.rm = TRUE))
# Median KHT change is -1 

wilcox.test(kht_change ~ PHQ_classification, data = LR_summary_kht)
# p-value = 0.3236

wilcox.test(behavioural_kht ~ PHQ_classification, data = features)
# p-value = 0.1103

# Behavioural FT Change----------------------------------------------------------

LR_summary_ft <- read.csv("C:/Users/james/Desktop/honours/processed_data/LR_summary_ft.csv")

LR_summary_ft %>%
  filter(PHQ_classification == "Major Depression") %>%
  summarise(median_ft_change = median(ft_change, na.rm = TRUE))
# Median FT change is -24.75

LR_summary_ft %>%
  filter(PHQ_classification == "Subclinical") %>%
  summarise(median_ft_change = median(ft_change, na.rm = TRUE))
# Median FT change is -39.5

wilcox.test(ft_change ~ PHQ_classification, data = LR_summary_ft)
# p-value = 0.9857

t.test(LR_summary_ft$VVR1, LR_summary_ft$deval_test)
# p-value = 0.02518

wilcox.test(behavioural_ft ~ PHQ_classification, data = features)
# p-value = 0.0001606

wilcox.test(behavioural_kht ~ PHQ_classification, data = features)
# p-value = 0.1103

cor(features$behavioural_kht, features$behavioural_ft)
# Correlation coefficient = 0.25

# Non-behavioural KHT -----------------------------------------------------

wilcox.test(non_behavioural_kht ~ PHQ_classification, data = features)
# p-value = 0.002281

# Comparing behavioural vs non-behavioural KHT
t.test(features$behavioural_kht, features$non_behavioural_kht)
# p-value = 0.03111

# Transfer KHT Change -----------------------------------------------------

transfer_summary_kht <- read.csv("C:/Users/james/Desktop/honours/processed_data/transfer_summary_kht.csv")

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

# Mean and median KHT values very similar for all vending machine statuses

# Transfer FT Change ------------------------------------------------------

transfer_summary_ft <- read.csv("C:/Users/james/Desktop/honours/processed_data/transfer_summary_ft.csv")

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

# Is there a significant difference between the stages with the largest and smallest ft
t.test(transfer_summary_ft$devalued, transfer_summary_ft$minus)
# p-value = 0.1366

# Transfer KHT and FT vs PHQ ----------------------------------------------

wilcox.test(transfer_kht ~ PHQ_classification, data = features)
# p-value = 0.3098

wilcox.test(transfer_ft ~ PHQ_classification, data = features)
# p-value = 0.008758

