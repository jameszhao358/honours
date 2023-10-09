# Import libraries

library(tidyr)
library(dplyr)

# Read processed demographic data

demographics3 <- read.csv("C:/Users/james/Desktop/honours/processed_data/demographics_df.csv")

# Descriptive Statistics ------------------------------------------------

# Counting gender 
gender_count <- table(demographics3$Gender)
print(gender_count)
# Female 99 (32%), Male 209 (67.6%), Other 1 (0.3%)

# Calculate age mean and sd 
mean_age <- mean(demographics2$Age, na.rm = TRUE)
sd_age <- sd(demographics2$Age)
# mean = 35.3, sd = 9.66

# Calculate height mean and sd 
mean_height <- mean(demographics2$Height, na.rm = TRUE)
sd_height <- sd(demographics2$Height, na.rm = TRUE)
# mean = 169.4, sd = 12 

# Calculate weight mean and sd
mean_weight <- mean(demographics2$Weight)
sd_weight <- sd(demographics2$Weight)
# mean = 69.5, sd = 15.88

# Calculate age mean and sd 
mean_age <- mean(demographics2$Age)
sd_age <- sd(demographics2$Age)
# mean = 35.3, sd = 9.66

# Counting education level
education_count <- table(demographics2$Education)
print(education_count)

# Counting marital status 
marital_count <- table(demographics2$Marital)
print(marital_count)

# Counting employment status 
employment_count <- table(demographics2$Employment)
print(employment_count)

# Counting yearly income 
income_count <- table(demographics2$Income)
print(income_count)

# Inferential Statistics --------------------------------------------------

gender_table <- table(demographics3$Gender, demographics3$PHQ_classification)
print(chisq.test(gender_table))
# p-value = 0.4887

education_table <- table(demographics3$Education, demographics3$PHQ_classification)
print(chisq.test(education_table))
# p-value = 9.803e-09

marital_table <- table(demographics3$Marital, demographics3$PHQ_classification)
print(chisq.test(marital_table))
# p-value = 1.923e-08

employment_table <- table(demographics3$Employment, demographics3$PHQ_classification)
print(chisq.test(employment_table))
# p-value = 0.00625

income_table <- table(demographics3$Income, demographics3$PHQ_classification)
print(chisq.test(income_table))
# p-value = 0.371

wilcox.test(Age ~ PHQ_classification, data = demographics3)
# p-value = 0.7119

wilcox.test(Height ~ PHQ_classification, data = demographics3)
# p-value = 0.0002776

wilcox.test(Weight ~ PHQ_classification, data = demographics3)
# p-value = 0.002592

# Visualisation ----------------------------------------------------------------

# Visualising gender counts

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









