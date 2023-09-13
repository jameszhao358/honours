# Import libraries 

library(tidyr)
library(dplyr)
library(factoextra)
library(dbscan)
library(ppclust)
library(e1071)
library(caret)
library(ranger)

# Select significant variables 

features_temp <- read.csv("C:/Users/james/Desktop/honours/processed_data/extracted_features.csv")

features <- features_temp %>%
  select(overall_kht, transferq_kht, transferq_ft, VVR_duration, transfer_ft, behavioural_ft, non_behavioural_kht, PHQ_classification) %>%
  mutate(PHQ_classification = ifelse(PHQ_classification == "Subclinical", 0, 1)) %>%
  mutate(across(everything(), ~ replace_na(., median(., na.rm = TRUE)))) # Replacing all n/a values with the column median

data <- features %>%
  select(-PHQ_classification)

data_scale <- as.data.frame(scale(data))

data_scale <- data_scale %>%
  mutate(PHQ9 = features$PHQ_classification)

data <- dist(data_scale)

# Within sum squares 
fviz_nbclust(data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# K-means 

km.out <- kmeans(data_scale, centers = 3, nstart = 100)
print(km.out)

km.clusters <- km.out$cluster
rownames(data_scale) <- paste(features$PHQ_classification, 1:dim(features)[1], sep = "_")
fviz_cluster(list(data=data_scale, cluster = km.clusters))
table(km.clusters, features$PHQ_classification)

# DBSCAN clustering 

# Finding optimal epsilon using elbow plot
kNNdistplot(data_scale, minPts = 7)

# DBScan 
dbscan_res <- dbscan(data_scale, eps = 2, minPts = 7)
dbscan_res
plot(data_scale, col = dbscan_res$cluster+1, main = "DBSCAN")

# Fuzzy clustering

fuzzy_clusters <- fcm(data_scale, centers = 3)

factoextra::fviz_cluster(list(data = data_scale, cluster = fuzzy_clusters$cluster),  
                         geom = "point", 
                         ellipse = FALSE, 
                         show.clust.cent = FALSE,
                         palette = "jco", 
                         ggtheme = theme_classic())

# Adding cluster assignments to data_scale
data_scale <- data_scale %>%
  mutate(fuzzy_cluster = fuzzy_clusters$cluster)

# PCA Biplot

pca <- prcomp(data_scale)
fviz_pca_biplot(pca,
                label="var",
                habillage = features$PHQ_classification)

# Random Forest Model -----------------------------------------------------

set.seed(123)

train_index <- sample(1:nrow(data_scale), 0.9 * nrow(data_scale))
data_train <- data_scale[train_index, ]
data_test <- data_scale[-train_index, ]

param_grid <- expand.grid(
  mtry = c(2, 4, 6),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10, 15, 20)
)

ctrl <- trainControl(
  method = "cv",           
  number = 10,              
  search = "grid"
)
  
set.seed(123)
rf_model <- train(
  as.factor(PHQ9) ~ .,
  data = data_train,
  method = "ranger",
  trControl = ctrl,         
  tuneGrid = param_grid
)

print(rf_model)
# The final values used for the model were mtry = 2, splitrule = extratrees and min.node.size = 20.
# Best accuracy before cluster assignment = 0.732 (when min.node.size = 15)
# Best accuracy after cluster assignment = 0.721

best_rf <- ranger(
  formula = as.factor(PHQ9) ~ .,
  data = data_train,
  mtry = 2,
  splitrule = "extratrees",
  min.node.size = 20
)

ranger_predict <- predict(best_rf, data = data_test)
predictions <- ranger_predict$predictions
true_labels = data_test$PHQ

confusion_matrix <- table(True = true_labels, Predicted = predictions)
print(confusion_matrix)

accuracy <- (confusion_matrix[1, 1] + confusion_matrix[2, 2]) / sum(confusion_matrix)
# Accuracy: 0.7419355
# Accuracy after cluster assignment: 0.7097

precision <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1])
# Precision: 0.6428571
# Precision after cluster assignment: 0.615

recall <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[1, 2])
# Recall (Sensitivity): 0.75 
# Recall after cluster assignment: 0.667

f1_score <- 2 * (precision * recall) / (precision + recall)
# F1-Score: 0.6923077 
# F1-score after cluster assignment: 0.64









