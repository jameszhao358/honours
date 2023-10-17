# Import libraries 

library(tidyr)
library(dplyr)
library(factoextra)
library(dbscan)
library(ppclust)
library(e1071)
library(caret)
library(ranger)
library(MLeval)
library(pROC)

# Select significant variables 

features_temp <- read.csv("C:/Users/james/Desktop/honours/processed_data/extracted_features.csv")
demographics <- read.csv("C:/Users/james/Desktop/honours/processed_data/demographics_df.csv")

demographics$Education = factor(demographics$Education, levels = c("Primary", "Secondary", "Associate or vocational education", "Bachelor", "Masters or above"))
demographics$Employment = factor(demographics$Employment, levels = c("Unemployed", "Retired", "Student", "Self-employed", "Employed (part-time)", "Employed (full-time)"))

features_temp2 <- features_temp %>%
  select(PIN, overall_kht, overall_kht_sd, total_keypresses, transferq_kht, transferq_ft, VVR_duration, transfer_ft, behavioural_ft, non_behavioural_kht, PHQ_classification) %>%
  mutate(PHQ_classification = ifelse(PHQ_classification == "Subclinical", 0, 1)) %>%
  mutate(across(everything(), ~ replace_na(., median(., na.rm = TRUE)))) # Replacing all n/a values with the column median

demographics_features <- demographics %>%
  mutate(Education = as.numeric(Education)) %>%
  mutate(Marital = as.numeric(as.factor(Marital))) %>%
  mutate(Employment = as.numeric(Employment)) %>%
  select("PIN", "Height", "Weight", "Education", "Marital", "Employment") %>%
  mutate(across(everything(), ~ replace_na(., median(., na.rm = TRUE)))) # Replacing all n/a values with the column median

features <- left_join(features_temp2, demographics_features, by = "PIN") %>%
  select(-"PIN")

data <- features %>%
  select(-PHQ_classification)

data_scale <- as.data.frame(scale(data))

# Elbow plot to determine optimal number of clusters for k-means clustering
fviz_nbclust(data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# K-means 

km.out <- kmeans(data_scale, centers = 3, nstart = 100)
km.clusters <- km.out$cluster
print(km.clusters)

data <- data %>%
  mutate(kmeans = km.clusters)

# DBSCAN clustering 

# Finding optimal epsilon using elbow plot
kNNdistplot(data_scale, minPts = 7)

# DBScan 
dbscan_res <- dbscan(data_scale, eps = 2, minPts = 7)
dbscan_res$cluster
plot(data_scale, col = dbscan_res$cluster+1, main = "DBSCAN")

data <- data %>%
  mutate(dbscan = dbscan_res$cluster)

# Fuzzy clustering

fuzzy_clusters <- fcm(data_scale, centers = 3)

factoextra::fviz_cluster(list(data = data_scale, cluster = fuzzy_clusters$cluster),  
                         geom = "point", 
                         ellipse = FALSE, 
                         show.clust.cent = FALSE,
                         palette = "jco", 
                         ggtheme = theme_classic())

# Adding fuzzy cluster assignments to data
data <- data %>%
  mutate(fuzzy_cluster = fuzzy_clusters$cluster)

# PCA Biplot

pca <- prcomp(data_scale)
fviz_pca_biplot(pca,
                label="var",
                habillage = features$PHQ_classification)

# Scaling data and adding target variable

data_scale <- as.data.frame(scale(data))

data_scale <- data_scale %>%
  mutate(PHQ9 = features$PHQ_classification)

# Random Forest Model -----------------------------------------------------

set.seed(123)

train_index <- sample(1:nrow(data_scale), 0.8 * nrow(data_scale))
data_train <- data_scale[train_index, ]
data_test <- data_scale[-train_index, ]

param_grid <- expand.grid(
  mtry = c(2, 4, 6),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10, 15, 20)
)

ctrl <- trainControl(
  method = "repeatedcv",           
  number = 3,
  repeats = 3,
  sampling = "up",
  search = "grid",
  savePredictions = TRUE
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

best_rf <- ranger(
  formula = as.factor(PHQ9) ~ .,
  data = data_train,
  mtry = 4,
  splitrule = "extratrees",
  min.node.size = 15,
  probability = TRUE
)

ranger_predict <- predict(best_rf, data = data_test, type = "response")
predictions <- ranger_predict$predictions[, 2]
predicted_classes <- ifelse(predictions >= 0.5, 1, 0)

confusion_matrix <- confusionMatrix(as.factor(predicted_classes), true_labels)
print(confusion_matrix)

auc(roc(as.numeric(true_labels), predictions))
plot(roc(true_labels, predictions), 
     main = "Random Forest ROC Curve", 
     print.auc = TRUE)

# Rpart -------------------------------------------------------------------

param_grid <- expand.grid(
  cp = seq(0.01, 0.5, by = 0.01)  
)

ctrl <- trainControl(
  method = "repeatedcv",           
  number = 3,
  repeats = 3,
  sampling = "up",
  search = "grid",
  savePredictions = TRUE
)

set.seed(123)

rpart_model <- train(
  as.factor(PHQ9) ~ .,
  data = data_train,
  method = "rpart",        
  trControl = ctrl,
  tuneGrid = param_grid    
)

print(rpart_model)

best_rpart <- rpart_model$finalModel 
predictions <- predict(best_rpart, newdata = data_test)
predicted_classes <- ifelse(predictions[, 2] >= 0.5, 1, 0)

confusion_matrix <- confusionMatrix(as.factor(predicted_classes), true_labels)
print(confusion_matrix)

auc(roc(true_labels, predictions[, 2]))
# AUC = 0.78 

# KNN ---------------------------------------------------------------------

ctrl <- trainControl(
  method = "repeatedcv",          
  number = 3,
  repeats = 3,
  sampling = "up",
  search = "grid",
  savePredictions = TRUE
)

set.seed(123)

knn_model <- train(
  as.factor(PHQ9) ~ .,               
  data = data_train,      
  method = "knn",         
  trControl = ctrl,
  metric = "Accuracy",
  tuneGrid = data.frame(k = seq(11, 100, by = 2)),
)

print(knn_model)

knn_predictions <- predict(knn_model, newdata = data_test)

confusion_matrix <- confusionMatrix(knn_predictions, true_labels)
print(confusion_matrix)
auc(roc(true_labels, as.numeric(knn_predictions)))

# XGBoost -----------------------------------------------------------------

param_grid <- expand.grid(
  nrounds = 100,
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(2, 3, 5, 10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

ctrl <- trainControl(
  method = "repeatedcv",           
  number = 3,
  repeats = 3,
  sampling = "up",
  search = "grid",
  savePredictions = TRUE
)

xgb_model <- train(
  as.factor(PHQ9) ~ .,
  data = data_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = param_grid,
)

print(xgb_model)

xgb_predictions <- predict(xgb_model, newdata = data_test, type = "prob")
xgb_predicted_class <- ifelse(xgb_predictions[, 2] >= 0.5, 1, 0)

confusion_matrix <- confusionMatrix(as.factor(xgb_predicted_class), true_labels)
print(confusion_matrix)
auc(roc(true_labels, xgb_predictions[,2]))
plot(roc(true_labels, xgb_predictions[,2]), 
     main = "XGBoost ROC Curve", 
     print.auc = TRUE)
