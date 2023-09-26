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

# Adding cluster assignments to data_scale
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

# The final values used for the model were mtry = 6, splitrule = gini and min.node.size = 20.
# Best accuracy before cluster assignment = 0.732 (when min.node.size = 15)
# Best accuracy after cluster assignment = 0.721
# Best accuracy when scaling after assigning clusters = ~0.71

best_rf <- ranger(
  formula = as.factor(PHQ9) ~ .,
  data = data_train,
  mtry = 6,
  splitrule = "gini",
  min.node.size = 20
)

ranger_predict <- predict(best_rf, data = data_test)
predictions <- ranger_predict$predictions
true_labels = as.factor(data_test$PHQ)

confusion_matrix <- confusionMatrix(predictions, true_labels)
print(confusion_matrix)
# Accuracy 0.7742, sensitivity 0.8333, specificity 0.7368
# New metrics: accuracy 0.6774, sensitivity 0.6667, specificity 0.6842

# Rpart -------------------------------------------------------------------

param_grid <- expand.grid(
  cp = seq(0.01, 0.5, by = 0.01)  
)

ctrl <- trainControl(
  method = "cv",
  number = 10,
  search = "grid"
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
# Best complexity = 0.3, accuracy = 0.70

best_rpart <- rpart_model$finalModel 
predictions <- predict(best_rpart, newdata = data_test)

rpart_predictions <- lapply(1:nrow(predictions), function(i) {
  if (predictions[i, "0"] > predictions[i, "1"]) {
    return(0)
  } else {
    return(1)
  }
})

rpart_predictions <- as.factor(rpart_predictions)

confusion_matrix <- confusionMatrix(rpart_predictions, true_labels)
print(confusion_matrix)
# Accuracy 0.6774, sensitivity 0.6667, specificity 0.6842

# KNN ---------------------------------------------------------------------

ctrl <- trainControl(
  method = "cv",          
  number = 10,           
)

set.seed(123)

knn_model <- train(
  as.factor(PHQ9) ~ .,               
  data = data_train,      
  method = "knn",         
  trControl = ctrl,
  metric = "Accuracy",
  tuneGrid = data.frame(k = seq(11, 100, by = 2))   
)

print(knn_model)
# k = 85, best accuracy 0.6638889

knn_predictions <- predict(knn_model, newdata = data_test)

confusion_matrix <- confusionMatrix(knn_predictions, true_labels)
print(confusion_matrix)
# Accuracy = 0.6774, sensitivity 0.75, specificity 0.6316

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
  method = "cv",
  number = 10 
)

xgb_model <- train(
  as.factor(PHQ9) ~ .,
  data = data_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = param_grid
)

print(xgb_model)

xgb_predictions <- predict(xgb_model, newdata = data_test)
confusion_matrix <- confusionMatrix(xgb_predictions, true_labels)
print(confusion_matrix)
# Accuracy = 0.7742, Sensitivity 0.6667, specificity 0.8421
