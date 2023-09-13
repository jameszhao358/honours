# Import libraries 

library(tidyr)
library(dplyr)
library(factoextra)
library(dbscan)
library(ppclust)
library(e1071)
library(caret)

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
  
rf_model <- train(
  as.factor(PHQ9) ~ .,
  data = data_train,
  method = "ranger",
  trControl = ctrl,         
  tuneGrid = param_grid
)

print(rf_model)
# The final values used for the model were mtry = 6, splitrule = gini and min.node.size = 10.
# Best accuracy = 0.732

predictions <- predict(rf_model, newdata = data_test)
true_labels = data_test$PHQ

confusion_matrix <- table(True = true_labels, Predicted = predictions)
print(confusion_matrix)

accuracy <- (confusion_matrix[1, 1] + confusion_matrix[2, 2]) / sum(confusion_matrix)
# Accuracy: 0.7419355

precision <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1])
# Precision: 0.6428571

recall <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[1, 2])
# Recall (Sensitivity): 0.75 

f1_score <- 2 * (precision * recall) / (precision + recall)
# F1-Score: 0.6923077 









