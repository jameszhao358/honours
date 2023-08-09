# Import libraries 

library(tidyr)
library(dplyr)
library(factoextra)
library(dbscan)
library(ppclust)
library(e1071)

# Select significant variables 

features_temp <- read.csv("C:/Users/james/Desktop/honours/processed_data/extracted_features.csv")

features <- features_temp %>%
  select(overall_kht, transferq_kht, transferq_ft, VVR_duration, transfer_ft, behavioural_ft, non_behavioural_kht, PHQ_classification) %>%
  mutate(PHQ_classification = ifelse(PHQ_classification == "Subclinical", 0, 1)) %>%
  mutate(across(everything(), ~ replace_na(., median(., na.rm = TRUE)))) # Replacing all n/a values with the column median

data <- features %>%
  select(-PHQ_classification)

data_scale <- as.data.frame(scale(data))

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

fuzzy_clusters <- fcm(data, centers = 3)

factoextra::fviz_cluster(list(data = fuzzy_clusters, cluster = fuzzy_clusters$cluster),  
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
