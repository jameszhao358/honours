library(tidyr)
library(dplyr)
library(factoextra)

master_df <- read.csv("C:/Users/james/Desktop/honours/master.csv") %>%
  mutate(PHQ_classification = ifelse(PHQ_classification == "Subclinical", 0, 1)) %>%
  select(-PIN) %>%
  mutate(across(everything(), ~ replace_na(., median(., na.rm = TRUE)))) # Replacing all n/a values with the column median

data <- master_df %>%
  select(-"PHQ_classification") %>%
  select(-c(1:21))

data_scale <- scale(data)

data <- dist(data_scale)

# Within sum squares 
fviz_nbclust(data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# K-means 

km.out <- kmeans(data_scale, centers = 3, nstart = 100)
print(km.out)

km.clusters <- km.out$cluster
rownames(data_scale) <- paste(master_df$PHQ_classification, 1:dim(master_df)[1], sep = "_")
fviz_cluster(list(data=data_scale, cluster = km.clusters))
table(km.clusters, master_df$PHQ_classification)

# DBSCAN clustering 

library(dbscan)

# Finding optimal epsilon using elbow plot
kNNdistplot(data_scale, minPts = 7)

# DBScan 
dbscan_res <- dbscan(data_scale, eps = 2, minPts = 7)
dbscan_res
plot(data_scale, col = dbscan_res$cluster+1, main = "DBSCAN")

# Fuzzy clustering

library(ppclust)

fuzzy_clusters <- fcm(data_scale, centers = 3)
factoextra::fviz_cluster(list(data=fuzzy_clusters, cluster = fuzzy_clusters$cluster),  
                         geom = "point", 
                         ellipse= FALSE, 
                         show.clust.cent = FALSE,
                         palette = "jco", 
                         ggtheme = theme_classic())

# PCA Biplot

pca <- prcomp(data_scale)
fviz_pca_biplot(pca,
                label="var",
                habillage = master_df$PHQ_classification)



