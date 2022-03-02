library(tidyverse)
library(factoextra)

# read in the dataset
heart <- read.csv("heart-c.csv")
# create new dataframe with only continuous variables for kmeans and remove
# the index
heart <- heart %>% select(age, trestbps, chol, 
                          thalach, oldpeak)

# create histograms of continuous data
heart %>% gather(attribute, value, 1:ncol(heart)) %>%
  ggplot(aes(x = value)) + 
  geom_histogram(fill = "salmon1", color = "black") +
  facet_wrap(~attribute, scales = 'free') +
  theme_grey()

# normalise the data so as it sits within a similar range
heart_norm <- scale(heart) %>% as.data.frame

# visualise the sum of squared error to determine the initial
# optimum no. of clusters
fviz_nbclust(x = heart_norm, FUNcluster = kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  geom_vline(xintercept = 5, linetype = 2) 

# visualise the silhouette scores to support elbow plot above
fviz_nbclust(x = heart_norm, FUNcluster = kmeans, method = 'silhouette')

# run kmeans with 2 clusters and with 5
kmeans_2 <- kmeans(heart_norm, centers = 2)
kmeans_5 <- kmeans(heart_norm, centers = 5)

# visualise both models in 2D
fviz_cluster(kmeans_2, geom = "point", pointsize = 2, data = heart_norm, main = 'Cluster Plot in 2D Space - 2 Clusters', 
             xlab = 'Dimension 1', ylab = 'Dimension 2')
fviz_cluster(kmeans_5, geom = "point", pointsize = 2, data = heart_norm, main = 'Cluster Plot in 2D Space - 5 Clusters', 
             xlab = 'Dimension 1', ylab = 'Dimension 2')

# add cluster values to dataframes as factors
# Two Clusters
heart_clust_2 <- cbind(heart, 'Cluster' = kmeans_2$cluster)
heart_clust_2$Cluster <- as.factor(heart_clust_2$Cluster)

# Five Clusters
heart_clust_5 <- cbind(heart, 'Cluster' = kmeans_5$cluster)
heart_clust_5$Cluster <- as.factor(heart_clust_5$Cluster)

# plot histograms to show distribution across the clusters
heart_clust_2 %>% gather(attribute, value, 1:ncol(heart_clust_2)-1) %>%
  ggplot(aes(x = value, group = Cluster, fill = Cluster)) + 
  geom_histogram(alpha = 0.6, bins = 30, color = 'black') +
  facet_wrap(~attribute, scales = 'free') +
  theme_grey()

heart_clust_5 %>% gather(attribute, value, 1:ncol(heart_clust_5)-1) %>%
  ggplot(aes(x = value, group = Cluster, fill = Cluster)) + 
  geom_histogram(alpha = 0.6, bins = 30, color = 'black') +
  facet_wrap(~attribute, scales = 'free') +
  theme_grey()


# Hierarchical Clustering
library(dendextend)
library(cluster)

# create distance matrix
# a matrix for use with fviz_nbclust
dist_1 <- dist(heart_norm, method = "euclidean") %>% as.matrix(dist_1)

# a vector for use with hclust
dist_2 <- dist(heart_norm, method = 'euclidean')

# use fviz_nbclust to view initial optimal clusters and silhouette plot
fviz_nbclust(x = dist_1, FUNcluster = hcut, method = "wss")
fviz_nbclust(x = dist_1, FUNcluster = hcut, method = 'silhouette')

# rm(dist_mat, hir_clust, hir_dend)
# Create hierarchical cluster model and plot
hir_clust <- hclust(dist_2, method = 'ward.D') 
plot(hir_clust, cex = 0.6, hang = -1) 
rect.hclust(hir_clust, k = 2, border = 2:6)
abline(h = 80, col = 'blue')

# plot hierarchical cluster with colour branches
hir_dend <- as.dendrogram(hir_clust)
plot(color_branches(hir_dend, k = 2), main = 'Cluster Dendrogram - 2 Clusters')
