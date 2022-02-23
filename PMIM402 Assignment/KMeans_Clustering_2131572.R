library(tidyverse)
library(factoextra)
options(repr.plot.width=15,repr.plot.height=10)
# read in the dataset
heart <- read.csv("heart-c.csv")

# create new dataframe with only continuous variables for kmeans
heart <- heart %>% select(c(age, trestbps, chol, thalach, oldpeak))

# create histograms
heart %>% gather(attributes, value, 1:ncol(heart)) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill="lightblue", color="black") +
  facet_wrap(~attributes, scales = 'free_x') +
  theme_bw()

heart_norm <- scale(heart) %>% as.data.frame

k_means <- kmeans(heart_norm, centers=2)

sse <- vector()
for (i in 1:10) {
  sse[i] <- kmeans(heart, i)$tot.withinss
}

plot(sse, type= "o")

heart_cluster <- cbind(heart_norm, "Cluster" = k_means$cluster)
heart_cluster$Cluster <- as.factor(heart_cluster$Cluster)

heart_cluster %>% gather(attributes, value, 1:ncol(heart_cluster)-1) %>%
  ggplot(aes(x=value, group = Cluster, fill = Cluster)) +
  geom_histogram(alpha=0.6, bins = 20, color = "black") +
  facet_wrap(~attributes, scales = 'free_x') +
  theme_bw()


fviz_cluster(k_means, geom = "point", data=heart_norm)

dist_mat <- dist(heart_cluster, method = "euclidean")
hir_clust <- hclust(dist_mat, method = 'average')
plot(hir_clust)
