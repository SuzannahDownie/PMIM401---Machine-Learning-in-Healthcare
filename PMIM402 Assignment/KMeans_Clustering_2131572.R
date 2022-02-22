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

k_means <- kmeans(heart_norm, centers=3)

sse <- vector()
for (i in 1:10) {
  sse[i] <- kmeans(heart, i)$tot.withinss
}

plot(sse, type= "o")
