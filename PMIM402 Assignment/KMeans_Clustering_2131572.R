library(tidyverse)
library(factoextra)
options(repr.plot.width=15,repr.plot.height=10)
# read in the dataset
heart <- read.csv("heart-c.csv")

# create new dataframe with only continuous variables for kmeans
heart <- heart %>% select(c(age, trestbps, chol, thalach, oldpeak))

heart_norm <- scale(heart) %>% as.data.frame
View(heart_norm)
