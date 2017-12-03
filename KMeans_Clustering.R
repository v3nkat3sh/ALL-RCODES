library(datasets)
head(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3)
irisCluster

table(irisCluster$cluster, iris$Species)

plot(iris[c("Sepal.Length", "Sepal.Width")], col=irisCluster$cluster)

# ASSIGNMENT
# Perform a clustering analysis on forestfires dataset 
# with numerical variables. Justify your choice of
# the number of clusters