library(datasets)
head(iris)

##################### K-means clustering
#K-means clustering voi k=3, su dung 2 cot 3,4 cua so lieu
set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

# So sanh gia tri du bao va gia tri thuc te
table(irisCluster$cluster, iris$Species)

# Ve cac quan sat theo hai bien Petal.Length, Petal.Width,
# to mau theo gia tri phan cum thuc te, dung ky tu cho phan cum du bao
plot(iris$Petal.Length, iris$Petal.Width,col=iris$Species, pch=irisCluster$cluster)



##################### Hierarchical clustering
distances = dist(iris[,3:4], method="euclidean")
hier_clust = hclust(distances, method = "average")
plot(hier_clust)

clusterCut <- cutree(hier_clust, 3)
table(clusterCut, iris$Species)

plot(iris$Petal.Length, iris$Petal.Width,col=iris$Species, pch=clusterCut)