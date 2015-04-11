setwd("/Users/kiote/www/kaggle/edx/week6")

dailykos = read.csv("dailykos.csv")
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")

plot(kosHierClust)

clusters = cutree(kosHierClust, k = 7)
cluster1 = subset(dailykos, clusters==1)
cluster2 = subset(dailykos, clusters==2)
cluster3 = subset(dailykos, clusters==3)
cluster4 = subset(dailykos, clusters==4)
cluster5 = subset(dailykos, clusters==5)
cluster6 = subset(dailykos, clusters==6)
cluster7 = subset(dailykos, clusters==7)

tail(sort(colMeans(cluster7)))

set.seed(1000)

dailykosMatrix = as.matrix(dailykos)
dailykosVector = as.vector(dailykosMatrix)
kmeans = kmeans(dailykosVector, centers = 7)

KmeansCluster1 = subset(dailykos, kmeans$cluster == 1)
KmeansCluster2 = subset(dailykos, kmeans$cluster == 2)
KmeansCluster3 = subset(dailykos, kmeans$cluster == 3)
KmeansCluster4 = subset(dailykos, kmeans$cluster == 4)
KmeansCluster5 = subset(dailykos, kmeans$cluster == 5)
KmeansCluster6 = subset(dailykos, kmeans$cluster == 6)
KmeansCluster7 = subset(dailykos, kmeans$cluster == 7)

tail(sort(colMeans(KmeansCluster1)))
table(kosHierClust, kmeans$cluster)

#
#
#

airlines = read.csv("AirlinesCluster.csv")
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

airDist = dist(airlinesNorm, method="euclidean")
airHierClust = hclust(airDist, method="ward.D")
plot(airHierClust)

airClusters = cutree(airHierClust, k = 5)

cluster1 = subset(airlines, airClusters==1)
cluster2 = subset(airlines, airClusters==2)
cluster3 = subset(airlines, airClusters==3)
cluster4 = subset(airlines, airClusters==4)
cluster5 = subset(airlines, airClusters==5)

tapply(airlines$Balance, airClusters, mean)
summary(cluster1)

lapply(split(airlines, airClusters), colMeans)

airMatrix = as.matrix(airlines)
airVector = as.vector(airMatrix)
KMC = kmeans(airVector, centers = 5, iter.max = 1000)

#
# 3
#
stocks = read.csv('StocksCluster.csv')

max(cor(stocks))

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~., family=binomial, data=stocksTrain)
prediction = predict(StocksModel, type = "response", newdata = stocksTest)
table(stocksTest$PositiveDec, prediction>0.5)
(417+1553)/nrow(stocksTest)

(344+1553)/nrow(stocksTest)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

set.seed(144)

km = kmeans(normTrain, centers = 3)

KmeansCluster1 = subset(normTrain, km$cluster == 1)
KmeansCluster2 = subset(normTrain, km$cluster == 2)
KmeansCluster3 = subset(normTrain, km$cluster == 3)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 = subset(stocksTrain, km$cluster == 1)
stocksTrain2 = subset(stocksTrain, km$cluster == 2)
stocksTrain3 = subset(stocksTrain, km$cluster == 3)

stocksTest1 = subset(stocksTest, km$cluster == 1)
stocksTest2 = subset(stocksTest, km$cluster == 2)
stocksTest3 = subset(stocksTest, km$cluster == 3)

summary(stocksTrain3$PositiveDec)

StocksModel1 = glm(PositiveDec ~., family=binomial, data=stocksTrain1)
StocksModel2 = glm(PositiveDec ~., family=binomial, data=stocksTrain1)
StocksModel3 = glm(PositiveDec ~., family=binomial, data=stocksTrain1)

summary(StocksModel3)
