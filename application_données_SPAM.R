setwd("D:/Bi/utc/SY09/Projet1")
source("./kmeans_adaptative.R")
library(MASS)
library(cluster)
library(mclust)

#Donnees Spam
Spam <- read.csv("spam.csv", header=T, row.names=1)
for (i in 1:54)
{
        Spam[,i][which(Spam[,i]>0)] <-1
}
X <- Spam[,-58]
z <- Spam[,58]
acp = princomp(X,cor=TRUE)
summary(acp,loadings=TRUE)
biplot(acp)
screeplot(acp,type="lines")
PC = predict(acp)[,1:40]
PC <- as.matrix(PC)
km <- kmeans(PC,2)
adjustedRandIndex(z,km$cluster)
resSpam = kmeans_da(PC,2)
adjustedRandIndex(z,resSpam$Cluster)

#PPV

Spam <- read.csv("spam.csv", header=T, row.names=1)
X <- Spam[,-58]
z <- Spam[,58]


source("./knn_adaptative.R")
nTotal <- nrow(X)
Spam.training <- as.matrix(X[1 : round((nTotal *2/3)),])
Spam.test <- as.matrix(X[(round((nTotal *2/3)) + 1) : nTotal,])
Spam.target <- as.matrix(z[1 : round((nTotal *2/3))])
pred <- knn_mahalanobis(Spam.target, traindata = Spam.training, testdata = Spam.test, k = 2)
print(pred)
print(table(pred, z[-(1 : round((nTotal *2/3)))]))

res <- estimationErreur(z, X, k = c(2, 3, 4), n = 5)
print(res)


