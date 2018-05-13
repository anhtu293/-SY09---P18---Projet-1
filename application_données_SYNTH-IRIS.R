setwd("D:/Bi/utc/SY09/Projet1")
source("./kmeans_adaptative.R")
library(MASS)
library(cluster)
library(mclust)
library(factoextra)
    #Donnees Synth1
        X1 <- read.csv("./Synth1.csv", header=T, row.names=1) 
        
        z <- X1[,3] 
        X <- X1[,-3]
    
        #plot 
          x11() 
          par(mfrow=c(1,3))
          plot(X1$X.1~X1$X.2, pch=21, xlab="X.2", ylab="X.1", bg=c("red","green3")[unclass(X1$z)], main="Répartition des données synth1")
        
        #hc
          d <- dist(X, diag = TRUE, method = "manhattan")
          hc <- hclust(d, method = "ward.D2")
          plot(hc,  main = "Dendrogramme des données synthétiques 1\n Méthode: ward")
        
        #PCA rapide à comparer avec clusplot
          x11()
          par(mfrow=c(1,2))
          fviz_pca_ind(res.pca<-princomp(X, scale=TRUE), habillage=X1$z)
       
         #kmeans classique
          km <- kmeans(X, 2)
          x11()
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 1 : Kmeans classique avec k = 2", xlab="composante 1", ylab="composante 2")
          inertie_matrice <- matrix(0, nrow = 100, ncol = 10)
          for (i in 1:10)
          {
            for (j in 1:100)
            {
              inertie_matrice[j,i] <- kmeans(X,i)$tot.withinss
            }
          }
          inertie <- apply(inertie_matrice,2,min)
          plot(inertie, main = "Synth 1 :Inertie intra-classe minimale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
          apply(inertie_matrice,2,var)
      
        # indice de rand
          IndiceRand<-matrix (0, nrow = 10, ncol = 6)
          colnames(IndiceRand) <-c("k1","k2","k3", "k4", "k5", "k6")
          for (i in 1:6){
            for (j in 1:10)
            {
              IndiceRand[j,i]<-adjustedRandIndex(z, kmeans(X, i)$cluster)
            }
          } 
          Rand<-apply(IndiceRand,2,max)
          x11()
          plot(Rand, main = "Synth 1: Indice de Rand maximal en fonction de K ", type = "l", xlab = "K", ylab = "Indice de Rand") 
          


        #kmeans adatative
          source("./kmeans_adaptative.R")
          X <- as.matrix(X)
          ka <- kmeans_da(X, 2)
          x11()
          clusplot(X, ka$Cluster,color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 1 : Kmeans adaptative avec k = 2", xlab="composante 1", ylab="composante 2")
          comparer <- adjustedRandIndex(ka$Cluster, z)
          
          inertie_matrice <- matrix(0, nrow = 10, ncol = 10)
          for (i in 1:10)
          {
            for (j in 1:10)
            {
              ka <- kmeans_da(X, i)
              inertie_matrice[j,i] <- ka$Valeur_critere
            }
          }
          inertie <- apply(inertie_matrice,2,max)
          plot(inertie, main = "Inertie intra-classe minimale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
          apply(inertie_matrice,2,var)

        #indice de rand k-means adaptative, à déterminer que pour k=2
          IndiceRand<-matrix (0, nrow = 10, ncol = 6)
          colnames(IndiceRand) <-c("k1","k2","k3", "k4", "k5", "k6")
          for (i in 1:6){
            for (j in 1:10)
            {
              IndiceRand[j,i]<-adjustedRandIndex(z, kmeans_da(X, i)$Cluster)
            }
          } 
          Rand<-apply(IndiceRand,2,max)
          plot(Rand, main = "Synth 1: Indice de Rand maximal en fonction de K ", type = "l", xlab = "K", ylab = "Indice de Rand") 


        #Donnees Synth2
          X2 <- read.csv("./Synth2.csv", header=T, row.names=1) 
          z <- X2[,3] 
          X <- X2[,-3]
        
        #plot 
          x11()
          plot(X2$X.1~X2$X.2, pch=21, xlab="X.2", ylab="X.1", bg=c("orange","blue")[unclass(X2$z)], main="Répartition des données synth2")
       
        #hc
          d <- dist(X, diag = TRUE)
          hc <- hclust(d)
          plot(hc, main = "Synth 2")

        #PCA rapide
         x11() 
         fviz_pca_ind(res.pca<-princomp(X, scale=TRUE), habillage=X1$z)
          
        #kmeans classique
          km <- kmeans(X, 2)
          x11() 
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 2 : Kmeans classique avec k = 2")
          km <- kmeans(X, 3)
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 2 : Kmeans classique avec k = 3")
          comparer <- adjustedRandIndex(z, km$cluster)
          comparer
          
          # inertie intraclasse
            inertie_matrice <- matrix(0, nrow = 100, ncol = 10)
            for (i in 1:10)
            {
              for (j in 1:100)
              {
                inertie_matrice[j,i] <- kmeans(X,i)$tot.withinss
              }
            }
            inertie <- apply(inertie_matrice,2,min)
            plot(inertie, main = "synth2: Inertie intra-classe minimale \n en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
            apply(inertie_matrice,2,var)
          
          # indice de rand
            IndiceRand<-matrix (0, nrow = 10, ncol = 6)
            colnames(IndiceRand) <-c("k1","k2","k3", "k4", "k5", "k6")
            for (i in 1:6){
              for (j in 1:10)
              {
                IndiceRand[j,i]<-adjustedRandIndex(z, kmeans(X, i)$cluster)
              }
            } 
            Rand<-apply(IndiceRand,2,max)
            plot(Rand, main = "synth2: Indice de Rand en fonction du nombre de groupes <K> \n ", type = "l", xlab = "K", ylab = "Indice de Rand")
      
          #proortion de inertie expliquée 
            inertie_Prop<- matrix(0, nrow = 100, ncol = 10)
            for (i in 1:10)
            {
              for (j in 1:100)
              {
                inertie_Prop[j,i] <- kmeans(X,i)$betweenss/kmeans(X,i)$totss
              }
            }
            inertieProp <- apply(inertie_Prop,2,max)
            x11()
            plot(inertieProp, main = "Proportion d'inertie maximale inter-classe en fonction de k", type = "l", xlab = "K", ylab = "Proportion inertie inter-classe", ylim=c(0,1), xlim=c(0,10))
            
          # autre alternative: indice de Calinski Harabasz, on cherche quel est le nombre de groupe pour maximiser le critère inter-classe
            library(fpc)
            sol.kmeans <- kmeansruns(X,krange=1:10,criterion="ch")
            x11()
            plot(1:10,sol.kmeans$crit,type="b",xlab="Nombre de groupes",ylab="Silhouette", main="Données synth2: Détermination du nombre de groupe maximisant \n le critère inter-classe")
            
            
            
        #kmeans adatative
        source("./kmeans_adaptative.R")
        X <- as.matrix(X)
        ka <- kmeans_da(X, 2)
        x11()
        clusplot(X, ka$Cluster,color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 2 : Kmeans adaptative avec k = 2")
        comparer <- adjustedRandIndex(z, ka$Cluster)
        comparer
        ka <- kmeans_da(X, 7)
        clusplot(X, ka$Cluster,color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 1 : Kmeans adaptative avec k = 7")
        inertie_matrice <- matrix(0, nrow = 2, ncol = 10)
        for (i in 1:2)
        {
          print(i)
          for (j in 1:10)
          {
            ka <- kmeans_da(X, i, n_inter = 1)
            inertie_matrice[j,i] <- ka$Valeur_critere
          }
        }
        inertie <- apply(inertie_matrice,2,max)
        plot(inertie, main = "Inertie intra-classe minimale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
        apply(inertie_matrice,2,var)



    #Donnees Synth3
        X3 <- read.csv("./Synth3.csv", header=T, row.names=1) 
        z <- X3[,3] 
        X <- X3[,-3]
        
      #plot 
        x11()
        plot(X3$X.1~X3$X.2, pch=21, xlab="X.2", ylab="X.1", bg=c("red","green3")[unclass(X3$z)], main="Répartition des données synth3")
      #hc
        d <- dist(X, diag = TRUE)
        hc <- hclust(d)
        plot(hc, main = "Synth 3")
        
      #PCA rapide à comparer avec clusplot
        x11()
        par(mfrow=c(1,2))
        fviz_pca_ind(res.pca<-princomp(X, scale=TRUE), habillage=X3$z)
        
    #kmeans classique
        km <- kmeans(X, 2)
        x11()
        clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 3 : Kmeans classique avec k = 2")
        comparer <- adjustedRandIndex(z, km$cluster)
        comparer
        km <- kmeans(X, 5)
        clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 1 : Kmeans classique avec k = 5")
        inertie_matrice <- matrix(0, nrow = 100, ncol = 10)
        for (i in 1:10)
        {
          for (j in 1:100)
          {
            inertie_matrice[j,i] <- kmeans(X,i)$tot.withinss
          }
        }
        inertie <- apply(inertie_matrice,2,min)
        plot(inertie, main = "Inertie intra-classe minimale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
        apply(inertie_matrice,2,var)

    #kmeans adatative
        source("./kmeans_adaptative.R")
        X <- as.matrix(X)
        ka <- kmeans_da(X, 2)
        x11()
        clusplot(X, ka$Cluster,color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth3 : Kmeans adaptative avec k = 2")
        comparer <- adjustedRandIndex(z, ka$Cluster)
        comparer
        ka <- kmeans_da(X, 5)
        clusplot(X, ka$Cluster,color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Synth 1 : Kmeans adaptative avec k = 5")
        inertie_matrice <- matrix(0, nrow = 2, ncol = 10)
        for (i in 1:10)
        {
          print(i)
          for (j in 1:2)
          {
            ka <- kmeans_da(X, i, n_inter = 10)
            inertie_matrice[j,i] <- ka$Valeur_critere
          }
        }
        inertie <- apply(inertie_matrice,2,min)
        plot(inertie, main = "Inertie intra-classe minimale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
        apply(inertie_matrice,2,var)


########Iris
        data(iris) 
        X <- iris[,1:4] 
        z<-as.numeric(iris$Species) #1=setosa/ 2=versicolor/ 3=virginica
        
        
        #kmeans classique
          x11() 
          par(mfrow=c(2,2))
          km <- kmeans(X, 1)
          km <- kmeans(X, 2)
          km <- kmeans(X, 3)
          # indice de rand 
            clusplot(X, km$cluster, color = TRUE ,   shade = TRUE, labels = 2, lines = 0, main = "Classification des données iris avec k = 1")  
            comparer <- adjustedRandIndex(z, (km$cluster))
            comparer
          
          
          km <- kmeans(X, 4)
          km <- kmeans(X, 5)
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans classique \n données iris avec k = 2") 
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans classique \n données iris avec k = 3")
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans classique \n données iris avec k = 4")
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans classique \n données iris avec k = 5")
          fviz_pca_ind(res.pca<-princomp(X, scale=TRUE), habillage=iris$Species)
          
          km <- kmeans(X, centers=3)
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans classique \n données iris avec k = 3")
          comparer <- adjustedRandIndex(z, km$cluster)
          comparer
          km <- kmeans(X, 4)
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans classique \n données iris avec k = 4")
          comparer <- adjustedRandIndex(z, km$cluster)
          comparer
          km <- kmeans(X, 5)
          clusplot(X, km$cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans classique \n données iris avec k = 5")
          comparer <- adjustedRandIndex(z, km$cluster)
          comparer
          
      #indice de rand
          IndiceRand<-matrix (0, nrow = 10, ncol = 6)
          colnames(IndiceRand) <-c("k1","k2","k3", "k4", "k5", "k6")
            for (i in 1:6){
              for (j in 1:10)
                {
                  IndiceRand[j,i]<-adjustedRandIndex(z, kmeans(X, i)$cluster)
                }
              }
          write.csv(IndiceRand, "Indice_de_rand_Iris_kmeans.csv")
          IndiceRand<-read.csv("Indice_de_rand_Iris_kmeans.csv", row.names = "X")
          matrice_Rand<-matrix(0,nrow=5, ncol=2)
          colnames(matrice_Rand)<-c("means", "ecart-type")
          row.names(matrice_Rand)<-c("1", "2","3","4","5")
          for (i in 1:5){
            matrice_Rand[i,1]<-mean(IndiceRand[,i])
          }
          for (i in 1:5){
            matrice_Rand[i,2]<-sd(IndiceRand[,i])
          }
          matrice_Rand<-as.data.frame(matrice_Rand)
          #matrice_Rand$k<-v1
          x11()
          plot(matrice_Rand$means, xlab="K", ylab="moyenne", type = "o", main="Indice de Rand pour les données Iris\n d'après kmeans-classique")              
        
       #indice de inertie totale                  
          inertie_matriceTotale <- matrix(0, nrow = 100, ncol = 10)
          for (i in 1:10)
          {
            for (j in 1:100)
            {
              inertie_matriceTotale[j,i] <- kmeans(X,i)$tot.withinss
            }
          }
          x11()
          par(mfrow=c(1,2))
          inertieTot <- apply(inertie_matriceTotale,2,min)
          plot(inertieTot, main = "Iris: Inertie intra-classe minimale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
          apply(inertie_matrice,2,var)
          
        #indice de inertie inter 
          inertie_Interclasse<- matrix(0, nrow = 100, ncol = 10)
            for (i in 1:10)
            {
              for (j in 1:100)
              {
                inertie_Interclasse[j,i] <- kmeans(X,i)$betweenss
              }
            }
            inertieInter <- apply(inertie_Interclasse,2,max)
            plot(inertieInter, main = "Iris: Inertie inter-classe maximale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
            apply(inertie_matrice,2,var)
            
          #proortion de inertie expliquée 
            inertie_Prop<- matrix(0, nrow = 100, ncol = 10)
            for (i in 1:10)
            {
              for (j in 1:100)
              {
                inertie_Prop[j,i] <- kmeans(X,i)$betweenss/kmeans(X,i)$tot.withinss
              }
            }
            inertieProp <- apply(inertie_Prop,2,max)
            plot(inertieProp, main = "Proportion d'inertie maximale inter-classe en fonction de k", type = "l", xlab = "K", ylab = "Inertie intra-classe")
            apply(inertie_matrice,2,var)  
            
        # autre alternative: indice de Calinski Harabasz, on cherche quel est le nombre de groupe pour maximiser le critère inter-classe
            library(fpc)
            sol.kmeans <- kmeansruns(X,krange=1:10,criterion="ch")
            x11()
            plot(1:10,sol.kmeans$crit,type="b",xlab="Nombre de groupes",ylab="Silhouette", main="Données Iris: Détermination du nombre de groupe maximisant \n le critère inter-classe")
       
            
       #K-means adapatif
          x11() 
          X<-as.matrix(X)
          par(mfrow=c(2,2))
          X <- as.matrix(X)
          ka <- kmeans_da(X, 1)
          clusplot(X, ka$Cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans adaptative \n données iris avec k = 1")
          ka$Valeur_critere
          ka2 <- kmeans_da(X, 2)
          clusplot(X, ka2$Cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans adaptative \n données iris avec k = 2")
          ka$Valeur_critere
          ka <- kmeans_da(X, 3)
          ka3<-ka
          clusplot(X, ka3$Cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans adaptative \n données iris avec k = 3")
          ka$Valeur_critere
          ka4 <- kmeans_da(X, 4)
          clusplot(X, ka4$Cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans adaptative \n données iris avec k = 4")
          ka$Valeur_critere
          ka5 <- kmeans_da(X, 5)
          clusplot(X, ka5$Cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans adaptative \n données iris avec k = 5")
          ka$Valeur_critere
          ka <- kmeans_da(X, 2)
          clusplot(X, ka$Cluster, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans adaptative \n données iris avec k = 2")
          ka$Valeur_critere
         
          ValeurC<-matrix (0, nrow = 10, ncol = 6)
          colnames(ValeurC) <-c("k1","k2","k3", "k4", "k5", "k6")
          for (i in 1:6){
            for (j in 1:10)
            {
              ValeurC[j,i]<-kmeans_da(X, i)$Valeur_critere
            }
          }
          
          IndiceRand<-matrix (0, nrow = 10, ncol = 6)
          colnames(IndiceRand) <-c("k1","k2","k3", "k4", "k5", "k6")
          for (i in 1:6){
            for (j in 1:10)
            {
              IndiceRand[j,i]<-adjustedRandIndex(z, kmeans_da(X, i)$Cluster)
            }
          }
          write.csv(IndiceRand, "Indice_de_rand_Iris.csv")
          IndiceRand<-read.csv("Indice_de_rand_Iris.csv", row.names = "X")
          matrice_Rand<-matrix(0,nrow=5, ncol=2)
                      colnames(matrice_Rand)<-c("means", "ecart-type")
                      row.names(matrice_Rand)<-c("1", "2","3","4","5")
                      for (i in 1:5){
                        matrice_Rand[i,1]<-mean(IndiceRand[,i])
                      }
                      for (i in 1:5){
                        matrice_Rand[i,2]<-sd(IndiceRand[,i])
                      }
                    matrice_Rand<-as.data.frame(matrice_Rand)
                     x11()
                     plot(matrice_Rand$means, xlab="K", ylab="moyenne", type = "o", main="Indice de Rand pour les données Iris\n d'après kmeans-adaptatif")         
          
          inertie_matrice <- matrix(0, nrow = 3, ncol = 10)
          for (i in 1:10)
          {
            print(i)
            for (j in 1:3)
            {
              ka <- kmeans_da(X, i)
              inertie_matrice[j,i] <- ka$Valeur_critere
            }
          }
          inertie <- apply(inertie_matrice,2,max)
          x11()
          plot(inertie, main = "Données Iris: Inertie intra-classe minimale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
          apply(inertie_matrice,2,var)
        
