source("./fonctions/distXY.R")
library(MASS)
kmeans_da<-function(data,k, ro = as.vector(matrix(1, ncol = k)), n_iter = 100, n_ess = 1, e = 10^-5){
        rows<-nrow(data) #nombre de colonne
        cols<-ncol(data) #nombre de colonne
        
        u_optimum<-matrix(0,nrow=k,ncol=cols)
        JOpt=1000000000
        Vk_optimum = list()
        clustersOptimum = matrix(0,nrow=rows,ncol=1) 
        nIteration = 0
        
        for (i in 1:n_ess){
                #enregistrer la classe de chaque individu
                clustering<-matrix(0,nrow=rows,ncol=1)
                
                u_pre<-matrix(0,nrow=k,ncol=cols) #matrice de centres précédents
                u<-matrix(0,nrow=k,ncol=cols) #matrice de nouveaux centres
                #initialisation de centres des classes au hasard
                rand<-as.vector(sample(1:rows,size=k)) #Générer les indices aléatoires
                for(i in 1:k){ # tirer k centres dans le tableau de données
                        clustering[rand[i],1]<-i
                        u[i,]<-data[rand[i],]
                        u<-matrix(u,k,cols)
                }
                #initialisation de matrices de covariance normalisée
                rhoK<-1
                Ip<-diag(cols)
                Vk=list()
                for (m in 1:k){
                        Vk[[m]] = rhoK^(-1/cols)*Ip
                }
                
                #Répartition
                nbrIter = 0 #nombre d'itérations
                erreur=100 #critère de convergence
                
                while((erreur>e)&(nbrIter<n_iter)){
                        nbrIter = nbrIter + 1
                        u_pre = u
                        
                        #nouvelle partition
                        clustering=apply(data,1,function(y){
                                which.min(lapply(t(c(1:k)),function(z){
                                        distXY(y,u_pre[z,],ginv(Vk[[z]]))
                                }))
                        })
                        indexMarix = as.matrix(clustering,cols,1)
                        #mise a jour de centres
                        for(m in 1:k){
                                clusterMatrix<-data[indexMarix[,1]==m,]
                                clusterMatrix<-as.matrix(clusterMatrix)
                                u[m,]<-colMeans(clusterMatrix)
                        }
                        #mise a jour de Vk
                        for(m in 1:k){
                                somme<-matrix(0,nrow=cols,ncol=cols)
                                clusterMatrix<-data[indexMarix[,1]==m,] 
                                clusterMatrix<-as.matrix(clusterMatrix)
                                for (i in 1:nrow(clusterMatrix)){
                                        a = clusterMatrix[i,]-u[m,]
                                        somme = somme+(a%*%t(a))
                                }
                                Vk[[m]] = 1/nrow(clusterMatrix)*somme
                                Vk[[m]] = (1*det(Vk[[m]]))^(-1/cols)*Vk[[m]]
                        }
                        #mise a jour le critère de convergence
                        erreurTot = 0
                        for(m in 1:k){
                                erreurTot =erreurTot+ dist(rbind(u_pre[m,],u[m,]),method="euclidean")
                        }
                        erreur = erreurTot
                }#fin while
                
                #optimum local
                JNew = 0
                for (m in 1:k){
                        clusterMatrix<-data[indexMarix[,1]==m,] 
                        clusterMatrix<-as.matrix(clusterMatrix)
                        JNew = JNew+sum(apply(clusterMatrix,1,function(y){
                                distXY(y,u[m,],ginv(Vk[[m]]))
                        }))
                }
                if (JNew < JOpt){
                        JOpt = JNew
                        u_optimum = u
                        Vk_optimum = Vk
                        clustersOptimum = clustering
                        nIteration = nbrIter
                }
        }
        return(list(Valeur_critere = JOpt,Numbre_iterations = nIteration, Cluster = clustersOptimum, Centres = u_optimum, Covariance_Normalisee = Vk_optimum))
}
