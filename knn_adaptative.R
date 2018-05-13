library(mclust)
distXY <- function(X, Y, M=diag(dim(X)[2]))
{
        if (!is.matrix(X))
        {
                X <- as.matrix(X, nrow=1)
        }
        if (!is.matrix(Y))
        {
                Y <- as.matrix(Y, nrow=1)
        }
        nx <- dim(X)[1]
        ny <- dim(Y)[1]
        h.x <- rowSums((X%*%t(chol(M)))^2)
        h.y <- rowSums((Y%*%t(chol(M)))^2)
        ones.x <- rep(1, nx)
        ones.y <- rep(1, ny)
        
        D2xy <- h.x %*% t(ones.y) - 2 * X %*% M %*% t(Y) + ones.x %*% t(h.y)
}
knn_mahalanobis <- function(target, traindata, testdata, k) 
{
        n = nrow(testdata)
        pred = rep(NA_character_, n)
        # calculer la distance mahalanobis des donnees
        distance <- distXY(traindata, testdata) 
        #ordonner les indexes
        IndexOrdonnee = apply(distance, 2, order) 
        for(i in 1:n) 
        {
                nn <- IndexOrdonnee[1:k,i]
                # calculer les frequencies des classes
                class.frequency = table(target[nn])
                most.frequent.classes = names(which.max(class.frequency))
                pred[i] = most.frequent.classes
        }
        factor(pred)
}
estimationErreur <- function(target, data, k, n = 10, ratio = 2/3) 
{
        # construire structure de data
        error.ratios = matrix(NA_real_, nrow = length(k), ncol = 2)
        for(i in seq_along(k)) 
        {
                err = matrix(NA_real_, ncol = n, nrow = 2)
                for(j in 1:n) 
                {
                        # separer training et test data
                        split.idx = sample(nrow(data), ratio * nrow(data))
                        train.data = data[split.idx, ]
                        test.data  = data[-split.idx, ]
                        cat("---")
                        train.target <- target[split.idx]
                        test.target <- target[-split.idx]
                        # calculer les erreurs 
                        train.result = knn_mahalanobis(train.target, train.data, train.data, k[i])
                        test.result  = knn_mahalanobis(train.target, train.data, test.data, k[i])
                        err[1, j] =  mean(train.result != train.target) #adjustedRandIndex(train.result, train.target)
                        err[2, j] =  mean(test.result != test.target) #adjustedRandIndex(test.result, test.target)
                }
                err = rowMeans(err)
                error.ratios[i,] = err
        }
        # nommer
        colnames(error.ratios) = c("train erreur", "test erreur")
        rownames(error.ratios) = k
        error.ratios
}