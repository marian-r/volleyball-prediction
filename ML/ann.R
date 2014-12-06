
# Artificial Neural Networks
library(nnet)
source("ML/functions.R")

trainANN = function(attrModel, learn, test, size = 1, decay = 0, maxit = 100)
{
  print("---------- ANN -------------")

  # the algorithm is more robust when scaled data is used

  norm.data <- scale.data(rbind(learn,test))
  norm.learn <- norm.data[1:nrow(learn),]
  norm.test <- norm.data[-(1:nrow(learn)),]

  observed <- test$Winner
  obsMat <- model.matrix(~Winner-1, norm.test)

  nn <- nnet(attrModel, data = norm.learn, size = size, decay = decay, maxit = maxit, trace = FALSE)
  predicted <- predict(nn, norm.test, type = "class")
  predMat <- predict(nn, norm.test, type = "raw")

  winner1mat = 1 - predMat
  winner2mat = predMat

  predMat = cbind(winner1mat, winner2mat)

  ca = CA(observed, predicted)
  bs = brier.score(obsMat, predMat)

  print(ca)
  print(bs)
  print(table(observed, predicted))

  list(model = nn, predicted = predicted, matrix = predMat, CA = ca, BrierScore = bs)
}

tuneANN = function(learn, test)
{
  evalDf = data.frame(row.names = c("Size", "Decay", "Maxit", "CA", "BrierScore"))
  for (size in 1:5) {
    for (decay in c(0, 0.000001, 0.000001, 0.00001, 0.0001)) {
      for (maxit in c(100, 500, 1000, 2000, 5000, 10000)) {
        print(paste("Size:", size, "Decay:", decay, "Maxit:", maxit))
        N = 5

        for (i in 1:N) {
          modelANN = trainANN(Winner ~ ., learn, test, size, decay, maxit)
          evalDf = rbind(evalDf, list(Size = size, Decay = decay, Maxit = maxit, CA = modelANN$CA, BrierScore = modelANN$BrierScore))
        }
      }
    }
  }

  evalDf
}

tuneANNwithoutSize = function(learn, test)
{
  evalDf = data.frame(row.names = c("Size", "Decay", "Maxit", "CA", "BrierScore"))
  size = 1
  for (decay in c(0, 0.000001, 0.000001, 0.00001, 0.0001)) {
    for (maxit in c(100, 500, 1000, 2000, 5000, 10000)) {
      print(paste("Size:", size, "Decay:", decay, "Maxit:", maxit))
      N = 5

      for (i in 1:N) {
        modelANN = trainANN(Winner ~ ., train, validate, size, decay, maxit)
        evalDf = rbind(evalDf, list(Size = size, Decay = decay, Maxit = maxit, CA = modelANN$CA, BrierScore = modelANN$BrierScore))
      }
    }
  }

  evalDf
}