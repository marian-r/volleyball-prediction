
library(CORElearn)
source("ML/functions.R")

trainKNN = function(attrModel, learn, test, kInNN = 10)
{
  print("---------- KNN -------------")

  # CA and Brier Score calculation

  observed <- test$Winner
  obsMat <- model.matrix(~Winner-1, test)

  mlModel <- CoreModel(attrModel, data = learn, model = "knn", kInNN = kInNN)
  #plot(mlModel, learn)

  predicted <- predict(mlModel, test, type="class")
  predMat <- predict(mlModel, test, type = "probability")

  ca = CA(observed, predicted)
  bs = brier.score(obsMat, predMat)

  print(ca)
  print(bs)
  print(table(observed, predicted))

  list(model = mlModel, predicted = predicted, matrix = predMat, CA = ca, BrierScore = bs)
}

tuneKNN = function(learn, test)
{
  evalDf = data.frame(row.names = c("kInNN", "CA", "BrierScore"))
  for (kInNN in 1:100) {
    print(paste("kInNN:", kInNN))

    modelKNN = trainKNN(Winner ~ ., learn, test, kInNN)
    evalDf = rbind(evalDf, list(kInNN = kInNN, CA = modelKNN$CA, BrierScore = modelKNN$BrierScore))
  }

  evalDf
}
