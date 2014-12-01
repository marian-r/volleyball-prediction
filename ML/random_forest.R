
library(CORElearn)
source("ML/functions.R")

trainRF = function(attrModel, learn, test, minNodeWeightRF = 2)
{
  print("---------- Random Forest -------------")

  # CA and Brier Score calculation

  observed <- test$Winner
  obsMat <- model.matrix(~Winner-1, test)

  mlModel <- CoreModel(attrModel, data = learn, model = "rf", minNodeWeightRF = minNodeWeightRF)
  #plot(mlModel, learn)

  predicted <- predict(mlModel, test, type="class")
  predMat <- predict(mlModel, test, type = "prob")

  ca = CA(observed, predicted)
  bs = brier.score(obsMat, predMat)

  print(ca)
  print(bs)
  print(table(observed, predicted))

  list(model = mlModel, predicted = predicted, matrix = predMat, CA = ca, BrierScore = bs)
}

tuneRF = function(learn, test)
{
  evalDf = data.frame(row.names = c("NodeWeightRF", "CA", "BrierScore"))
  for (nodeWeight in 1:10) {
    print(paste("Node weight RF:", nodeWeight))
    for (i in 1:10) {
      modelRF = trainRF(Winner ~ ., learn, test, nodeWeight)
      evalDf = rbind(evalDf, list(NodeWeightRF = nodeWeight, CA = modelRF$CA, BrierScore = modelRF$BrierScore))
    }
  }

  evalDf
}
