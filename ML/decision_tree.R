
library(CORElearn)
source("ML/functions.R")

trainTree = function(attrModel, learn, test, minInstanceWeight = 0.05, minNodeWeightTree = 5)
{
  print("---------- TREE -------------")

  # CA and Brier Score calculation

  observed <- test$Winner
  obsMat <- model.matrix(~Winner-1, test)

  mlModel <- CoreModel(attrModel, data = learn, model = "tree", minInstanceWeight = minInstanceWeight, minNodeWeightTree = minNodeWeightTree)
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

tuneTree = function(learn, test)
{
  evalDf = data.frame(row.names = c("NodeWeight", "InstaneWeight", "CA", "BrierScore"))
  for (nodeWeight in 1:25) {
    for (instWeight in c(0.01, 0.05, 0.10, 0.20, 0.30)) {
      print(paste("Node weight:", nodeWeight, "Instance weight:", instWeight))

      modelTree = trainTree(Winner ~ ., learn, test, instWeight, nodeWeight)
      evalDf = rbind(evalDf, list(NodeWeight = nodeWeight, InstanceWeight = instWeight, CA = modelTree$CA, BrierScore = modelTree$BrierScore))

    }
  }

  evalDf
}
