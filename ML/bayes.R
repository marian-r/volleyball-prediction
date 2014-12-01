
# Naive Bayes classifier
library(CORElearn)
source("ML/functions.R")

trainBayes = function(attrModel, learn, test)
{
  print("---------- BAYES -------------")

  # CA and Brier Score calculation

  observed <- test$Winner
  obsMat <- model.matrix(~Winner-1, test)

  mlModel <- CoreModel(attrModel, data = learn, model = "bayes")

  predicted <- predict(mlModel, test, type="class")
  predMat <- predict(mlModel, test, type = "prob")

  ca = CA(observed, predicted)
  bs = brier.score(obsMat, predMat)

  print(ca)
  print(bs)
  print(table(observed, predicted))

  list(model = mlModel, predicted = predicted, matrix = predMat, CA = ca, BrierScore = bs)
}
