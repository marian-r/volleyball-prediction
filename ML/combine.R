
library(CORElearn)
source("ML/functions.R")

# voting

# the class with the most votes wins
voting <- function(predictions)
{
  res <- vector()

  for (i in 1 : nrow(predictions))
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }

  res
}

combine3 = function(modelName, model1, model2, model3, train, validate)
{
  print(modelName)

  observed = validate$Winner
  obsMat <- model.matrix(~Winner-1, validate)

  # Voting
  pred = data.frame(model1$predicted, model2$predicted, model3$predicted)
  predicted <- voting(pred)

  vca = CA(observed, predicted)

  print(vca)
  print(table(observed, predicted))

  #Weighted voting
  pred.prob <- (model1$matrix + model2$matrix + model3$matrix) / 3
  predicted <- levels(train$Winner)[apply(pred.prob, 1, which.max)]

  wvca = CA(observed, predicted)
  wvbs = brier.score(obsMat, pred.prob)

  print(wvca)
  print(wvbs)
  print(table(observed, predicted))

  list(Models = modelName, VotingCA = vca, WeightedVotingCA = wvca, WeightedVotingBS = wvbs)
}

tune3 = function(models, modelNames, train, validate)
{
  evalDf = data.frame(row.names = c("Models", "VotingCA", "WeightedVotingCA", "WeightedVotingBS"))

  for (i in 1:length(models)) {
    model1 = models[[i]]

    for (j in 1:length(models)) {

      if (j != i) {
        model2 = models[[j]]

        for (k in 1:length(models)) {

          if (k != i & k != j) {
            model3 = models[[k]]

            modelName = paste(modelNames[i], modelNames[j], modelNames[k])
            combinedModel = combine3(modelName, model1, model2, model3, train, validate)

            evalDf = rbind(evalDf, combinedModel)
            evalDf$Models = as.character(evalDf$Models)
          }
        }
      }
    }
  }

  evalDf
}


observed = validate$Winner
obsMat <- model.matrix(~Winner-1, validate)

modelDT = trainTree(Winner ~ ., train, validate, 0.05, 6)
modelANN = trainANN(Winner ~ ., train, validate, 1, 0, 100)
modelNB <- trainBayes(Winner ~ ., train, validate)
modelKNN <- trainKNN(Winner ~ ., train, validate, 15)
modelRF = trainRF(Winner ~ ., train, validate, 4)

models = list(modelDT, modelANN, modelNB, modelKNN, modelRF)
modelNames = c("DT", "ANN", "NB", "KNN", "RF")
evalDf = tune3(models, modelNames, train, validate)
evalDf



# all 5 models

pred = data.frame(modelDT$predicted, modelANN$predicted, modelNB$predicted, modelKNN$predicted, modelRF$predicted)
predicted <- voting(pred)
#predicted
CA(observed, predicted)
table(observed, predicted)


pred.prob <- (modelDT$matrix + modelANN$matrix + modelNB$matrix + modelKNN$matrix + modelRF$matrix) / 5
#pred.prob

predicted <- levels(train$Winner)[apply(pred.prob, 1, which.max)]
CA(observed, predicted)
brier.score(obsMat, pred.prob)
table(observed, predicted)



# best 3 models
combine3("ANN KNN RF", modelANN, modelKNN, modelRF, train, validate)


modelANN$CA
modelRF$CA
modelANN$BrierScore
modelRF$BrierScore



# Boosting

library(rpart)
library(ada)

bm <- ada(Winner ~ ., train)
predicted <- predict(bm, validate)
CA(observed, predicted)
