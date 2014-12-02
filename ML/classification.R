
source("ML/ann.R")
source("ML/decision_tree.R")
source("ML/bayes.R")
source("ML/knn.R")
source("ML/random_forest.R")


mlDf = read.table("data/matches.class.txt", header=TRUE, stringsAsFactors=FALSE, sep=",")
mlDf$Winner = as.factor(mlDf$Winner)
summary(mlDf)

# divide according to date (~ 70:30)
date = as.Date("2013-08-01")
learn = mlDf[difftime(date, df$Date) > 0,]
test = mlDf[difftime(date, df$Date) <= 0,]

#sel <- 1:(0.6*nrow(mlDf))
#learn = mlDf[sel,]
#test = mlDf[-sel,]

# remove NAs
learn = learn[!is.na(learn$WinRatio_1) & !is.na(learn$WinRatio_2),]
test = test[!is.na(test$WinRatio_1) & !is.na(test$WinRatio_2),]

print(nrow(learn))
print(nrow(test))
print(summary(learn))
print(summary(test))

# Divide learning set further (70:30)
sel <- 1:(0.7*nrow(learn))
train = learn[sel,]
validate = learn[-sel,]

print(nrow(train))
print(nrow(validate))

source("ML/combine.R")

# Majority classifier
majority(learn, test)

# Best model from each type
modelANN = trainANN(Winner ~ ., learn, test, 1, 0, 100)
modelDT = trainTree(Winner ~ ., learn, test, 0.05, 6)
modelNB = trainBayes(Winner ~ ., learn, test)
modelKNN = trainKNN(Winner ~ ., learn, test, 15)
modelRF = trainRF(Winner ~ ., learn, test, 4)

# Best combined model
modelCombined = combine3("ANN KNN RF", modelANN, modelKNN, modelRF, learn, test)
