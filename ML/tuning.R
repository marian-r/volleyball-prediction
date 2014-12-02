
# ANN
source("ML/ann.R")
evalANN = tuneANN(train, validate)

# Size matters, the lower, the better
boxplot(CA ~ Size, data=evalANN)
boxplot(BrierScore ~ Size, data=evalANN)

# Decay does not matter
boxplot(CA ~ Decay, data=evalANN)
boxplot(BrierScore ~ Decay, data=evalANN)

# Maxit does not matter
boxplot(CA ~ Maxit, data=evalANN)
boxplot(BrierScore ~ Maxit, data=evalANN)


evalANN2 = tuneANNwithoutSize(train, validate)

# Decay does not matter
boxplot(CA ~ Decay, data=evalANN2)
boxplot(BrierScore ~ Decay, data=evalANN2)

# Maxit does not matter
boxplot(CA ~ Maxit, data=evalANN2)
boxplot(BrierScore ~ Maxit, data=evalANN2)

# Best ANN model
modelANN = trainANN(Winner ~ ., train, validate, 1, 0, 100)



# TREE
source("ML/decision_tree.R")
evalDT = tuneTree(train, validate)

# NodeWeight matters, the lower, the better, but danger of over-fitting
boxplot(CA ~ NodeWeight, data=evalDT)
boxplot(BrierScore ~ NodeWeight, data=evalDT)

# InstanceWeight seems that it does not matter
boxplot(CA ~ InstanceWeight, data=evalDT)
boxplot(BrierScore ~ InstanceWeight, data=evalDT)

# Best tree model
modelDT = trainTree(Winner ~ ., train, validate, 0.05, 6)
plot(modelDT$model, train)



# Bayes
source("ML/bayes.R")
modelBayes = trainBayes(Winner ~ ., train, validate)



# KNN
source("ML/knn.R")
evalKNN = tuneKNN(train, validate)

plot(CA ~ kInNN, data=evalKNN)
plot(BrierScore ~ kInNN, data=evalKNN)

# Best KNN model
modelKNN = trainKNN(Winner ~ ., train, validate, 15)



# Random forests
source("ML/random_forest.R")
evalRF = tuneRF(train, validate)

# NodeWeight matters to some extent, as the algorithm is random
boxplot(CA ~ NodeWeightRF, data=evalRF)
boxplot(BrierScore ~ NodeWeightRF, data=evalRF)

# Best Random Forest model
# With or without NAs
#modelRF = trainRF(Winner ~ ., train, validate, 3)
modelRF = trainRF(Winner ~ ., train, validate, 4)
# Without NAs
modelRF = trainRF(Winner ~ ., train, validate, 7)
plot(modelRF$model, train)
