
#!!! Not final !!!



library(rpart)

rt.model <- rpart(Time ~ ., train)
predicted <- predict(rt.model, validate)
rmae(observed, predicted, mean(train$Time))

plot(rt.model);text(rt.model, pretty = 0)


# parameters that control aspects of the rpart fit
rpart.control()

# build a tree with optional parameters for controlling tree growth (eg maxdepth)
rt.model <- rpart(Time ~ ., train, maxdepth = 5)
plot(rt.model);text(rt.model, pretty = 0)

# the minsplit parameter determines the minimum number of observations that must
# exist in a node in order for a split to be attempted
rt.model <- rpart(Time ~ ., train, minsplit = 3)
plot(rt.model);text(rt.model, pretty = 0)

# cp parameter controls a compomise between predictive accuracy and tree size
rt.model <- rpart(Time ~ ., train, cp = 0)
plot(rt.model);text(rt.model, pretty = 0)

# a table of optimal prunings based on a complexity parameter
printcp(rt.model)

# prune the tree using the complexity parameter associated with minimum error (eg xerror)
rt.model2 <- prune(rt.model, cp = 0.02)
plot(rt.model2)
predicted <- predict(rt.model2, validate)
rmae(observed, predicted, mean(train$Time))




library(CORElearn)

# modelTypeReg
# type: integer, default value: 1, value range: 1, 8
# type of models used in regression tree leaves (1=mean predicted value, 2=median predicted value, 3=linear by MSE, 4=linear by MDL, 5=linear as in M5, 6=kNN, 7=Gaussian kernel regression, 8=locally weighted linear regression).

rt <- CoreModel(Time ~ ., data=train, model="regTree", modelTypeReg = 1)
plot(rt, train)
predicted <- predict(rt, validate)
mae(observed, predicted)
rmae(observed, predicted, mean(train$Time))
mse(observed, predicted)
rmse(observed, predicted, mean(train$Time))

cbind(observed, predicted)

str(train)

modelEval(rt, validate$Time, predicted)
