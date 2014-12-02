
regDf = read.table("data/matches.reg.txt", header=TRUE, stringsAsFactors=FALSE, sep=",")
summary(regDf)

# divide according to date (~ 70:30)
date = as.Date("2013-08-01")
learn = regDf[difftime(date, df$Date) > 0,]
test = regDf[difftime(date, df$Date) <= 0,]

#sel <- 1:(0.6*nrow(mlDf))
#learn = mlDf[sel,]
#test = mlDf[-sel,]

# remove NAs
learn = learn[!is.na(learn$WinRatio_1) & !is.na(learn$WinRatio_2),]
test = test[!is.na(test$WinRatio_1) & !is.na(test$WinRatio_2),]

learn$Sets_Mean_1 = NULL
learn$Sets_Mean_2 = NULL
test$Sets_Mean_1 = NULL
test$Sets_Mean_2 = NULL


print(nrow(learn))
print(nrow(test))
print(summary(learn))
print(summary(test))

sel <- 1:(0.7*nrow(learn))
train = learn[sel,]
validate = learn[-sel,]

print(nrow(train))
print(nrow(validate))




# Linear model

observed = test$Time

lm.model <- lm(Time ~ ., data = learn)
lm.model

predicted <- predict(lm.model, test)
mae(observed, predicted)
rmae(observed, predicted, mean(learn$Time))


lm.model <- lm(Time ~ SetsRankRatio_1 + Points1 + Points2, data = learn)
lm.model

predicted <- predict(lm.model, test)
mae(observed, predicted)
rmae(observed, predicted, mean(learn$Time))

#cbind(observed, predicted)



# Random forests

library(randomForest)

observed = test$Time
rf <- randomForest(Time ~ ., learn)
predicted <- predict(rf, test)
rmae(observed, predicted, mean(learn$Time))

rf <- randomForest(Time ~ SetsRankRatio_1 + Points1 + Points2, learn)
predicted <- predict(rf, test)
rmae(observed, predicted, mean(learn$Time))




library(nnet)

#
# important!!!
# in regression problems use linout = T

observed = validate$Time
evalDf = data.frame(row.names = c("Size", "Decay", "Maxit", "RMAE"))
for (size in 1:5) {
  for (decay in c(0, 0.000001, 0.000001, 0.00001, 0.0001)) {
    for (maxit in c(100, 500, 1000, 2000, 5000, 10000)) {
      print(paste("Size:", size, "Decay:", decay, "Maxit:", maxit))
      N = 5

      for (i in 1:N) {
        modelANN = nnet(Time ~ ., train, size = size, decay = decay, maxit = maxit, linout = T)
        predicted <- predict(modelANN, validate)
        rmaeValue = rmae(observed, predicted, mean(train$Time))
        evalDf = rbind(evalDf, list(Size = size, Decay = decay, Maxit = maxit, RMAE = rmaeValue))
      }
    }
  }
}

evalDf

#set.seed(6789)
observed = test$Time
nn <- nnet(Time ~ ., learn, size = 5, decay = 1e-4, maxit = 10000, linout = T)
predicted <- predict(nn, test)
rmae(observed, predicted, mean(learn$Time))

nn <- nnet(Time ~ SetsRankRatio_1 + Points1 + Points2, learn, size = 5, decay = 1e-4, maxit = 10000, linout = T)
predicted <- predict(nn, test)
rmae(observed, predicted, mean(learn$Time))



library(kknn)

observed = validate$Time
evalDf = data.frame(row.names = c("K", "RMAE"))
for (i in 1:50) {
  knn.model <- kknn(Time ~ ., train, validate, k = i)
  predicted <- fitted(knn.model)
  rmaeValue = rmae(observed, predicted, mean(train$Time))

  evalDf = rbind(evalDf, list(K = i, RMAE = rmaeValue))
}

plot(evalDf)

observed = test$Time
knn.model <- kknn(Time ~ ., learn, test, k = 15)
predicted <- fitted(knn.model)
rmae(observed, predicted, mean(test$Time))

knn.model <- kknn(Time ~ SetsRankRatio_1 + Points1 + Points2, learn, test, k = 15)
predicted <- fitted(knn.model)
rmae(observed, predicted, mean(test$Time))
