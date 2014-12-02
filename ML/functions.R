
# classification

# Majority classifier
majority = function(learn, test)
{
  observed <- test$Winner

  majority.class <- names(which.max(table(learn$Winner)))
  print(majority.class)
  print(sum(test$Winner == majority.class) / length(test$Winner))
}

CA <- function(observed, predicted)
{
	t <- table(observed, predicted)

	sum(diag(t)) / sum(t)
}

brier.score <- function(observedMatrix, predictedMatrix)
{
	sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

Sensitivity <- function(observed, predicted, pos.class)
{
	t <- table(observed, predicted)

	t[pos.class, pos.class] / sum(t[pos.class,])
}

Specificity <- function(observed, predicted, pos.class)
{
	t <- table(observed, predicted)
	neg.class <- which(row.names(t) != pos.class)

	t[neg.class, neg.class] / sum(t[neg.class,])
}

scale.data <- function(data)
{
	norm.data <- data

	for (i in 1:ncol(data))
	{
		if (!is.factor(data[,i]))
			norm.data[,i] <- scale(data[,i])
	}

	norm.data
}

# regression

mae <- function(observed, predicted)
{
  mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val)
{
  sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
  mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val)
{
  sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}
