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
