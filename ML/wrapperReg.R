wrapperReg <- function(dataset, regName, folds = 10)
{
	require(ipred)
	require(CORElearn)

	mypredict.regtree <- function(object, newdata) {pred <- predict(object, newdata); destroyModels(object); pred}
	mymodel.regtree <- function(formula, data) {CoreModel(formula, data, model = "regTree")}


	formula <- paste(regName," ~ ", sep = "")
	candidates <- names(dataset)[-match(regName,names(dataset))]

	global.res <- Inf
	global.formula <- formula

	while(length(candidates))
	{
		best.att <- 1
		best.res <- Inf

		for (i in 1:length(candidates))
		{
			tmp.formula <- paste(formula, candidates[i], sep = "")
			cat("evaluating ", tmp.formula, "...\n")
			flush.console()

			res <- errorest(as.formula(tmp.formula), data = dataset, model = mymodel.regtree, predict = mypredict.regtree, est.para=control.errorest(k=folds))
			if (res$error < best.res)
			{
				best.res <- res$error
				best.att <- i
			}
		}

		cat("selected attribute: ", candidates[best.att], "\n")

		flush.console()
		
		if (best.res < global.res)
		{
			global.formula <- paste(formula, candidates[best.att], sep = "")
			global.res <- best.res
		}
		
		formula <- paste(formula, candidates[best.att], " + ", sep = "")
		candidates <- candidates[-best.att]
	}

	cat("best evaluated model: ", global.formula, ", error: ", global.res, "\n")
}
