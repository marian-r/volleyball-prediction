wrapper <- function(dataset, className, classModel = "tree", folds = 10)
{
	require(ipred)
	require(CORElearn)

	mymodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
	mypredict <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}

	formula <- paste(className," ~ ", sep = "")
	candidates <- names(dataset)[-match(className,names(dataset))]

	global.res <- 1.0
	global.formula <- formula

	while(length(candidates))
	{
		best.att <- 1
		best.res <- 1.0

		for (i in 1:length(candidates))
		{
			tmp.formula <- paste(formula, candidates[i], sep = "")
			cat("formula to evaluate:", tmp.formula, "...\n")
			flush.console()

			res <- errorest(as.formula(tmp.formula), data = dataset, model = mymodel, predict = mypredict, target.model = classModel, est.para=control.errorest(k=folds))
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

	cat("best model: estimated error = ", global.res,", selected feature subset = ", global.formula, "\n")
}
