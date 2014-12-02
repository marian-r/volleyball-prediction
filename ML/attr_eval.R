
# Attribute estimation



# Classification

sort(attrEval(Winner ~ ., learn, "InfGain"), decreasing = TRUE)
sort(attrEval(Winner ~ ., learn, "Gini"), decreasing = TRUE)
sort(attrEval(Winner ~ ., learn, "GainRatio"), decreasing = TRUE)
sort(attrEval(Winner ~ ., learn, "Relief"), decreasing = TRUE)
sort(attrEval(Winner ~ ., learn, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(Winner ~ ., learn, "MDL"), decreasing = TRUE)

source("ML/wrapper.R")
wrapper(learn, className="Winner", classModel="tree", folds=5)
wrapper(learn, className="Winner", classModel="knn", folds=5)
wrapper(learn, className="Winner", classModel="rf", folds=5)



# Regression

sort(attrEval(Time ~ ., learn, "RReliefFequalK"), decreasing = TRUE)

source("ML/wrapperReg.R")
wrapperReg(train, "Time")
