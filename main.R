
source("process/data_process.R")
source("ml_prepare.R")


# Read data and modify for later use
df = processData()
write.table(df, "data/matches.txt", col.names=T, row.names = F, sep=",")


tempDf = read.table("data/matches.txt", sep=",", header=TRUE, stringsAsFactors=FALSE)
tempDf$Winner = as.factor(tempDf$Winner)


# Create data set for classification
mlDf = prepareForClassification(tempDf)
write.table(mlDf, "data/matches.class.txt", col.names=T, row.names = F, sep=",")
#mlDf = prepareForClassification(tempDf, 1)
#mlDf = prepareForClassification(tempDf, 2)
mlDf = read.table("data/matches.class.txt", sep=",", header=TRUE, stringsAsFactors=FALSE)
mlDf$Winner = as.factor(mlDf$Winner)
summary(mlDf)

# Run classification
source("ML/classification.R")


# Create data set for regression
regDf = prepareForRegression(tempDf)
write.table(regDf, "data/matches.reg.txt", col.names=T, row.names = F, sep=",")

regDf = read.table("data/matches.reg.txt", sep=",", header=TRUE, stringsAsFactors=FALSE)
summary(regDf)

# Run regression
source("ML/regression.R")
