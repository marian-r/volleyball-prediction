
source("date_time_helpers.R")

read.result = function(fileName) 
{
	filePath = paste("data/results/", fileName, ".txt", sep="")
	read.table(filePath, sep="\t", header=TRUE, stringsAsFactors=FALSE)
}

read.all.results = function()
{
	resultFiles = c("fivb_world_cup_2011", "fivb_world_championship_2010",
			"fivb_world_championship_2014", "fivb_world_league_2010",
			"fivb_world_league_2011", "fivb_world_league_2012",
			"fivb_world_league_2013", "fivb_world_league_2014")
	
	results = data.frame()
	
	for (fileName in resultFiles)
	{
		newResults = read.result(fileName)
		results = rbind(results, newResults)
	}
	
	results
}

prepareResults = function(res)
{
	res = divideColumnsByTeams(res, "Teams", FALSE)
	res = divideColumnsByTeams(res, "Result")
	res = divideColumnsByTeams(res, "Set1")
	res = divideColumnsByTeams(res, "Set2")
	
	res[res$Result_1 > res$Result_2, "Winner"] = "1"
	res[res$Result_1 < res$Result_2, "Winner"] = "2"
	res$Winner = as.factor(res$Winner)
	res$Time = getTimeInMinutes(res$Time)

	res$Date= to.Date(res$Date)

	res[order(res$Date),]
}

divideColumnsByTeams = function(results, columnName, asInt = TRUE)
{
	charVector = as.character(results[,columnName])
	spl = strsplit(charVector , "-")
	splitVector = unlist(spl)

	team1 = splitVector[seq(from=1, to=length(splitVector), by=2)]
	team2 = splitVector[seq(from=2, to=length(splitVector), by=2)]

	columnName1 = paste(columnName, "_1", sep="")
	columnName2 = paste(columnName, "_2", sep="")
	
	if (asInt)
	{
		results[,columnName1] = as.integer(team1)
		results[,columnName2] = as.integer(team2)
	}
	else
	{
		results[,columnName1] = as.factor(team1)
		results[,columnName2] = as.factor(team2)
	}
	results
}
