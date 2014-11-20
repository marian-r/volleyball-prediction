
source("date_time_helpers.R")


read.ranking = function(date) 
{
	filePath = paste("data/statistics/", "fivb_world_ranking_", date, ".txt", sep="")
	read.table(filePath, sep="\t", header=TRUE, stringsAsFactors=FALSE)
}

read.all.rankings = function()
{
	dates = c("15-Jan-2010", "27-Jul-2010", "15-Jan-2011", "02-Oct-2011",
			"04-Jan-2012", "23-Jan-2013", "04-Jan-2014", "21-Jul-2014")
	
	rankingsTable = c()
	
	for (date in dates)
	{
		ranking = read.ranking(date)
		ranking$Date = to.Date(date)
		rankingsTable = rbind(rankingsTable, ranking)
	}
	
	rankingsTable
}

addRankingsToResults = function(rankings, results)
{
	rankCountries = rankings$Country
	rankDates = rankings$Date
	dateLevels = levels(as.factor(rankDates))
	latestDates = getLatestDateForResults(dateLevels, results)
	
	results$RankDate = latestDates

	for (i in 1:nrow(results))
	{
		row <- results[i,]
		latestDate = as.character(latestDates[i])

		date = rankDates == latestDate
		country1 = rankCountries == row$Team1
		country2 = rankCountries == row$Team2

		ranking1 = rankings[date & country1,]
		ranking2 = rankings[date & country2,]
		
		results[i,"Rank1"] = ranking1$Rank
		results[i,"Rank2"] = ranking2$Rank
		
		results[i,"Points1"] = ranking1$Points
		results[i,"Points2"] = ranking2$Points
	}

	#results$Rank1 = as.integer(results$Rank1)
	#results$Rank2 = as.integer(results$Rank2)
	
	results
}
