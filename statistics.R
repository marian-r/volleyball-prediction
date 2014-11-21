
read.statistics = function(date) 
{
	filePath = paste("data/statistics/fivb_world_league_statistics_", date, ".txt", sep="")
	read.table(filePath, sep=" ", header=TRUE, stringsAsFactors=FALSE)
}

read.all.statistics = function()
{
	wlDates = c("03-Jun-2010", "27-May-2011", "16-May-2012", "04-Jun-2013")
	
	statisticsTable = list()
	
	for (date in wlDates)
	{
		statistics = read.statistics(date)
		
		statisticsTable[[as.character(to.Date(date))]] = statistics
	}
	
	statisticsTable
}

addStatisticsToResults = function(statisticsTable, results)
{
	statDf = data.frame()
	for (i in 1:nrow(results))
	{
		row = results[i,]
		stat1 = getStatisticsForTeam(statisticsTable, as.character(row$Team1), row$Date)
		stat2 = getStatisticsForTeam(statisticsTable, as.character(row$Team2), row$Date)
		
		colNames = names(stat1)
		names(stat1) = paste("Team1_", colNames, sep="")
		names(stat2) = paste("Team2_", colNames, sep="")

		statRow = cbind(stat1, stat2)
		#print(statRow)
		statDf = rbind(statDf, statRow)	
	}

	cbind(results, statDf)
}

getStatisticsForTeam = function(statisticsTable, team, date)
{
	dates = sort(names(statisticsTable),decreasing=T)
	
	index = 1
		
	repeat {
		latestDate = dates[index]
		#print(latestDate)
		if (difftime(date, latestDate) > 0) {
			statDf = statisticsTable[[as.character(latestDate)]]
			statRow = statDf[statDf$Country == team,]
			
			if(nrow(statRow) == 1) {
				break
			}
		}
		
		if (index >= length(dates)) {
			colNames = names(statisticsTable[[as.character(latestDate)]])
			len = length(statisticsTable[[as.character(latestDate)]])
			emptyRow = data.frame(matrix(ncol=len))
			names(emptyRow) = colNames
			return(emptyRow)
		}
		
		
		index = index + 1
	}
	
	statRow
}