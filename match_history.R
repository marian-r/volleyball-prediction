
source("date_time_helpers.R")


read.history = function(competition, date)
{
	filePath = paste("data/statistics/fivb_", competition, "_head_to_head_", date, ".txt", sep="")
	read.table(filePath, sep=" ", header=TRUE, stringsAsFactors=FALSE, na.strings="-")
}

read.all.history = function()
{
	wlDates = c("03-Jun-2010", "27-May-2011", "16-May-2012", "04-Jun-2013", "23-May-2014")

	#date = "21-Oct-2011"
	historyTable = list()
	#historyTable[[as.character(to.Date(date))]] = read.history("world_cup", date);

	for (date in wlDates)
	{
		historyTable[[as.character(to.Date(date))]] = read.history("world_league", date)
	}

	historyTable
}

addHistoryToResults = function(history, results)
{
	df = results

	latestDates = getLatestDateForResults(names(history), df)
	dates = sort(names(history),decreasing=T)

	for (i in 1:nrow(df))
	{
		row <- df[i,]

		team1 = as.character(row$Team1)
		team2 = as.character(row$Team2)

		index = which(dates == latestDates[i])
		#print(index)

		repeat {
			latestDate = dates[index]

			histDf = history[[as.character(latestDate)]]

			histTeam1row = histDf[histDf$Country == team1,]
			histTeam2row = histDf[histDf$Country == team1,]

			if((nrow(histTeam1row) == 1 & nrow(histTeam2row) == 1) | index >= length(dates)) {
				break
			}
			#print(latestDate)
			index = index + 1
		}

		df[i,"HistoryDate"] = as.character(latestDate)

		#print(latestDate)

		histTeam1row = histDf[histDf$Country == team1,]
		histTeam2row = histDf[histDf$Country == team2,]

		# Total wins

		if (nrow(histTeam1row) == 1) {
			histTeam1row$Country = NULL
			histTeam1row[paste(team1, "_W", sep="")] = NULL
			histTeam1row[paste(team1, "_L", sep="")] = NULL

			team1wins = histTeam1row[seq(from=1, to=length(histTeam1row), by=2)]
			team1losses = histTeam1row[seq(from=2, to=length(histTeam1row), by=2)]

			team1totalWins = sum(team1wins)
			team1totalLosses = sum(team1losses)

			df[i,"Team1_TotalWins"] = team1totalWins
			df[i,"Team1_TotalLosses"] = team1totalLosses

		} else if (nrow(histTeam1row) > 0) {
			print("More than one row")
			print(histTeam1row)
		}

    # Total losses

		if (nrow(histTeam2row) == 1) {
			histTeam2row$Country = NULL
			histTeam2row[paste(team2, "_W", sep="")] = NULL
			histTeam2row[paste(team2, "_L", sep="")] = NULL

			team2wins = histTeam2row[seq(from=1, to=length(histTeam2row), by=2)]
			team2losses = histTeam2row[seq(from=2, to=length(histTeam2row), by=2)]

			team2totalWins = sum(team2wins)
			team2totalLosses = sum(team2losses)

			df[i,"Team2_TotalWins"] = team2totalWins
			df[i,"Team2_TotalLosses"] = team2totalLosses

		} else if (nrow(histTeam2row) > 0) {
			print("More than one row")
			print(histTeam2row)
		}

    # Wins

		colname1 = paste(team2, "_W", sep="")
		colname2 = paste(team1, "_W", sep="")

		team1Wins = histDf[histDf$Country == team1,colname1]
		team2Wins = histDf[histDf$Country == team2,colname2]

		if (length(team1Wins) > 0 & length(team2Wins) > 0) {
			df[i,"Team1Wins"] = as.integer(as.character(team1Wins))
			df[i,"Team2Wins"] = as.integer(as.character(team2Wins))
		}

    # Losses

		colname1 = paste(team2, "_L", sep="")
		colname2 = paste(team1, "_L", sep="")

		team1Losses = histDf[histDf$Country == team1,colname1]
		team2Losses = histDf[histDf$Country == team2,colname2]

		if (length(team1Losses) > 0 & length(team2Losses) > 0) {
		  df[i,"Team1Losses"] = as.integer(as.character(team1Losses))
		  df[i,"Team2Losses"] = as.integer(as.character(team2Losses))
		}
	}

	df
}
