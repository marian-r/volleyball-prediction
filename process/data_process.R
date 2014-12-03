
WD <- getwd()
if (!is.null(WD)) {
  setwd(WD)
}

source("date_time_helpers.R")
source("process/results.R")
source("process/rankings.R")
source("process/match_history.R")
source("process/statistics.R")

processData = function(class = TRUE)
{
  printTime("Before")
  res <- read.all.results()
  printTime("Read results")
  res = prepareResults(res)
  printTime("Prepare results")

  df = res

  df$Team1 = res$Teams_1
  df$Team2 = res$Teams_2
  df$Teams_1 = NULL
  df$Teams_2 = NULL

  printTime("DF prepare")

  rank = read.all.rankings()
  printTime("Read rankings")

  history = read.all.history()
  printTime("Read history")

  stat = read.all.statistics()
  printTime("Read statistics")

  df = addRankingsToResults(rank, df)
  printTime("Add rankings")
  df = addHistoryToResults(history, df)
  printTime("Add history")
  df = addStatisticsToResults(stat, df)
  printTime("Add statistics")

  df$Team1_Country = NULL
  df$Team2_Country = NULL

  df
}

printTime = function(title)
{
  print (title)
  print(Sys.time())
}
