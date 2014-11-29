
prepareForML= function(tempDf)
{
  mlDf = data.frame(tempDf$Winner)
  names(mlDf) = c("Winner")
  mlDf$Points1 = tempDf$Points1
  mlDf$Points2 = tempDf$Points2

  for (i in 1:nrow(tempDf)) {
    row = tempDf[i,]

    pastN = 3
    pastNmatches1 = getPastGamesForOneTeam(tempDf[1:i-1,], row, row$Team1, pastN)
    pastNmatches2 = getPastGamesForOneTeam(tempDf[1:i-1,], row, row$Team2, pastN)

    if (nrow(pastNmatches1) > 0) {
      tempDf[i,"WinRatio_1"] = sum(pastNmatches1$IsWinner == TRUE) / nrow(pastNmatches1)
    }

    if (nrow(pastNmatches2) > 0) {
      tempDf[i,"WinRatio_2"] = sum(pastNmatches2$IsWinner == TRUE) / nrow(pastNmatches2)
    }

    if (length(pastNmatches1) > 0) {
      tempDf[i,"ResultPM_Cum_1"] = sum(pastNmatches1$ResultPM)
      tempDf[i,"ResultPM_Mean_1"] = mean(pastNmatches1$ResultPM)

      tempDf[i,"ResultPtsPM_Cum_1"] = sum(pastNmatches1$ResultPointsPM)
      tempDf[i,"ResultPtsPM_Mean_1"] = mean(pastNmatches1$ResultPointsPM)

      tempDf[i,"ResultPM_RankRatio_Cum_1"] = sum(pastNmatches1$ResultPM_RankingRatio)
      tempDf[i,"ResultPM_RankRatio_Mean_1"] = mean(pastNmatches1$ResultPM_RankingRatio)
    }

    if (length(pastNmatches2) > 0) {
      tempDf[i,"ResultPM_Cum_2"] = sum(pastNmatches2$ResultPM)
      tempDf[i,"ResultPM_Mean_2"] = mean(pastNmatches2$ResultPM)

      tempDf[i,"ResultPtsPM_Cum_2"] = sum(pastNmatches2$ResultPointsPM)
      tempDf[i,"ResultPtsPM_Mean_2"] = mean(pastNmatches2$ResultPointsPM)

      tempDf[i,"ResultPM_RankRatio_Cum_2"] = sum(pastNmatches2$ResultPM_RankingRatio)
      tempDf[i,"ResultPM_RankRatio_Mean_2"] = mean(pastNmatches2$ResultPM_RankingRatio)
    }

    #   pastMatches = getPastGamesForBothTeams(tempDf, row, 3)
    #   team1Wins = (pastMatches$Team1 == row$Team1 & pastMatches$Winner == "1") | (pastMatches$Team2 == row$Team1 & pastMatches$Winner == "2")
    #   team2Wins = (pastMatches$Team1 == row$Team2 & pastMatches$Winner == "1") | (pastMatches$Team2 == row$Team2 & pastMatches$Winner == "2")
    #
    #   t1len = length(team1Wins[team1Wins == TRUE])
    #   t2len = length(team2Wins[team2Wins == TRUE])
    #   if (t1len == 0 & t2len == 0) {
    #     WinnerRatio = NA
    #   } else {
    #     WinnerRatio = t1len - t2len
    #   }

    print(paste("------", i, "------"))
    #print(team1Wins)
    #print(team2Wins)
    #print(WinnerRatio)
    #print(nrow(pastMatches))

    #tempDf[i,"Winner"] = WinnerRatio

  }

  mlDf$WinRatio_1 = tempDf$WinRatio_1
  mlDf$WinRatio_2 = tempDf$WinRatio_2

  #tmpMlDf = cbind(mlDf, matchdata$WinRatioA)
  #tempDf[17:17,]
  #matchdata[17:17,]
  #tmpMlDf[tmpMlDf$WinRatio_1 != matchdata$WinRatioA,]

  mlDf$ResultPM_Cum_1 = tempDf$ResultPM_Cum_1
  mlDf$ResultPM_Mean_1 = tempDf$ResultPM_Mean_1
  mlDf$ResultPM_Cum_2 = tempDf$ResultPM_Cum_2
  mlDf$ResultPM_Mean_2 = tempDf$ResultPM_Mean_2

  mlDf$ResultPtsPM_Cum_1 = tempDf$ResultPtsPM_Cum_1
  mlDf$ResultPtsPM_Mean_1 = tempDf$ResultPtsPM_Mean_1
  mlDf$ResultPtsPM_Cum_2 = tempDf$ResultPtsPM_Cum_2
  mlDf$ResultPtsPM_Mean_2 = tempDf$ResultPtsPM_Mean_2

  #mlDf$ResultPM_RankRatio_Cum_1 = tempDf$ResultPM_RankRatio_Cum_1
  #mlDf$ResultPM_RankRatio_Mean_1 = tempDf$ResultPM_RankRatio_Mean_1
  #mlDf$ResultPM_RankRatio_Cum_2 = tempDf$ResultPM_RankRatio_Cum_2
  #mlDf$ResultPM_RankRatio_Mean_2 = tempDf$ResultPM_RankRatio_Mean_2

  #hasH2H = !is.na(tempDf$Team1Wins)
  #mlDf[hasH2H,"WinRatio_h2h"] = (tempDf[hasH2H,"Team1Wins"] - tempDf[hasH2H,"Team2Wins"]) / (tempDf[hasH2H,"Team1Wins"] + tempDf[hasH2H,"Team2Wins"])

  mlDf
}

getPastGamesForOneTeam = function(df, row, team, latestN = 3)
{
  isTeam1 = df$Team1 == team
  isTeam2 = df$Team2 == team
  isTeam = isTeam1 | isTeam2
  isInPast = difftime(df$Date, row$Date) < 0

  df = df[isTeam & isInPast,]

  range = nrow(df):max(0, nrow(df)-latestN+1)
  df = df[range,]

  df$IsWinner = (df$Team1 == team & df$Winner == "1") | (df$Team2 == team & df$Winner == "2")

  isTeam1 = df$Team1 == team
  isTeam2 = df$Team2 == team
  whereTeam1 = df[isTeam1,]
  whereTeam2 = df[isTeam2,]

  df[isTeam1,"ResultPM"] = whereTeam1$Result_1 - whereTeam1$Result_2
  df[isTeam2,"ResultPM"] = whereTeam2$Result_2 - whereTeam2$Result_1

  df[isTeam1,"ResultPointsPM"] = whereTeam1$Points_1 - whereTeam1$Points_2
  df[isTeam2,"ResultPointsPM"] = whereTeam2$Points_2 - whereTeam2$Points_1

  df[isTeam1,"ResultPM_RankingRatio"] = (whereTeam1$Points_1 - whereTeam1$Result_2) / whereTeam1$Points2
  df[isTeam2,"ResultPM_RankingRatio"] = (whereTeam2$Result_2 - whereTeam2$Result_1) / whereTeam2$Points1

  df
}

getPastGamesForBothTeams = function(df, row, latestN = 3)
{
  isTeam1 = df$Team1 == row$Team1 | df$Team2 == row$Team1
  isTeam2 = df$Team1 == row$Team2 | df$Team2 == row$Team2
  isInPast = difftime(df$Date, row$Date) < 0

  df = df[isTeam1 & isTeam2 & isInPast,]

  range = nrow(df):max(0, nrow(df)-latestN+1)
  df[range,]
}
