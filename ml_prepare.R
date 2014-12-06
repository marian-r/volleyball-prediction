
prepareForClassification = function(tempDf, knownSets = 0)
{
  mlDf = data.frame(tempDf$Winner)
  names(mlDf) = c("Winner")

  if (knownSets >= 1) {
    mlDf$Set1_1 = tempDf$Set1_1
    mlDf$Set1_2 = tempDf$Set1_2
  }
  if (knownSets >= 2) {
    mlDf$Set2_1 = tempDf$Set2_1
    mlDf$Set2_2 = tempDf$Set2_2
  }

  mlDf$Points1 = tempDf$Points1
  mlDf$Points2 = tempDf$Points2

  for (i in 1:nrow(tempDf)) {
    print(paste("------", i, "------"))

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
  }

  mlDf$WinRatio_1 = tempDf$WinRatio_1
  mlDf$WinRatio_2 = tempDf$WinRatio_2

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

  mlDf
}

prepareForRegression = function(tempDf)
{
  mlDf = data.frame(tempDf$Time)
  names(mlDf) = c("Time")

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
      tempDf[i,"Sets_1"] = mean(pastNmatches1$Result_1 + pastNmatches1$Result_2)
      tempDf[i,"MatchPoints_1"] = mean(pastNmatches1$Points_1 + pastNmatches1$Points_2)
      tempDf[i,"SetsRankRatio_1"] = mean(pastNmatches1$ResultPoints_RankingRatio)
      tempDf[i,"ResultPM_Cum_1"] = sum(pastNmatches1$ResultPM)
      tempDf[i,"ResultPM_Mean_1"] = mean(pastNmatches1$ResultPM)

      tempDf[i,"ResultPtsPM_Cum_1"] = sum(pastNmatches1$ResultPointsPM)
      tempDf[i,"ResultPtsPM_Mean_1"] = mean(pastNmatches1$ResultPointsPM)
    }

    if (length(pastNmatches2) > 0) {
      tempDf[i,"Sets_2"] = mean(pastNmatches2$Result_1 + pastNmatches2$Result_2)
      tempDf[i,"MatchPoints_2"] = mean(pastNmatches2$Points_1 + pastNmatches2$Points_2)
      tempDf[i,"SetsRankRatio_2"] = mean(pastNmatches2$ResultPoints_RankingRatio)
      tempDf[i,"ResultPM_Cum_2"] = sum(pastNmatches2$ResultPM)
      tempDf[i,"ResultPM_Mean_2"] = mean(pastNmatches2$ResultPM)

      tempDf[i,"ResultPtsPM_Cum_2"] = sum(pastNmatches2$ResultPointsPM)
      tempDf[i,"ResultPtsPM_Mean_2"] = mean(pastNmatches2$ResultPointsPM)
    }

    print(paste("------", i, "------"))

  }

  mlDf$WinRatio_1 = tempDf$WinRatio_1
  mlDf$WinRatio_2 = tempDf$WinRatio_2

  #mlDf$ResultPM_Cum_1 = tempDf$ResultPM_Cum_1
  mlDf$ResultPM_Mean_1 = tempDf$ResultPM_Mean_1
  #mlDf$ResultPM_Cum_2 = tempDf$ResultPM_Cum_2
  mlDf$ResultPM_Mean_2 = tempDf$ResultPM_Mean_2

  #mlDf$ResultPtsPM_Cum_1 = tempDf$ResultPtsPM_Cum_1
  mlDf$ResultPtsPM_Mean_1 = tempDf$ResultPtsPM_Mean_1
  #mlDf$ResultPtsPM_Cum_2 = tempDf$ResultPtsPM_Cum_2
  mlDf$ResultPtsPM_Mean_2 = tempDf$ResultPtsPM_Mean_2

#   mlDf$Team1_Results_3_0_Ratio = tempDf$Team1_Results_3_0 / tempDf$Team1_Matches_Played
#   mlDf$Team1_Results_3_1_Ratio = tempDf$Team1_Results_3_1 / tempDf$Team1_Matches_Played
#   mlDf$Team1_Results_3_2_Ratio = tempDf$Team1_Results_3_2 / tempDf$Team1_Matches_Played
#   mlDf$Team1_Results_2_3_Ratio = tempDf$Team1_Results_2_3 / tempDf$Team1_Matches_Played
#   mlDf$Team1_Results_1_3_Ratio = tempDf$Team1_Results_1_3 / tempDf$Team1_Matches_Played
#   mlDf$Team1_Results_0_3_Ratio = tempDf$Team1_Results_0_3 / tempDf$Team1_Matches_Played

#   mlDf$Team2_Results_3_0_Ratio = tempDf$Team2_Results_3_0 / tempDf$Team2_Matches_Played
#   mlDf$Team2_Results_3_1_Ratio = tempDf$Team2_Results_3_1 / tempDf$Team2_Matches_Played
#   mlDf$Team2_Results_3_2_Ratio = tempDf$Team2_Results_3_2 / tempDf$Team2_Matches_Played
#   mlDf$Team2_Results_2_3_Ratio = tempDf$Team2_Results_2_3 / tempDf$Team2_Matches_Played
#   mlDf$Team2_Results_1_3_Ratio = tempDf$Team2_Results_1_3 / tempDf$Team2_Matches_Played
#   mlDf$Team2_Results_0_3_Ratio = tempDf$Team2_Results_0_3 / tempDf$Team2_Matches_Played

  mlDf$Sets_1 = tempDf$Sets_1
  mlDf$Sets_2 = tempDf$Sets_2
  mlDf$MatchPoints_1 = tempDf$MatchPoints_1
  mlDf$MatchPoints_2 = tempDf$MatchPoints_2
  mlDf$SetsRankRatio_1 = tempDf$SetsRankRatio_1
  mlDf$SetsRankRatio_2 = tempDf$SetsRankRatio_2

  mlDf$Sets_Mean_1 = tempDf$Team1_Sets_Played / tempDf$Team1_Matches_Played
  mlDf$Sets_Mean_2 = tempDf$Team2_Sets_Played / tempDf$Team2_Matches_Played

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

  df[isTeam1,"ResultPoints_RankingRatio"] = (whereTeam1$Points_1 + whereTeam1$Result_2) / whereTeam1$Points2
  df[isTeam2,"ResultPoints_RankingRatio"] = (whereTeam2$Points_1 + whereTeam2$Result_2) / whereTeam2$Points1

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
