
getLatestDateForResults = function(dates, results)
{
	latestDates = rep(dates[1], length(results$Date))
		
	for (date in dates)
	{
		isInPast = difftime(results$Date, date) > 0
		isMoreRecent = difftime(date, latestDates) > 0

		latestDates[isInPast & isMoreRecent] = date
	}

	latestDates
}

to.Date = function(date)
{
	as.Date(date, format="%d-%b-%Y")
}

getTimeInMinutes = function(time)
{
	charVector = as.character(time)
	spl = strsplit(charVector, ":")
	splitVector = unlist(spl)

	hours = as.integer(splitVector[seq(from=1, to=length(splitVector), by=2)])
	minutes = as.integer(splitVector[seq(from=2, to=length(splitVector), by=2)])

	total = hours * 60 + minutes
	total
}
