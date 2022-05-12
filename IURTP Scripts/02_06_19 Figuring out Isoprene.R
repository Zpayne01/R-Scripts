file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

head(csv)

csv$date.middle.sampling <- as.POSIXlt(csv$date.middle.sampling, format = '%m/%d/%Y %H:%M')
plot(csv$date.middle.sampling, csv$isoprène, type = 'l')

csv <- csv[1:208,]

csv$date.middle.sampling <- round_date(csv$date.middle.sampling, unit = '1 hour')
isocount <- aggregate(isoprène~date.middle.sampling$hour, csv, length)
isosd <- aggregate(isoprène~date.middle.sampling$hour, csv, sd)

subset(csv, csv$date.middle.sampling$hour == 9 | csv$date.middle.sampling$hour == 10 | csv$date.middle.sampling$hour == 11)

csvSunrise <- csv[csv$date.middle.sampling$hour %in% c(9,10,11),]
csvAfternoon <- csv[csv$date.middle.sampling$hour %in% c(12,13,14,15,16),]
csvMorning <- csv[csv$date.middle.sampling$hour %in% c(5,6,7,8),]
csvEarlyMorning <- csv[csv$date.middle.sampling$hour %in% c(1,2,3,4),]
csvEvening <- csv[csv$date.middle.sampling$hour %in% c(21,22,23),]

csvSunriseIso <- aggregate(isoprène~as.Date(date.middle.sampling), csvSunrise, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))
csvAfternoonIso <- aggregate(isoprène~as.Date(date.middle.sampling), csvAfternoon, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))
csvMorningIso <- aggregate(isoprène~as.Date(date.middle.sampling), csvMorning, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))
csvEarlyMorningIso <- aggregate(isoprène~as.Date(date.middle.sampling), csvEarlyMorning, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))
csvEveningIso <- aggregate(isoprène~as.Date(date.middle.sampling), csvEvening, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))

ratioEMorAft <- c(mean(csvEarlyMorningIso$isoprène[,1]/csvAfternoonIso$isoprène[,1]), sd(csvEarlyMorningIso$isoprène[,1]/csvAfternoonIso$isoprène[,1]))
ratioEMorMor <- c(mean(csvEarlyMorningIso$isoprène[,1]/csvMorningIso$isoprène[,1]), sd(csvEarlyMorningIso$isoprène[,1]/csvMorningIso$isoprène[,1]))
ratioMorAft <- c(mean(csvMorningIso$isoprène[,1]/csvAfternoonIso$isoprène[,1]), sd(csvMorningIso$isoprène[,1]/csvAfternoonIso$isoprène[,1]))
ratioEveAft <- c(mean(csvEveningIso$isoprène[,1]/csvAfternoonIso$isoprène[,1]), sd(csvEveningIso$isoprène[,1]/csvAfternoonIso$isoprène[,1]))

plot(csvAfternoon$date.middle.sampling, csvAfternoon$isoprène, type = 'p')
