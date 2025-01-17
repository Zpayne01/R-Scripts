file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

head(csv)

csv$date.middle.sampling <- as.POSIXlt(csv$date.middle.sampling, format = '%m/%d/%Y %H:%M')
plot(csv$date.middle.sampling, csv$isopr�ne, type = 'l')

csv <- csv[1:208,]

csv$date.middle.sampling <- round_date(csv$date.middle.sampling, unit = '1 hour')
isocount <- aggregate(isopr�ne~date.middle.sampling$hour, csv, length)
isosd <- aggregate(isopr�ne~date.middle.sampling$hour, csv, sd)

subset(csv, csv$date.middle.sampling$hour == 9 | csv$date.middle.sampling$hour == 10 | csv$date.middle.sampling$hour == 11)

csvSunrise <- csv[csv$date.middle.sampling$hour %in% c(9,10,11),]
csvAfternoon <- csv[csv$date.middle.sampling$hour %in% c(12,13,14,15,16),]
csvMorning <- csv[csv$date.middle.sampling$hour %in% c(5,6,7,8),]
csvEarlyMorning <- csv[csv$date.middle.sampling$hour %in% c(1,2,3,4),]
csvEvening <- csv[csv$date.middle.sampling$hour %in% c(21,22,23),]

csvSunriseIso <- aggregate(isopr�ne~as.Date(date.middle.sampling), csvSunrise, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))
csvAfternoonIso <- aggregate(isopr�ne~as.Date(date.middle.sampling), csvAfternoon, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))
csvMorningIso <- aggregate(isopr�ne~as.Date(date.middle.sampling), csvMorning, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))
csvEarlyMorningIso <- aggregate(isopr�ne~as.Date(date.middle.sampling), csvEarlyMorning, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))
csvEveningIso <- aggregate(isopr�ne~as.Date(date.middle.sampling), csvEvening, FUN = function (x) c(mn = mean(x), obs = length(x), stddev = sd(x)))

ratioEMorAft <- c(mean(csvEarlyMorningIso$isopr�ne[,1]/csvAfternoonIso$isopr�ne[,1]), sd(csvEarlyMorningIso$isopr�ne[,1]/csvAfternoonIso$isopr�ne[,1]))
ratioEMorMor <- c(mean(csvEarlyMorningIso$isopr�ne[,1]/csvMorningIso$isopr�ne[,1]), sd(csvEarlyMorningIso$isopr�ne[,1]/csvMorningIso$isopr�ne[,1]))
ratioMorAft <- c(mean(csvMorningIso$isopr�ne[,1]/csvAfternoonIso$isopr�ne[,1]), sd(csvMorningIso$isopr�ne[,1]/csvAfternoonIso$isopr�ne[,1]))
ratioEveAft <- c(mean(csvEveningIso$isopr�ne[,1]/csvAfternoonIso$isopr�ne[,1]), sd(csvEveningIso$isopr�ne[,1]/csvAfternoonIso$isopr�ne[,1]))

plot(csvAfternoon$date.middle.sampling, csvAfternoon$isopr�ne, type = 'p')
