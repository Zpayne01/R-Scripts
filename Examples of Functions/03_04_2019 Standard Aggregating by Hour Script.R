library(lubridate)

## First choose the file that you are going to aggregate

file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

colnames(csv)
head(csv)

## Choose the column with the time for datetime, and the column with the species you want to average for average

DatetimeColName <- 'Excel.Dtime'
colnames(csv)[which(colnames(csv) == DatetimeColName)] <- 'datetime'
head(csv$datetime)

##colnames(csv)[which(colnames(csv) == 'OH..molecules.cm3.')] <- 'ave'

## Change the name of the datetime column and then change the format to match

csv$datetime <- as.POSIXlt(csv$datetime, format = "%m/%d/%Y %H:%M")

## Change how you want to aggregate the data (in this example its by hour)

csv$rounddate <- round_date(csv$datetime, unit = '1 hour')
csv$rounddatehour <- csv$rounddate$hour

csv$rounddate <- as.POSIXct(csv$rounddate)

csv$datetime <- NULL
csv$rounddate <- NULL

colnames(csv)

## Choose columns that you want to take stats of

agg <- aggregate(HO2 ~ rounddate, csv, mean)
obs <- aggregate(. ~ rounddate, csv, length)
stdev <- aggregate(OH ~ rounddate, csv, sd)

## Change the file name that you want to export to use 'clipboard' to add it to the clipboard

# filename <- 'C:/Users/zacpayne/Desktop/Working Files/Ozone.csv'

write.table(agg, file = 'clipboard', sep = '\t', row.names = FALSE)
write.table(stdev , file = 'clipboard', sep = '\t', row.names = FALSE)

write.csv(agg, file = 'C:/Users/zacpayne/Desktop/agg.csv', row.names = FALSE)

export <- cbind(agg, obs$caps_NO2_vNov_ppb, stdev$caps_NO2_vNov_ppb)

write.table(export, file = 'clipboard', sep = '\t', row.names = FALSE)
