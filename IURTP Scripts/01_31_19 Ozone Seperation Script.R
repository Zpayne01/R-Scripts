library(lubridate)
library(dplyr)

file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

head(csv)

csv$datetime <- as.POSIXlt(csv$datetime, format = '%m/%d/%Y %H:%M')

head(csv)

csv$datetime <- round_date(csv$datetime, unit = '1 hour')
csv$datetime <- as.POSIXct(csv$datetime, format = '%Y-%m-%d %H:%M:%S')

head(csv)

average <- aggregate(ave ~ datetime, csv, mean)
stddev <- aggregate(ave ~ datetime, csv, sd)

export <- data.frame(average, stddev$ave)
colnames(export) <- c('datetime', 'average_ppb', 'stddev_ppb')

csv$date <- as.Date(csv$datetime)

uniqueDates <- unique(csv$date)

for (i in 1:length(uniqueDates)) {
  
  exportDate <- csv[csv$date == uniqueDates[i],]
  dateForName <- gsub('-','_',uniqueDates[i])
  filename <- paste0('C:/Users/zacpayne/Desktop/Working Files/',dateForName,' O3.csv')
  write.csv(exportDate, filename, row.names = FALSE)
  
}