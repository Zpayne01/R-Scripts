file <- file.choose()
csv <- read.csv(file)

csv$date <- as.Date(as.POSIXct(csv$time, format = '%m/%d/%Y %H:%M:%S'))

uniqueDates <- unique(csv$date)

for (i in 1:length(uniqueDates)) {
  
  exportDate <- csv[csv$date == uniqueDates[i],]
  dateForName <- gsub('-','_',uniqueDates[i])
  filename <- paste0('C:/Users/zacpayne/Desktop/Working Files/',dateForName,' Ambient NOx.csv')
  write.csv(exportDate, filename, row.names = FALSE)
  
}