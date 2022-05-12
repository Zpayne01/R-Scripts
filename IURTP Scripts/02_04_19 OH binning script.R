file <- file.choose()

csv <- read.csv(file, stringsAsFactors = FALSE)

head(csv)

colnames(csv)

csvOH <- data.frame(csv$Date, csv$DateTime, csv$OH...Interference)
colnames(csvOH) <- c('date', 'datetime', 'OH')

csvOH$date <- as.POSIXct(csvOH$date, format = '%m/%d/%Y')

uniqueDates <- unique(as.Date(csvOH$date))

for (i in 1:length(uniqueDates)) {
  
  exportDate <- csvOH[as.Date(csvOH$date) == uniqueDates[i],]
  dateForName <- gsub('-','_',uniqueDates[i])
  filename <- paste0('C:/Users/zacpayne/Desktop/Working Files/',dateForName,' OH.csv')
  write.csv(exportDate, filename, row.names = FALSE)
  
}