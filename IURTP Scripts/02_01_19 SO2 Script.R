library(lubridate)

f <- list.files('D:/Surface 3/Main Folder/One Drive/IURTP/12_06_18 Modeling Work/Modeling Data/Extra Days/Box Files/SO2')

combinedtxt <- data.frame(TimeS = numeric(), SO2r = numeric())

for (i in 1:length(f)) {
  
  txt <- read.delim(f[i], stringsAsFactors = FALSE)
  combinedtxt <- rbind(combinedtxt, txt)
  head(combinedtxt)
}

head(combinedtxt)
tail(combinedtxt)

combinedtxt$TimeS <- as.POSIXlt(combinedtxt$TimeS, format = '%m/%d/%Y %H:%M:%S')

combinedtxt$datetime <- round_date(combinedtxt$TimeS, unit = '1 hour')

combinedtxt$datetime <- as.POSIXct(combinedtxt$datetime, format = '%Y-%m-%d')

aveSo2 <- aggregate(SO2r ~ datetime, combinedtxt, mean)

filename = 'C:/Users/zacpayne/Desktop/Working Files/1-hr binned SO2.csv'
write.csv(aveSo2, file = filename, row.names = FALSE)

uniqueDates <- unique(as.Date(aveSo2$datetime))

for (i in 1:length(uniqueDates)) {
  exportDate <- aveSo2[as.Date(aveSo2$datetime) == uniqueDates[i],]
  exportDate[order(exportDate$datetime),]
  dateForName <- gsub('-','_',uniqueDates[i])
  filename <- paste0('C:/Users/zacpayne/Desktop/Working Files/',dateForName,' SO2.csv')
  write.csv(exportDate, filename, row.names = FALSE)
}
