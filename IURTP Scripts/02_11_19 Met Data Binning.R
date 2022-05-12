library(lubridate)

file <- file.choose()
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

csv$Broadband.Flux <- NULL
csv$UV <- NULL
csv$percent_UV <- NULL
csv$percent_Broadband <- NULL
csv$X <- NULL

hold <- as.POSIXlt(csv$datetime, format = "%m/%d/%Y %H:%M")
csv$datetime <- hold

head(csv)

csv$datetime <- round_date(csv$datetime, unit = '1 hour')

aggcsv <- aggregate(csv[2:ncol(csv)], by = list(as.POSIXct(csv$datetime, format = "%Y-%m-%d %H:%M:%S")), FUN = mean, na.rm = TRUE)
names <- colnames(aggcsv)
names[1] <- 'Time'
colnames(aggcsv) <- names

filename <- 'C:/Users/zacpayne/Desktop/Working Files/1-hour binned.csv'
write.csv(aggcsv, file = filename, row.names = FALSE)

aggcsv$Time <- as.POSIXlt(aggcsv$Time, format = "%Y-%m-%d %H:%M:%S")
aggcsv$Date <- as.Date(aggcsv$Time)

uniqueDates <- unique(aggcsv$Date)

for (i in 1:length(uniqueDates)) {
  
  aggcsvDate <- aggcsv$Date == uniqueDates[i]
  aggcsvDate <- aggcsv[aggcsvDate,]
  date <- gsub('-','_',uniqueDates[i])
  filenameDate <- paste0('C:/Users/zacpayne/Desktop/Working Files/',date,' Met Data.csv')
  write.csv(aggcsvDate, filenameDate, row.names = FALSE)
  
}