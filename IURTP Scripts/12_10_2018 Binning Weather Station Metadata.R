library(lubridate)

## Choose the weather station file (IURTP/Weather Station/Complete during measurement period)

file <- file.choose()
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

csv <- csv[-13]
csv <- csv[-12]
csv <- csv[-3]
csv <- csv[-2]

csv$datetime <- as.POSIXct(csv$datetime, format = "%m/%d/%Y %H:%M")
csv$datetime <- floor_date(csv$datetime, unit = "15 minutes")

binnedcsv <- aggregate(.~datetime, csv, mean)

filename <- 'C:/Users/zacpayne/Desktop/Working Files/Binned (15-min) Weather Station Data.csv'

write.csv(binnedcsv, file = filename, row.names = FALSE)