file <- file.choose()
csv <- read.csv(file, stringsAsFactors = TRUE)

library(lubridate)

colnames(csv) <- c('datetime', 'airtemp', 'rh', 'soiltemp', 'probe', 'saturation')

csv$datetime <- strptime(csv$datetime, format = "%m/%d/%Y %H:%M")

head(csv)

## Floor by every 15 minutes

csv$datetime <- floor_date(csv$datetime, unit = '15 minutes')
csv$datetime <- as.POSIXct(csv$datetime, format = '%Y-%m-%d %H:%M')

## Make numbers numerics instead of factors

csv$saturation <- as.numeric(as.character(csv$saturation))
csv$soiltemp <- as.numeric(as.character(csv$soiltemp))
csv$rh <- as.numeric(as.character(csv$rh))
csv$airtemp <- as.numeric(as.character(csv$airtmep))

## Aggregate based on floored dates

saturation <- aggregate(saturation ~ datetime, csv, mean)
rh <- aggregate(rh ~ datetime, csv, mean)
airtemp <- aggregate(airtemp ~ datetime, csv, mean)
soiltemp <- aggregate(soiltemp ~ datetime, csv, mean)

filename1 = 'C:/Users/Zachary/Desktop/airtemp.csv'
filename2 = 'C:/Users/Zachary/Desktop/rh.csv'
filename3 = 'C:/Users/Zachary/Desktop/saturation.csv'
filename4 = 'C:/Users/Zachary/Desktop/soiltemp.csv'

write.csv(airtemp, file = filename1, row.names = FALSE)
write.csv(rh, file = filename2, row.names = FALSE)
write.csv(saturation, file = filename3, row.names = FALSE)
write.csv(soiltemp, file = filename4, row.names = FALSE)
