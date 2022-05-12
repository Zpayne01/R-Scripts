## File for correcting NOx data based of Absolute Humidity 

AHfile <- file.choose()
ambfile <- file.choose()

AH <- read.csv(AHfile, stringsAsFactors = FALSE)
amb <- read.csv(ambfile, stringsAsFactors = FALSE)

#colnames(AH) <- c("",'datetime','AH')
#colnames(amb) <- c('time', 'no','nosd','no2','no2sd','hono','honosd')

head(AH)
head(amb)

AH$rounddate <- as.POSIXct(AH$rounddate, format = "%m/%d/%Y %H:%M")
amb$datetime <- as.POSIXct(amb$datetime, format = "%m/%d/%Y %H:%M")

#AH$datetime <- AH$datetime - 60*45

match <- match(amb$datetime, AH$rounddate)

head(match)

for (i in 2:ncol(amb)) {
  amb[,i] <- amb[,i]/(-0.0068*as.numeric(AH$absolute_humidity[match])+1)
}


head(amb)

# colnames(amb) <- c('time', 'no','nosd','no2','no2sd','hono','honosd')

write.csv(amb, file = "C:/Users/zacpayne/Desktop/humidity_corrected.csv", row.names = FALSE)
