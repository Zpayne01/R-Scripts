file <- file.choose()
csv <- read.csv(file)

csv$datetime <- strptime(csv$datetime, format = "%m/%d/%Y %H:%M")
datetime <- csv$datetime
csv$hour <- datetime$hour
csv$day <- datetime$mday

head(csv)

AH <- aggregate(absolute_humidity ~ hour, csv, mean)
RH <- aggregate(RH ~ hour, csv, mean)
 
UV <- subset(csv, csv$percent_UV != 'NA' & csv$day < 16 & csv$day > 11)

UV2 <- aggregate(UV ~ hour, UV, mean)

UV2UV2
UV$hour == 0

UV$hour
UV

UV2
head(UV)

export <- cbind(RH, UV2)
write.table(export, file = "clipboard", sep = "\t")
