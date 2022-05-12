
file <- file.choose()

csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
csv$datetime <- as.POSIXlt(csv$datetime, format = "%Y-%m-%d %H:%M:%S")

file2 <- file.choose()

csv2 <- read.csv(file2, header = TRUE, stringsAsFactors = FALSE)
colnames(csv2) <- c("datetime", "air_temp", "air_temp_sd", "air_temp_obs")
csv2$datetime <- as.POSIXlt(csv2$datetime, format = "%Y-%m-%d %H:%M:%S")

csv2$datetime <- update(csv2$datetime, minutes = 45)

dateMatch <- match(as.Date("2017-08-01"), as.Date(csv2$datetime))
csv2 <- csv2[dateMatch:nrow(csv2),]

tempMatch <- match(csv2$datetime, csv$datetime)

csv2$tempDifference <- csv2$air_temp - csv$Temperature[tempMatch]

head(csv2)

write.csv(csv2, "C:/users/Zachary/Desktop/Rename File.csv", row.names = FALSE)
