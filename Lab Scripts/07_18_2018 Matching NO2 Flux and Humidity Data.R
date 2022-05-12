## This file is specifically for matching humidity data but can be used to match any files based on their time

file1 <- file.choose()
file2 <- file.choose()

csv1 <- read.csv(file1, header = TRUE, stringsAsFactors = FALSE)
csv2 <- read.csv(file2, header = TRUE, stringsAsFactors = FALSE)

csv1$time <- as.POSIXct(csv1$time, format = "%Y-%m-%d %H:%M:%S")
csv2$time <- as.POSIXct(csv2$datetime_B_airtemp, format = "%Y-%m-%d %H:%M:%S")

matchedtimes <- match(csv1$time, csv2$time)

export <- cbind(csv1, csv2$airtemp_B_ave[matchedtimes], csv2$airtemp_B_sd[matchedtimes])
write.csv(export, file = "C:/Users/Zachary/Desktop/matched_file.csv", row.names = FALSE)
