## Used to match meta data with measurement data

file1 <- file.choose()
file2 <- file.choose()

csv1 <- read.csv(file1)
csv2 <- read.csv(file2)

colnames(csv1) <- c('time', 'ave', 'sd')
colnames(csv2) <- c('time', 'ave', 'sd')

csv1$time <- strptime(csv1$time, format = '%m/%d/%Y %H:%S')
csv1$time <- as.POSIXct(csv1$time, format = '%Y-%m-%d %H:%M:%S')
csv2$time <- as.POSIXct(csv2$time, format = '%m/%d/%Y %H:%M')

csv1 <- csv1[!is.na(csv1$time),]
csv2 <- csv2[!is.na(csv2$time),]

matches <- match(csv1$time, csv2$time)

export <- data.frame(csv1, csv2$ave[matches], csv2$sd[matches])
write.csv(export, file = 'C:/Users/Zachary/Desktop/Match.csv', row.names = FALSE)

head(csv1)
