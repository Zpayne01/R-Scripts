
## Choose STC file and load data.table

file <- file.choose()

data <- read.table(file, sep = ' ', skip = 1, header = FALSE)
data <- data[,1:8]

colnames(data) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')

## Update Time

data$datetime <- as.POSIXct(data$datetime, origin = "1904-01-01 00:00:00", tz = 'UTC')
print(head(data))

## Choose start and end time

start <- as.POSIXct('2020-10-23 00:00:01', tz = 'UTC')
end <- as.POSIXct('2020-10-23 01:00:00', tz = 'UTC')

startmatch <- match(as.integer(start), as.integer(data$datetime))
endmatch <- match(as.integer(end), as.integer(data$datetime))

## Plot By Start and End Time

plot(data$datetime[startmatch:endmatch], data$`446`[startmatch:endmatch], type = 'l')
