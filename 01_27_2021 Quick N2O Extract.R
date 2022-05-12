## N2O extraction 

file <- file.choose()

data <- read.table(file, sep = ' ', skip = 1, header = FALSE)
data <- data[,1:8]

colnames(data) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')

## Change the string to a relevant time format

data$datetime <- as.POSIXct(data$datetime+(3600*5), origin = "1904-01-01 00:00:00", tz = Sys.timezone())
print(head(data))

write.csv(data, file = 'C:/users/zacpayne/Desktop/02_05_20 n2o.csv', row.names = FALSE)

## Experiment

start <- as.POSIXct('2021-01-25 11:21:52')
end <- as.POSIXct('2021-01-25 23:59:59')

startmatch <- match(as.integer(start), as.integer(data$datetime))
startmatch

endmatch <-match(as.integer(end), as.integer(data$datetime))
endmatch

plot(data$datetime[startmatch:endmatch], 
     data$`446`[startmatch:endmatch], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[N2O] (ppb)')

## CO2 Extraction

## Open CO2 tab delemited file ##

file <- file.choose()
CO2 <- read.delim(file, sep = "", header = FALSE)
CO2 <- CO2[-1,]
CO2 <- CO2[-1,]
CO2[5:9] <- list(NULL)

colnames(CO2) <- c('date','time','CO2_ppm','H2O_ppt')

## Get time ##
datetime <- paste(CO2$date, CO2$time)
datetime <- strptime((datetime), format='%Y-%m-%d %H:%M:%S', tz = 'EST')
CO2$min <- datetime$min
CO2$day <- datetime$mday
CO2$hour <- datetime$hour
datetime <- format(datetime, "%m/%d/%Y %H:%M:%S")
CO2$datetime <- as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%S")
CO2$date <- NULL
CO2$time <- NULL

start <- as.POSIXct('2020-10-16 00:00:00')
end <- as.POSIXct('2020-10-16 07:00:00')

startmatch <- match(as.integer(start), as.integer(CO2$datetime))
startmatch

endmatch <-match(as.integer(end), as.integer(CO2$datetime))
endmatch

plot(CO2$datetime[startmatch:endmatch], 
     CO2$H2O_ppt[startmatch:endmatch], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[H2O] (ppth)')

start2 <- as.POSIXct('2020-10-15 20:00:00')
end2 <- as.POSIXct('2020-10-15 22:00:00')

startmatch2 <- match(as.integer(start2), as.integer(CO2$datetime))
startmatch2

endmatch2 <-match(as.integer(end2), as.integer(CO2$datetime))
endmatch2

par(mfrow = c(1,1))

plot(CO2$datetime[startmatch2:endmatch2], 
     CO2$CO2_ppm[startmatch2:endmatch2], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[CO2] (ppth)')

plot(CO2$datetime[startmatch:endmatch], 
     CO2$H2O_ppt[startmatch:endmatch], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[H2O] (ppth)')