library(lubridate)

## This file was used to seperate the igor file containing the NO concentration data (using 0.56 as the calibration factor
## and subtracting the background). Therefore it is fairly limited in capability, applicaible only to the beginning of the Prophet 
## Campaign (07/18-07/22).

file <- file.choose()
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

head(csv)

csv$time <- as.POSIXlt(csv$time, format = "%m/%d/%Y %H:%M")

amba <- subset(csv, csv$time$min == 0)
ambb <- subset(csv, csv$time$min == 15)
ambc <- subset(csv, csv$time$min == 30)
ambd <- subset(csv, csv$time$min == 45)

plot(amba$time, amba$no, type = "l")
plot(ambb$time, ambb$no, type = "l")
plot(ambc$time, ambc$no, type = "l")
plot(ambd$time, ambd$no, type = "l")

ambb$time <- floor_date(ambb$time, unit = 'hour')
ambc$time <- floor_date(ambc$time, unit = 'hour')
ambd$time <- floor_date(ambd$time, unit = 'hour')

filename <- "C:/Users/Zachary/Desktop/Ambient_"

write.csv(amba, file = paste0(filename, "A.csv"), row.names = FALSE)
write.csv(ambb, file = paste0(filename, "B.csv"), row.names = FALSE)
write.csv(ambc, file = paste0(filename, "C.csv"), row.names = FALSE)
write.csv(ambd, file = paste0(filename, "D.csv"), row.names = FALSE)