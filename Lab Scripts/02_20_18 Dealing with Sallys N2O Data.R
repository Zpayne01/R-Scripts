## This is for processing the corrected data from Sally into chamber measurements

file <- file.choose()
n2o <- read.csv(file, stringsAsFactors = FALSE)

colnames(n2o) <- c('time', 'n2o_despiked', 'n2o_corrected')

library(lubridate)

time <- as.POSIXct(n2o$time, format = '%m/%d/%Y %H:%M:%S')
time <- time + 365 * 24 * 60 * 60 * 117 + 29 * 24 * 60 * 60

time2 <- floor_date(time, unit = '1 minutes')
n2o$time <- time2
time2 <- as.POSIXlt(time2, format = "%Y-%m-%d %H:%M:%S")
n2o$min <- time2$min

sampledn2o <- subset(n2o, n2o$min != 0 & n2o$min != 5 & n2o$min != 15 & n2o$min != 20 & n2o$min != 30 & n2o$min != 35 & n2o$min != 45 & n2o$min != 50)
                     
time <- floor_date(sampledn2o$time, unit = '5 minutes')

sampledn2o$time <- time

averagen2o <- aggregate(n2o_corrected ~ time, sampledn2o, mean)
sdn2o <- aggregate(n2o_corrected ~ time, sampledn2o, sd)

preseperatedn2o <- data.frame(averagen2o, sdn2o$n2o_corrected)

preseperatedn2o$time <- as.POSIXlt(preseperatedn2o$time, format = '%Y-%m-%d %H:%M:%S')
preseperatedn2o$min <- preseperatedn2o$time$min

chambera <- subset(preseperatedn2o, preseperatedn2o$min == 0)
chamberb <- subset(preseperatedn2o, preseperatedn2o$min == 15)
chamberc <- subset(preseperatedn2o, preseperatedn2o$min == 30)
chamberd <- subset(preseperatedn2o, preseperatedn2o$min == 45)

chambera$time <- floor_date(chambera$time, unit = '1 hour')
chamberb$time <- floor_date(chamberb$time, unit = '1 hour')
chamberc$time <- floor_date(chamberc$time, unit = '1 hour')
chamberd$time <- floor_date(chamberd$time, unit = '1 hour')

columnnames <- c('time', 'n2o_average', 'n2o_sd')

exporta <- chambera[1:3]
exportb <- chamberb[1:3]
exportc <- chamberc[1:3]
exportd <- chamberd[1:3]

colnames(exporta) <- columnnames
colnames(exportb) <- columnnames
colnames(exportc) <- columnnames
colnames(exportd) <- columnnames

write.csv(exporta, file = 'C:/Users/Zachary/Desktop/N2O_chamber_a.csv', row.names = FALSE)
write.csv(exportb, file = 'C:/Users/Zachary/Desktop/N2O_chamber_b.csv', row.names = FALSE)
write.csv(exportc, file = 'C:/Users/Zachary/Desktop/N2O_chamber_c.csv', row.names = FALSE)
write.csv(exportd, file = 'C:/Users/Zachary/Desktop/N2O_chamber_d.csv', row.names = FALSE)



