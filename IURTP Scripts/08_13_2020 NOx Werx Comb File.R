#### NOx Measurements by Chamber using a combined file ####

## Upload Relevant Packages

library(TTR)
library(plyr)
library(dplyr)
library(zoo)

## Determineing NOx Concentrations (NO and NO2) without systems of equations

files <- list.files('D:/Surface 3/Main Folder/One Drive/IURTP/NOx/AQD NOx New CE/Raw Concentrations')

noxcomb <- data.frame(
  TheTime = c(),
  CH1_Hz = c()
)

for (i in 1:length(files)) {
  ## Place holder for i ##
  ## i <- 1
  ## Determine the file you are opening
  
  noxfilename <- paste0('D:/Surface 3/Main Folder/One Drive/IURTP/NOx/AQD NOx New CE/Raw Concentrations/', files[i])
  
  ## Open the file and eliminate unnessary data 
  
  noxfile <- read.table(noxfilename, sep = "\t", header = TRUE)
  noxfile <- data.frame(noxfile$TheTime, noxfile$CH1_Hzx)
  colnames(noxfile) <- c('TheTime', 'CH1_Hz')
  
  ## Append to previous file 
  
  noxcomb <- rbind(noxcomb, noxfile)
}
## Remove repeated times and values less than 0 ##

noxcomb <- noxcomb[!duplicated(noxfile[,c('TheTime')]),]
noxcomb$CH1_Hz[noxcomb$CH1_Hz < 0] <- NA

## Chamber the time format

noxcomb$TheTime <- as.POSIXct(noxcomb$TheTime, format = '%m/%d/%Y %H:%M:%S', tz = 'America/New_York')

## Start and end of IURTP measurement campaign

start <- as.POSIXct('08/02/2017 00:00:00', format = '%m/%d/%Y %H:%M:%S', tz = 'America/New_York')
end <- as.POSIXct('08/02/2017 13:00:00',format = '%m/%d/%Y %H:%M:%S', tz = 'America/New_York')

## Delete points before start and after end

startmatch <- match(as.integer(start), as.integer(noxcomb$TheTime))
endmatch <- match(as.integer(end), as.integer(noxcomb$TheTime))

noxcomb <- noxcomb[startmatch:endmatch, ]

## Insert nonvalues for missing points ##

ts <- seq.POSIXt(start, end, by = 'sec')
timedf <- data.frame(TheTime = ts)

noxcomb <- full_join(timedf, noxcomb)

## Estimate values for missing values based on an average of surrounding data points

noxfilenaapprox <- na.approx(noxcomb$CH1_Hz)
noxcomb$CH1_Hz <- noxfilenaapprox

## Put in minutes column again

datetime <- as.POSIXlt(noxcomb$TheTime, tz = 'America/New_York')
noxcomb$min <- datetime$min
noxcomb$sec <- datetime$sec

## Separate NOx Measurements by measurement species and find counts with those species##

noxcombbkgd <- subset(noxcomb, subset = (min %in% seq(0, 59, 5) & (sec > 30) & (sec < 55)))
noxcombno <- subset(noxcomb, subset = (min %in% seq(2, 59, 5) & (sec < 50)))
noxcombno2 <- subset(noxcomb, subset= (min %in% seq(3, 59, 5) & (sec > 30) & (sec < 55)))

noxcombbkgd$TheTime <- floor_date(noxcombbkgd$TheTime, unit = '1 minute')
noxcombno$TheTime <- floor_date(noxcombno$TheTime, unit = '1 minute')
noxcombno2$TheTime <- floor_date(noxcombno2$TheTime, unit = '1 minute')

bkgdcountsraw <- ddply(noxcombbkgd, .(TheTime), summarize, mean = mean(CH1_Hz), sd = sd(CH1_Hz))
nocountsraw <- ddply(noxcombno, .(TheTime), summarize, mean = mean(CH1_Hz), sd = sd(CH1_Hz))
no2countsraw <- ddply(noxcombno2, .(TheTime), summarize, mean = mean(CH1_Hz), sd = sd(CH1_Hz))

## Calculate counts related to each measurement

nocounts <- nocountsraw$mean - bkgdcountsraw$mean
no2counts <- no2countsraw$mean - nocountsraw$mean

nocountssd <- sqrt(nocountsraw$sd^2 + bkgdcountsraw$sd^2)
no2countssd <- sqrt(no2countsraw$sd^2 + nocountsraw$sd^2)

counts <- data.frame(
  datetime = bkgdcountsraw$TheTime,
  no_counts = nocounts,
  no_counts_sd = nocountssd,
  no2_counts = no2counts,
  no2_counts_sd = no2countssd
)

## Calculate the concentrations related to the counts

## Input the calibration constants

counts$c[as.Date(counts$datetime) %in% as.Date(c('2017-08-01', '2017-08-02', '2017-08-03', '2017-08-04', '2017-08-05'))] <- 1600.3
counts$c[as.Date(counts$datetime) %in% as.Date(c('2017-08-06', '2017-08-07'))] <- 1705.5
counts$c[as.Date(counts$datetime) %in% as.Date(c('2017-08-08'))] <- 2668.7
counts$c[as.Date(counts$datetime) %in% as.Date(c('2017-08-09', '2017-08-10'))] <- 2794.4
counts$c[as.Date(counts$datetime) %in% as.Date(c('2017-08-11', '2017-08-12'))] <- 3008.1
counts$c[as.Date(counts$datetime) %in% as.Date(c('2017-08-13', '2017-08-14', '2017-08-15'))] <- 2865
counts$c[as.Date(counts$datetime) %in% as.Date(c('2017-08-16', '2017-08-17', '2017-08-18'))] <- 2939.9

ceNO22 <- 0.751

## Concentration data.frame

concentration <- data.frame(
  datetime = counts$datetime,
  no_ppb = counts$no_counts/counts$c,
  no_ppb_sd = counts$no_counts_sd/counts$c,
  no2_ppb = counts$no2_counts/ceNO22/counts$c,
  no2_ppb_sd = counts$no2_counts_sd/ceNO22/counts$c
)

## Remove measurement times according to recording nonmeasurements times.txt

concentration <- concentration[!(concentration$datetime %in% c(
                                                
                                            #seq.POSIXt(as.POSIXct('2017-08-01 12:40:00'), as.POSIXct('2017-08-02 12:29'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-02 9:30'), as.POSIXct('2017-08-02 9:44'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-02 12:15'), as.POSIXct('2017-08-02 12:29'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-03 16:45'), as.POSIXct('2017-08-03 16:59'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-04 11:00'), as.POSIXct('2017-08-04 14:44'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-05 10:30'), as.POSIXct('2017-08-05 10:44'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-05 12:00'), as.POSIXct('2017-08-05 13:59'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-06 10:00'), as.POSIXct('2017-08-06 12:49'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-08 11:45'), as.POSIXct('2017-08-08 12:19'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-08 16:00'), as.POSIXct('2017-08-08 18:04'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-09 11:00'), as.POSIXct('2017-08-09 11:59'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-09 17:00'), as.POSIXct('2017-08-09 17:19'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-10 10:45'), as.POSIXct('2017-08-10 10:59'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-10 12:00'), as.POSIXct('2017-08-10 12:14'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-11 12:15'), as.POSIXct('2017-08-11 12:59'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-12 10:50'), as.POSIXct('2017-08-12 10:59'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-12 19:25'), as.POSIXct('2017-08-12 19:25'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-12 20:50'), as.POSIXct('2017-08-12 20:50'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-13 11:15'), as.POSIXct('2017-08-13 11:24'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-13 15:20'), as.POSIXct('2017-08-13 15:59'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-14 11:20'), as.POSIXct('2017-08-14 11:29'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-14 13:30'), as.POSIXct('2017-08-14 14:14'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-15 14:00'), as.POSIXct('2017-08-15 14:00'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-16 10:00'), as.POSIXct('2017-08-16 10:39'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-16 15:30'), as.POSIXct('2017-08-16 16:14'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-17 15:00'), as.POSIXct('2017-08-17 15:24'), '5 mins'),
                                            seq.POSIXt(as.POSIXct('2017-08-18 10:00'), as.POSIXct('2017-08-18 10:59'), '5 mins'))),]

## Sepearate by Chamber

chamberamb <- subset(concentration, as.POSIXlt(concentration$datetime)$min %in% seq(0, 59, 15))
chambera <- subset(concentration, as.POSIXlt(concentration$datetime)$min %in% seq(5, 14, 5))
chamberb <- subset(concentration, as.POSIXlt(concentration$datetime)$min %in% seq(20, 29, 5))
chamberc <- subset(concentration, as.POSIXlt(concentration$datetime)$min %in% seq(35, 44, 5))
chamberd <- subset(concentration, as.POSIXlt(concentration$datetime)$min %in% seq(50, 59, 5))

chamberamb$datetime <- floor_date(chamberamb$datetime, '1 hour')
chambera$datetime <- floor_date(chambera$datetime, '1 hour')
chamberb$datetime <- floor_date(chamberb$datetime, '1 hour')
chamberc$datetime <- floor_date(chamberc$datetime, '1 hour')
chamberd$datetime <- floor_date(chamberd$datetime, '1 hour')

chamberamb <- ddply(chamberamb, .(datetime), summarize, no_ave = mean(no_ppb),
                                          no_sd_meas = sd(no_ppb),
                                          no_sd_inst = max(no_ppb_sd),
                    no2_ave = mean(no2_ppb),
                    no2_sd_meas = sd(no2_ppb),
                    no2_sd_inst = max(no2_ppb_sd))
chambera <- ddply(chambera, .(datetime), summarize, no_ave = mean(no_ppb),
                  no_sd_meas = sd(no_ppb),
                  no_sd_inst = max(no_ppb_sd),
                  no2_ave = mean(no2_ppb),
                  no2_sd_meas = sd(no2_ppb),
                  no2_sd_inst = max(no2_ppb_sd))
chamberb <- ddply(chamberb, .(datetime), summarize, no_ave = mean(no_ppb),
                  no_sd_meas = sd(no_ppb),
                  no_sd_inst = max(no_ppb_sd),
                  no2_ave = mean(no2_ppb),
                  no2_sd_meas = sd(no2_ppb),
                  no2_sd_inst = max(no2_ppb_sd))
chamberc <- ddply(chamberc, .(datetime), summarize, no_ave = mean(no_ppb),
                  no_sd_meas = sd(no_ppb),
                  no_sd_inst = max(no_ppb_sd),
                  no2_ave = mean(no2_ppb),
                  no2_sd_meas = sd(no2_ppb),
                  no2_sd_inst = max(no2_ppb_sd))
chamberd <- ddply(chamberd, .(datetime), summarize, no_ave = mean(no_ppb),
                  no_sd_meas = sd(no_ppb),
                  no_sd_inst = max(no_ppb_sd),
                  no2_ave = mean(no2_ppb),
                  no2_sd_meas = sd(no2_ppb),
                  no2_sd_inst = max(no2_ppb_sd))
## Write to desktop

filenameamb <- 'C:/Users/zacpayne/Desktop/amb.csv'
filenamea <- 'C:/Users/zacpayne/Desktop/a.csv'
filenameb <- 'C:/Users/zacpayne/Desktop/b.csv'
filenamec <- 'C:/Users/zacpayne/Desktop/c.csv'
filenamed <- 'C:/Users/zacpayne/Desktop/d.csv'

write.csv(chamberamb, filenameamb, row.names = FALSE)
write.csv(chambera, filenamea, row.names = FALSE)
write.csv(chamberb, filenameb, row.names = FALSE)
write.csv(chamberc, filenamec, row.names = FALSE)
write.csv(chamberd, filenamed, row.names = FALSE)

## For looking at the plot ##

forplot <- noxcomb[noxcomb$TheTime %in% seq.POSIXt(as.POSIXct('2017-08-05 10:00:00'), as.POSIXct('2017-08-05 11:00'), '1 sec'),]
plot(forplot$TheTime, forplot$CH1_Hz, type = 'l')

start <-'2017-08-10 10:00:05'
end <- '2017-08-10 10:19:00'

despike <- noxcomb[noxcomb$TheTime %in% seq.POSIXt(as.POSIXct(start), as.POSIXct(end), '1 sec'),]

plot(despike$TheTime, despike$CH1_Hz, type = 'l')
despike$CH1_Hz[despike$CH1_Hz > 4000] <- NA
despike

despike$CH1_Hz[is.na(despike$CH1_Hz)] <- mean(despike$CH1_Hz[!is.na(despike$CH1_Hz)])

noxcomb[noxcomb$TheTime %in% seq.POSIXt(as.POSIXct(start), as.POSIXct(end), '1 sec'),] <- despike

write.csv(noxcomb, 'C:/Users/zacpayne/Desktop/NOx Counts Cleanup.csv', row.names = FALSE)
