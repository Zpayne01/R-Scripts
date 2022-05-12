#### NOx Measurements Chamber ####

library(TTR)
library(plyr)
library(dplyr)
library(zoo)

## Choose File ##

noxfile <- file.choose()

noxfile <- read.table(noxfile, sep=",", header = TRUE)
noxfile <- subset(noxfile, select=c(TheTime, CH1_Hz))
colnames(noxfile) <- c('TheTime', 'CH1_Hz')

## Remove repeated times and values less than 0 ##

noxfile <- noxfile[!duplicated(noxfile[,c('TheTime')]),]
noxfile$CH1_Hz[noxfile$CH1_Hz < 0] <- NA 

## Obtain Date and Time ##

datetime <- strptime(noxfile$TheTime, "%m/%d/%Y %H:%M:%S", tz = "est")

## Adding 3 seconds to get rid of the annoying peaks due to three way valve switching ##

datetime <- datetime 
datetime <- as.POSIXlt(datetime)
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec
datetime <- as.POSIXct(datetime)

noxfile$TheTime <- datetime

## Delete data up until a whole measurement ##

while(!(noxfile$sec[1] == 0 & (noxfile$min[1] == 0 | noxfile$min[1] == 5 | noxfile$min[1] == 10 | noxfile$min[1] == 15 | noxfile$min[1] == 20 | noxfile$min[1] == 25 | noxfile$min[1] == 30 | noxfile$min[1] == 35 | noxfile$min[1] == 40 | noxfile$min[1] == 45 | noxfile$min[1] == 50 | noxfile$min[1] == 55)))
{noxfile <- noxfile[-c(1),]}
while(!(noxfile$sec[nrow(noxfile)] == 59 & (noxfile$min[nrow(noxfile)] == 4 | noxfile$min[nrow(noxfile)] == 9 | noxfile$min[nrow(noxfile)] == 14 | noxfile$min[nrow(noxfile)] == 19 | noxfile$min[nrow(noxfile)] == 24 |  noxfile$min[nrow(noxfile)] == 29 | noxfile$min[nrow(noxfile)] == 34 | noxfile$min[nrow(noxfile)] == 39 | noxfile$min[nrow(noxfile)] == 49 | noxfile$min[nrow(noxfile)] == 54 |noxfile$min[nrow(noxfile)] == 44 | noxfile$min[nrow(noxfile)] == 59)))
{noxfile <- noxfile[-c(nrow(noxfile)),]}

## Insert non values for missing points ##

start <- as.POSIXct(noxfile$TheTime[1])

end <- as.POSIXct(noxfile$TheTime[nrow(noxfile)]) 

ts <- seq.POSIXt(start, end, by = "sec")

timedf <- data.frame(timestamp=ts)
colnames(timedf) <- c('TheTime')

noxfile <- full_join(timedf, noxfile)

## 

noxfilenaapprox <- na.approx(noxfile$CH1_Hz)
noxfile$CH1_Hz <- noxfilenaapprox

## Put in Minutes again ##

datetime <- as.POSIXlt(noxfile$TheTime, tz = "est")
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec

## Plot the Raw Signal to see ##

plot(noxfile$TheTime, noxfile$CH1_Hz, type = 'l')

## Trim the signal into useful subsets for subsequent averaging ##

trimsignal <- subset(noxfile, subset = (min != 1 & min != 6 & min != 11 & min != 16 &
                                                 min != 21 & min != 26 & min != 31 & min != 36 &
                                                 min != 41 & min != 46 & min != 51 & min != 56 & sec >= 15))

## Average and take the standard deviation of the signal ##

avesignal <-  runMean(trimsignal$CH1_Hz, 45)[seq(45,length(trimsignal$CH1_Hz),45)]
sdsignal <- runSD(trimsignal$CH1_Hz, 45)[seq(45, length(trimsignal$CH1_Hz),45)]

zeroave <- avesignal[seq(1,length(avesignal),4)]
zerosd <- sdsignal[seq(1, length(sdsignal), 4)]

noave <- avesignal[seq(2,length(avesignal), 4)]
nosd <- sdsignal[seq(2, length(sdsignal), 4)]

blc2ave <- avesignal[seq(3, length(avesignal), 4)]
blc2sd <- sdsignal[seq(3, length(sdsignal), 4)]

blc1ave <- avesignal[seq(4, length(avesignal), 4)]
blc1sd <- sdsignal[seq(4, length(sdsignal), 4)]

## Counts by Gas - This is particularly useful for calibration ##

nocountsave <- noave - zeroave
nocountssd <- sqrt(nosd ^ 2 + zerosd ^ 2)

blc2countsave <- blc2ave - noave
blc2countssd <- sqrt(blc2sd ^ 2 + nosd ^ 2)

blc1countsave <- blc1ave - noave
blc1countssd <- sqrt(blc1sd ^ 2 + nosd ^ 2)

## Extract data as counts per time ##

rawtime <- trimsignal$TheTime[seq(45,length(trimsignal$TheTime),45)]
time <- rawtime[seq(1,length(rawtime),4)]
time <- as.POSIXct(time - 59)

finalcounts <- data.frame(time, nocountsave, nocountssd, blc2countsave, blc2countssd, blc1countsave, blc1countssd)
filename <- 'C:/Users/zacpayne/Desktop/Counts.csv'

write.csv(finalcounts, file = filename, row.names = FALSE)
