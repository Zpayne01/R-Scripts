#### NOx Measurements Chamber ####

## This file is for determine the concentration of NOy at the UMBS during the 2016 Prophet AMOS campaign. As such the calibration during this time
## is finicky and may need to be redetermined to get acurate results. Regardless, the concentrations found can be used to determine fluxes.
## Note that the measurement periods are different during the first half of the campaign and the second half of the campaign. This will need to be updated
## in the code if further analysis is necessary.

## Upload relevant packages

library(TTR)
library(plyr)
library(dplyr)
library(zoo)

## Determining NOx concentrations usin systems of equations to determine the conversion efficiency

file <- file.choose()

noxfile <- read.table(file, sep="\t", header = TRUE)
noxfile <- subset(noxfile, select=c(TheTime, CH1_Hz))
colnames(noxfile) <- c('TheTime', 'CH1_Hz')

## Remove repeated times and values less than 0 ##

noxfile <- noxfile[!duplicated(noxfile[,c('TheTime')]),]
noxfile$CH1_Hz[noxfile$CH1_Hz < 0] <- NA 

## Obtain Date and Time ##

datetime <- strptime(noxfile$TheTime, "%m/%d/%Y %H:%M:%S", tz = "est")

## Adding 3 seconds to get rid of the annoying peaks due to three way valve switching (we see a peak during valve transition periods adding this parameter
## makes sure the it doesn't end up in our averages)

datetime <- datetime 
datetime <- as.POSIXlt(datetime)
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec
datetime <- as.POSIXct(datetime)

noxfile$TheTime <- datetime

## Delete data up until a whole measurement (measurements beging on either 0, 15, 30, or 45 mins ##

while(!(noxfile$sec[1] == 0 & (noxfile$min[1] == 0) | (noxfile$min[1] == 15) | (noxfile$min[1] == 30) | (noxfile$min[1] == 45)))
{noxfile <- noxfile[-c(1),]}
while(!(noxfile$sec[nrow(noxfile)] == 59 &  (noxfile$min[nrow(noxfile)] == 14) | (noxfile$min[nrow(noxfile)] == 29) | (noxfile$min[nrow(noxfile)] == 44) | (noxfile$min[nrow(noxfile)] == 59)))
{noxfile <- noxfile[-c(nrow(noxfile)),]}

## Insert non values for missing points ##

start <- as.POSIXct(noxfile$TheTime[1])

end <- as.POSIXct(noxfile$TheTime[nrow(noxfile)]) 

ts <- seq.POSIXt(start, end, by = "sec")

timedf <- data.frame(timestamp=ts)
colnames(timedf) <- c('TheTime')

noxfile <- full_join(timedf, noxfile)

## Estimate values for non-values based on surrounding data points

noxfilenaapprox <- na.approx(noxfile$CH1_Hz)
noxfile$CH1_Hz <- noxfilenaapprox

## Put in Minutes again ##

datetime <- as.POSIXlt(noxfile$TheTime, tz = "est")
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec

## Quick Plot

rawplot <- plot(noxfile$TheTime, noxfile$CH1_Hz, type = "l")

## Seperate NOx measurements by Chamber ##

chambera <- subset(noxfile, subset = min >= 0 & min < 15)
chamberb <- subset(noxfile, subset = min >= 15 & min < 30)
chamberc <- subset(noxfile, subset = min >= 30 & min < 45)
chamberd <- subset(noxfile, subset = min >= 45 & min < 60)

## Trim time it takes to reach equilibrium ##

trimchambera <- subset(chambera, subset = ( min == 1 | min == 2 | min == 3 | min == 4 | min == 6 | min == 7 | 
                                              min == 8 | min == 9 | min == 11 | min == 12 | min == 13 | min == 14) & sec >= 15)
trimchamberb <- subset(chamberb, subset = ( min == 16 | min == 17 | min == 18 | min == 19 |min == 21 | 
                                              min == 22 | min == 23 | min == 24 |min == 26 | min == 27 | min == 28 | min == 29) & sec >= 15)
trimchamberc <- subset(chamberc, subset = ( min == 31 | min == 32 | min == 33 | min == 34 |min == 36 | 
                                              min == 37 | min == 38 | min == 39 | min == 41 | min == 42 | min == 43 | min == 44)  & sec >= 15)
trimchamberd <- subset(chamberd, subset = ( min == 46 | min == 47 | min == 48 | min == 49 |min == 51 | 
                                              min == 52 | min == 53 | min == 54 | min == 56 | min == 57 | min == 58 | min == 59) & sec >= 15)

## Take the average and standard deviation every 45 points points ##

avea <- runMean(trimchambera$CH1_Hz, 45)[seq(45,length(trimchambera$CH1_Hz),45)]
aveb <- runMean(trimchamberb$CH1_Hz, 45)[seq(45,length(trimchamberb$CH1_Hz),45)]
avec <- runMean(trimchamberc$CH1_Hz, 45)[seq(45,length(trimchamberc$CH1_Hz),45)]
aved <- runMean(trimchamberd$CH1_Hz, 45)[seq(45,length(trimchamberd$CH1_Hz),45)]

sda <- runSD(trimchambera$CH1_Hz, 45)[seq(45,length(trimchambera$CH1_Hz),45)]
sdb <- runSD(trimchamberb$CH1_Hz, 45)[seq(45,length(trimchamberb$CH1_Hz),45)]
sdc <- runSD(trimchamberc$CH1_Hz, 45)[seq(45,length(trimchamberc$CH1_Hz),45)]
sdd <- runSD(trimchamberd$CH1_Hz, 45)[seq(45,length(trimchamberd$CH1_Hz),45)]

## Seperate by instrument state ##

zeroaraw <- avea[seq(1,length(avea),4)]
zerobraw <- aveb[seq(1,length(aveb),4)]
zerocraw <- avec[seq(1,length(avec),4)]
zerodraw <- aved[seq(1,length(aved),4)]

zeroarawsd <- sdamb[seq(1, length(sda), 4)]
zerobrawsd <- sdb[seq(1, length(sdb), 4)]
zerocrawsd <- sdc[seq(1, length(sdc), 4)]
zerodrawsd <- sdd[seq(1, length(sdd), 4)]

noaraw <- avea[seq(2,length(avea),4)]
nobraw <- aveb[seq(2,length(aveb),4)]
nocraw <- avec[seq(2,length(avec),4)]
nodraw <- aved[seq(2,length(aved),4)]

noarawsd <- sda[seq(2,length(sda),4)]
nobrawsd <- sdb[seq(2,length(sdb),4)]
nocrawsd <- sdc[seq(2,length(sdc),4)]
nodrawsd <- sdd[seq(2,length(sdd),4)]

blc1a <- avea[seq(4, length(avea),4)]
blc1b <- aveb[seq(4, length(aveb),4)]
blc1c <- avec[seq(4, length(avec),4)]
blc1d <- aved[seq(4, length(aved),4)]

blc1asd <- sda[seq(4, length(sda),4)]
blc1bsd <- sdb[seq(4, length(sdb),4)]
blc1csd <- sdc[seq(4, length(sdc),4)]
blc1dsd <- sdd[seq(4, length(sdd),4)]

blc2a <- avea[seq(3, length(avea),4)]
blc2b <- aveb[seq(3, length(aveb),4)]
blc2c <- avec[seq(3, length(avec),4)]
blc2d <- aved[seq(3, length(aved),4)]

blc2asd <- sda[seq(3, length(sda),4)]
blc2bsd <- sdb[seq(3, length(sdb),4)]
blc2csd <- sdc[seq(3, length(sdc),4)]
blc2dsd <- sdd[seq(3, length(sdd),4)]

## Relevant counts for each instrument state ##

## Relevant NO counts ##

noacounts <- noaraw - zeroaraw
nobcounts <- nobraw - zerobraw
noccounts <- nocraw - zerocraw
nodcounts <- nodraw - zerodraw

noacountssd <- sqrt(noarawsd^2 + zeroarawsd^2)
nobcountssd <- sqrt(nobrawsd^2 + zerobrawsd^2)
noccountssd <- sqrt(nocrawsd^2 + zerocrawsd^2)
nodcountssd <- sqrt(nodrawsd^2 + zerodrawsd^2)

## Add in the time ##

rawtimea <- trimchambera$TheTime[seq(45,length(trimchambera$TheTime),45)]
timea <- rawtimea[seq(1,length(rawtimea),4)]
timea <- as.POSIXct(timea - 119)

rawtimeb <- trimchamberb$TheTime[seq(45,length(trimchamberb$TheTime),45)]
timeb <- rawtimeb[seq(1,length(rawtimeb),4)]
timeb <- as.POSIXct(timeb - 119)

rawtimec <- trimchamberc$TheTime[seq(45,length(trimchamberc$TheTime),45)]
timec <- rawtimec[seq(1,length(rawtimec),4)]
timec <- as.POSIXct(timec - 119)

rawtimed <- trimchamberd$TheTime[seq(45,length(trimchamberd$TheTime),45)]
timed <- rawtimed[seq(1,length(rawtimed),4)]
timed <- as.POSIXct(timed - 119)

chambera <- data.frame(timea, noacounts)
chamberb <- data.frame(timeb, nobcounts)
chamberc <- data.frame(timec, noccounts)
chamberd <- data.frame(timed, nodcounts)

chamberaamb <- chambera[seq(1, nrow(chambera), 3),]
chamberbamb <- chamberb[seq(1, nrow(chamberb), 3),]
chambercamb <- chamberc[seq(1, nrow(chamberc), 3),]
chamberdamb <- chamberd[seq(1, nrow(chamberd), 3),]

names <- c("time", "no_counts")

colnames(chamberaamb) <- names
colnames(chamberbamb) <- names
colnames(chambercamb) <- names
colnames(chamberdamb) <- names


ambientmeasurements <- rbind(chamberaamb, chamberbamb, chambercamb, chamberdamb)

ambientmeasurements <- ambientmeasurements[order(as.POSIXct(ambientmeasurements$time, format = "%Y-%m-%d %H:%M:%S", decreasing = FALSE)),]

head(ambientmeasurements)
write.table(ambientmeasurements, file = "clipboard", sep = "\t", row.names = FALSE)
