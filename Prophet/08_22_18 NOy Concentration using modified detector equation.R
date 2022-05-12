library(TTR)
library(plyr)
library(dplyr)
library(zoo)

## Prophet NO using the old system ##

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

## Seperate NOx measurements by Chamber ##

chambera <- subset(noxfile, subset = min >= 0 & min < 15)
chamberb <- subset(noxfile, subset = min >= 15 & min < 30)
chamberc <- subset(noxfile, subset = min >= 30 & min < 45)
chamberd <- subset(noxfile, subset = min >= 45 & min < 60)

## Trim time it takes to reach equilibrium ##

trimchambera <- subset(chambera, subset = ( min == 1 | min == 2 | min == 3 | min == 4 | min == 6 | min == 7 | 
                                              min == 8 | min == 9 | min == 11 | min == 12 | min == 13 | min == 14) & sec >= 30)
trimchamberb <- subset(chamberb, subset = ( min == 16 | min == 17 | min == 18 | min == 19 |min == 21 | 
                                              min == 22 | min == 23 | min == 24 |min == 26 | min == 27 | min == 28 | min == 29) & sec >= 30)
trimchamberc <- subset(chamberc, subset = ( min == 31 | min == 32 | min == 33 | min == 34 |min == 36 | 
                                              min == 37 | min == 38 | min == 39 | min == 41 | min == 42 | min == 43 | min == 44)  & sec >= 30)
trimchamberd <- subset(chamberd, subset = ( min == 46 | min == 47 | min == 48 | min == 49 |min == 51 | 
                                              min == 52 | min == 53 | min == 54 | min == 56 | min == 57 | min == 58 | min == 59) & sec >= 30)

## Take the average and standard deviation every 45 points points ##

avea <- runMean(trimchambera$CH1_Hz, 30)[seq(30,length(trimchambera$CH1_Hz),30)]
aveb <- runMean(trimchamberb$CH1_Hz, 30)[seq(30,length(trimchamberb$CH1_Hz),30)]
avec <- runMean(trimchamberc$CH1_Hz, 30)[seq(30,length(trimchamberc$CH1_Hz),30)]
aved <- runMean(trimchamberd$CH1_Hz, 30)[seq(30,length(trimchamberd$CH1_Hz),30)]

sda <- runSD(trimchambera$CH1_Hz, 30)[seq(30,length(trimchambera$CH1_Hz),30)]
sdb <- runSD(trimchamberb$CH1_Hz, 30)[seq(30,length(trimchamberb$CH1_Hz),30)]
sdc <- runSD(trimchamberc$CH1_Hz, 30)[seq(30,length(trimchamberc$CH1_Hz),30)]
sdd <- runSD(trimchamberd$CH1_Hz, 30)[seq(30,length(trimchamberd$CH1_Hz),30)]

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

## NO Concentration Measurement ##

noa = noaraw*((0.162+(0.426*exp(-5.1e-6*noaraw))+(0.598*exp(-6.1e-5*noaraw))))-zeroaraw*((0.162+(0.426*exp(-5.1e-6*zeroaraw))+(0.598*exp(-6.1e-5*zeroaraw))))
nob = nobraw*((0.162+(0.426*exp(-5.1e-6*nobraw))+(0.598*exp(-6.1e-5*nobraw))))-zerobraw*((0.162+(0.426*exp(-5.1e-6*zerobraw))+(0.598*exp(-6.1e-5*zerobraw))))
noc = nocraw*((0.162+(0.426*exp(-5.1e-6*nocraw))+(0.598*exp(-6.1e-5*nocraw))))-zerocraw*((0.162+(0.426*exp(-5.1e-6*zerocraw))+(0.598*exp(-6.1e-5*zerocraw))))
nod = nodraw*((0.162+(0.426*exp(-5.1e-6*nodraw))+(0.598*exp(-6.1e-5*nodraw))))-zerodraw*((0.162+(0.426*exp(-5.1e-6*zerodraw))+(0.598*exp(-6.1e-5*zerodraw))))

## Time of each measurement ##

rawtimea <- trimchambera$TheTime[seq(30,length(trimchambera$TheTime),30)]
timea <- rawtimea[seq(1,length(rawtimea),4)]
timea <- as.POSIXct(timea - 119)

rawtimeb <- trimchamberb$TheTime[seq(30,length(trimchamberb$TheTime),30)]
timeb <- rawtimeb[seq(1,length(rawtimeb),4)]
timeb <- as.POSIXct(timeb - 119)

rawtimec <- trimchamberc$TheTime[seq(30,length(trimchamberc$TheTime),30)]
timec <- rawtimec[seq(1,length(rawtimec),4)]
timec <- as.POSIXct(timec - 119)

rawtimed <- trimchamberd$TheTime[seq(30,length(trimchamberd$TheTime),30)]
timed <- rawtimed[seq(1,length(rawtimed),4)]
timed <- as.POSIXct(timed - 119)

## Combine time with each gas measurement ##

finalchambera <- data.frame(timea, noa)
finalchamberb <- data.frame(timeb, nob)
finalchamberc <- data.frame(timec, noc)
finalchamberd <- data.frame(timed, nod)