#### NOx Measurements Chamber ####

library(TTR)
library(dplyr)
library(zoo)
library(plyr)

## Choose File ##

file <- file.choose()
noxfile <- read.table(file, sep="\t", header = TRUE)
noxfile <- subset(noxfile, select=c(TheTime, CH1_Hz))

## Remove repeated times and values less than 0 ##

noxfile <- noxfile[!duplicated(noxfile[,c('TheTime')]),]
noxfile$CH1_Hz[noxfile$CH1_Hz < 0] <- NA 

## Obtain Date and Time ##

datetime <- strptime(noxfile$TheTime, "%m/%d/%Y %H:%M:%S", tz = "est")
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec
datetime <- as.POSIXct(datetime)

noxfile$TheTime <- datetime

## Delete data up until a whole measurement ##

while(!(noxfile$sec[1] == 0 & (noxfile$min[1] == 0) | (noxfile$min[1] == 12) | (noxfile$min[1] == 24) | (noxfile$min[1] == 36) | (noxfile$min[1] == 48)))
	{noxfile <- noxfile[-c(1),]}
while(!(noxfile$sec[nrow(noxfile)] == 0 &  (noxfile$min[nrow(noxfile)] == 0) | (noxfile$min[nrow(noxfile)] == 11) | (noxfile$min[nrow(noxfile)] == 23) | (noxfile$min[nrow(noxfile)] == 35) | (noxfile$min[nrow(noxfile)] == 47)))
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

## Seperate NOx measurements by Chamber ##

chambera <- subset(noxfile, subset = min >= 0 & min < 12)
chamberb <- subset(noxfile, subset = min >= 12 & min < 24)
chamberc <- subset(noxfile, subset = min >= 24 & min < 36)
chamberd <- subset(noxfile, subset = min >= 36 & min < 48)
chambere <- subset(noxfile, subset = min >= 48 & min < 60)

## Trim time it takes to reach equilibrium ##

trimchambera <- subset(chambera, subset = min == 1 | min == 2 | min == 4 | min == 5 |
							min == 7 | min == 8 | min == 10 | min == 11)
trimchamberb <- subset(chamberb, subset = min == 13 | min == 14 | min == 16 | min == 17 |
							min == 19 | min == 20 | min == 22 | min == 23)
trimchamberc <- subset(chamberc, subset = min == 25 | min == 26 | min == 28 | min == 29 |
							min == 31 | min == 32 | min == 34 | min == 35)
trimchamberd <- subset(chamberd, subset = min == 37 | min == 38 | min == 40 | min == 41 |
							min == 43 | min == 44 | min == 46 | min == 47)
trimchambere <- subset(chambere, subset = min == 49 | min == 50 | min == 52 | min == 53 |
							min == 55 | min == 56 | min == 58 | min == 59)

## Average every 120 points ##

avea <- runMean(trimchambera$CH1_Hz, 120, cumulative = TRUE)[seq(120,length(trimchambera$CH1_Hz),120)]
aveb <- runMean(trimchamberb$CH1_Hz, 120)[seq(120,length(trimchamberb$CH1_Hz),120)]
avec <- runMean(trimchamberc$CH1_Hz, 120)[seq(120,length(trimchamberc$CH1_Hz),120)]
aved <- runMean(trimchamberd$CH1_Hz, 120)[seq(120,length(trimchamberd$CH1_Hz),120)]
avee <- runMean(trimchambere$CH1_Hz, 120)[seq(120,length(trimchambere$CH1_Hz),120)]

## Seperate by instrument state ##

zeroaraw <- avea[seq(1,length(avea),4)]
zerobraw <- aveb[seq(1,length(aveb),4)]
zerocraw <- avec[seq(1,length(avec),4)]
zerodraw <- aved[seq(1,length(aved),4)]
zeroeraw <- avee[seq(1,length(avee),4)]

noaraw <- avea[seq(2,length(avea),4)]
nobraw <- aveb[seq(2,length(aveb),4)]
nocraw <- avec[seq(2,length(avec),4)]
nodraw <- aved[seq(2,length(aved),4)]
noeraw <- avee[seq(2,length(avee),4)]

blc1a <- avea[seq(4, length(avea),4)]
blc1b <- aveb[seq(4, length(aveb),4)]
blc1c <- avec[seq(4, length(avec),4)]
blc1d <- aved[seq(4, length(aved),4)]
blc1e <- avee[seq(4, length(avee),4)]

blc2a <- avea[seq(3, length(avea),4)]
blc2b <- aveb[seq(3, length(aveb),4)]
blc2c <- avec[seq(3, length(avec),4)]
blc2d <- aved[seq(3, length(aved),4)]
blc2e <- avee[seq(3, length(avee),4)]

## Measurement by Gas ##

noacounts <- noaraw - zeroaraw
nobcounts <- nobraw - zerobraw
noccounts <- nocraw - zerocraw
nodcounts <- nodraw - zerodraw
noecounts <- noeraw - zeroeraw

blc1acounts <- blc1a - noaraw
blc1bcounts <- blc1b - nobraw
blc1ccounts <- blc1c - nocraw
blc1dcounts <- blc1d - nodraw
blc1ecounts <- blc1e - noeraw

blc2acounts <- blc2a - noaraw
blc2bcounts <- blc2b - nobraw
blc2ccounts <- blc2c - nocraw
blc2dcounts <- blc2d - nodraw
blc2ecounts <- blc2e - noeraw

## Gas Concentration Equation (with conversion efficiency) ##

noa <- noacounts/2472.6
nob <- nobcounts/2472.6
noc <- noccounts/2472.6
nod <- nodcounts/2472.6
noe <- noecounts/2472.6

no2a <- blc1acounts/(2472.6*0.575)
no2b <- blc1bcounts/(2472.6*0.575)
no2c <- blc1ccounts/(2472.6*0.575)
no2d <- blc1dcounts/(2472.6*0.575)
no2e <- blc1ecounts/(2472.6*0.575)

honoa <- (blc2acounts/(2472.6*0.667) - blc1acounts/(2472.6*0.575))/(.088-0.021)
honob <- (blc2bcounts/(2472.6*0.667) - blc1bcounts/(2472.6*0.575))/(.088-0.021)
honoc <- (blc2ccounts/(2472.6*0.667) - blc1ccounts/(2472.6*0.575))/(.088-0.021)
honod <- (blc2dcounts/(2472.6*0.667) - blc1dcounts/(2472.6*0.575))/(.088-0.021)
honoe <- (blc2ecounts/(2472.6*0.667) - blc1ecounts/(2472.6*0.575))/(.088-0.021)

## Time of each measurement ##

rawtimea <- trimchambera$TheTime[seq(120,length(trimchambera$TheTime),120)]
timea <- rawtimea[seq(1,length(rawtimea),4)]

rawtimeb <- trimchamberb$TheTime[seq(120,length(trimchamberb$TheTime),120)]
timeb <- rawtimeb[seq(1,length(rawtimeb),4)]

rawtimec <- trimchamberc$TheTime[seq(120,length(trimchamberc$TheTime),120)]
timec <- rawtimec[seq(1,length(rawtimec),4)]

rawtimed <- trimchamberd$TheTime[seq(120,length(trimchamberd$TheTime),120)]
timed <- rawtimed[seq(1,length(rawtimed),4)]

rawtimee <- trimchambere$TheTime[seq(120,length(trimchambere$TheTime),120)]
timee <- rawtimee[seq(1,length(rawtimee),4)]

## Combine time with each gas measurement ##

finalchambera <- data.frame(timea, noa, no2a, honoa)
finalchamberb <- data.frame(timeb, nob, no2b, honob)
finalchamberc <- data.frame(timec, noc, no2c, honoc)
finalchamberd <- data.frame(timed, nod, no2d, honod)
finalchambere <- data.frame(timee, noe, no2e, honoe)

## Write to excel file ##

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

final <- cbind.fill(finalchambera, finalchamberb,finalchamberc,finalchamberd,finalchambere)
colnames(final) <- c('timea', 'noa', 'no2a', 'honoa', 'timeb', 'nob', 'no2b', 'honob', 'timec', 'noc', 'no2c', 'honoc', 'timed', 'nod', 'no2d', 'honod','timee', 'noe', 'no2e', 'honoe')

write.table(final, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)
