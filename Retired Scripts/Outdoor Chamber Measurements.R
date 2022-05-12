#### NOx Measurements Chamber ####

library(TTR)
library(dplyr)
library(zoo)
library(plyr)

## Choose File ##

file <- file.choose()
noxfile <- read.table(file, sep="\t", header = TRUE)
noxfile <- subset(noxfile, select=c(TheTime, CH1_Hzx))
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

chamberamb <- subset(noxfile, subset = min >= 0 & min < 5 | min >= 15 & min < 20 | min >= 30 & min < 35 | min >= 45 & min < 50)
chambera <- subset(noxfile, subset = min >= 5 & min < 15)
chamberb <- subset(noxfile, subset = min >= 20 & min < 30)
chamberc <- subset(noxfile, subset = min >= 35 & min < 45)
chamberd <- subset(noxfile, subset = min >= 50 & min < 60)

## Trim time it takes to reach equilibrium ##

trimchamberamb <- subset(chamberamb, subset = (min == 0 | min == 2 | min == 3 | min == 4 |
							min == 15 | min == 17 | min == 18 | min == 19 |
							min == 30 | min == 32 | min == 33 | min == 34 |
							min == 45 | min == 47 | min == 48 | min == 49) & sec >= 15)
trimchambera <- subset(chambera, subset = (min == 5 | min == 7 | min == 8 | min == 9 |
							min == 10 | min == 12 | min == 13 | min == 14) & sec >= 15)
trimchamberb <- subset(chamberb, subset = (min == 20 | min == 22 | min == 23 | min == 24 |
							min == 25 | min == 27 | min == 28 | min == 29) & sec >= 15)
trimchamberc <- subset(chamberc, subset = (min == 35 | min == 37 | min == 38 | min == 39 |
							min == 40 | min == 42 | min == 43 | min == 44)  & sec >= 15)
trimchamberd <- subset(chamberd, subset = (min == 50 | min == 52 | min == 53 | min == 54 |
							min == 55 | min == 57 | min == 58 | min == 59) & sec >= 15)

## Average every 45 points points ##

aveamb <- runMean(trimchamberamb$CH1_Hz, 45)[seq(45,length(trimchamberamb$CH1_Hz),45)]
avea <- runMean(trimchambera$CH1_Hz, 45)[seq(45,length(trimchambera$CH1_Hz),45)]
aveb <- runMean(trimchamberb$CH1_Hz, 45)[seq(45,length(trimchamberb$CH1_Hz),45)]
avec <- runMean(trimchamberc$CH1_Hz, 45)[seq(45,length(trimchamberc$CH1_Hz),45)]
aved <- runMean(trimchamberd$CH1_Hz, 45)[seq(45,length(trimchamberd$CH1_Hz),45)]

## Seperate by instrument state ##

zeroambraw <- aveamb[seq(1,length(aveamb),4)]
zeroaraw <- avea[seq(1,length(avea),4)]
zerobraw <- aveb[seq(1,length(aveb),4)]
zerocraw <- avec[seq(1,length(avec),4)]
zerodraw <- aved[seq(1,length(aved),4)]

noambraw <- aveamb[seq(2,length(aveamb),4)]
noaraw <- avea[seq(2,length(avea),4)]
nobraw <- aveb[seq(2,length(aveb),4)]
nocraw <- avec[seq(2,length(avec),4)]
nodraw <- aved[seq(2,length(aved),4)]

blc1amb <- aveamb[seq(4, length(aveamb),4)]
blc1a <- avea[seq(4, length(avea),4)]
blc1b <- aveb[seq(4, length(aveb),4)]
blc1c <- avec[seq(4, length(avec),4)]
blc1d <- aved[seq(4, length(aved),4)]

blc2a <- avea[seq(3, length(avea),4)]
blc2b <- aveb[seq(3, length(aveb),4)]
blc2c <- avec[seq(3, length(avec),4)]
blc2d <- aved[seq(3, length(aved),4)]
blc2amb <- aveamb[seq(3, length(aveamb),4)]

## Measurement by Gas ##

noacounts <- noaraw - zeroaraw
nobcounts <- nobraw - zerobraw
noccounts <- nocraw - zerocraw
nodcounts <- nodraw - zerodraw
noambcounts <- noambraw - zeroambraw

blc1acounts <- blc1a - noaraw
blc1bcounts <- blc1b - nobraw
blc1ccounts <- blc1c - nocraw
blc1dcounts <- blc1d - nodraw
blc1ambcounts <- blc1amb - noambraw

blc2acounts <- blc2a - noaraw
blc2bcounts <- blc2b - nobraw
blc2ccounts <- blc2c - nocraw
blc2dcounts <- blc2d - nodraw
blc2ambcounts <- blc2amb - noambraw

## Gas Concentration Equation (with conversion efficiency) ##

## C is the slope of the calibration curve ##

calibration <- 2962
c <- calibration

## Calculation for NO

noa <- noacounts/c
nob <- nobcounts/c
noc <- noccounts/c
nod <- nodcounts/c
noamb <- noambcounts/c

## Calculation for NO2, CE1 is the conversion efficiency of BLC1

CE1 <- 0.575

no2a <- blc1acounts/(c*CE1)
no2b <- blc1bcounts/(c*CE1)
no2c <- blc1ccounts/(c*CE1)
no2d <- blc1dcounts/(c*CE1)
no2amb <- blc1ambcounts/(c*CE1)

honoa <- (blc2acounts/(c*0.667) - blc1acounts/(c*0.575))/(.088-0.021)
honob <- (blc2bcounts/(c*0.667) - blc1bcounts/(c*0.575))/(.088-0.021)
honoc <- (blc2ccounts/(c*0.667) - blc1ccounts/(c*0.575))/(.088-0.021)
honod <- (blc2dcounts/(c*0.667) - blc1dcounts/(c*0.575))/(.088-0.021)
honoamb <- (blc2ambcounts/(c*0.667) - blc1ambcounts/(c*0.575))/(.088-0.021)

## Time of each measurement ##

rawtimea <- trimchambera$TheTime[seq(45,length(trimchambera$TheTime),45)]
timea <- rawtimea[seq(1,length(rawtimea),4)]
timea <- as.POSIXct(timea - 59)

rawtimeb <- trimchamberb$TheTime[seq(45,length(trimchamberb$TheTime),45)]
timeb <- rawtimeb[seq(1,length(rawtimeb),4)]
timeb <- as.POSIXct(timeb - 59)

rawtimec <- trimchamberc$TheTime[seq(45,length(trimchamberc$TheTime),45)]
timec <- rawtimec[seq(1,length(rawtimec),4)]
timec <- as.POSIXct(timec - 59)

rawtimed <- trimchamberd$TheTime[seq(45,length(trimchamberd$TheTime),45)]
timed <- rawtimed[seq(1,length(rawtimed),4)]
timed <- as.POSIXct(timed - 59)

rawtimeamb <- trimchamberamb$TheTime[seq(45,length(trimchamberamb$TheTime),45)]
timeamb <- rawtimeamb[seq(1,length(rawtimeamb),4)]
timeamb <- as.POSIXct(timeamb - 59)

## Combine time with each gas measurement ##

finalchambera <- data.frame(timea, noa, no2a, honoa)
finalchamberb <- data.frame(timeb, nob, no2b, honob)
finalchamberc <- data.frame(timec, noc, no2c, honoc)
finalchamberd <- data.frame(timed, nod, no2d, honod)
finalchamberamb <- data.frame(timeamb, noamb, no2amb, honoamb)

## Write to excel file ##

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

final <- cbind.fill(finalchambera, finalchamberb,finalchamberc,finalchamberd,finalchamberamb)
colnames(final) <- c('timea', 'noa', 'no2a', 'honoa', 'timeb', 'nob', 'no2b', 'honob', 'timec', 'noc', 'no2c', 'honoc', 'timed', 'nod', 'no2d', 'honod','timeamb', 'noamb', 'no2amb', 'honoamb')

write.table(final, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)
