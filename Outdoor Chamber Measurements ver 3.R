#### NOx Measurements Chamber ####

library(TTR)
library(plyr)
library(dplyr)
library(zoo)

## Choose File ##

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/NOx/AQD NOx", pattern = "_", full.names = TRUE)

for (j in 1:length(f))
{ assign(f[j], read.csv(f[j], header = FALSE))	}

for (k in 16:length(f))
{ noxfile <- paste0(f[k],"/",list.files(f[k], pattern = "x_17"))

noxfile <- read.table(noxfile, sep="\t", header = TRUE)
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

sdamb <- runSD(trimchamberamb$CH1_Hz, 45)[seq(45, length(trimchamberamb$CH1_Hz), 45)]
sda <- runSD(trimchambera$CH1_Hz, 45)[seq(45,length(trimchambera$CH1_Hz),45)]
sdb <- runSD(trimchamberb$CH1_Hz, 45)[seq(45,length(trimchamberb$CH1_Hz),45)]
sdc <- runSD(trimchamberc$CH1_Hz, 45)[seq(45,length(trimchamberc$CH1_Hz),45)]
sdd <- runSD(trimchamberd$CH1_Hz, 45)[seq(45,length(trimchamberd$CH1_Hz),45)]

## Seperate by instrument state ##

zeroambraw <- aveamb[seq(1,length(aveamb),4)]
zeroaraw <- avea[seq(1,length(avea),4)]
zerobraw <- aveb[seq(1,length(aveb),4)]
zerocraw <- avec[seq(1,length(avec),4)]
zerodraw <- aved[seq(1,length(aved),4)]

zeroambrawsd <- sdamb[seq(1, length(sdamb), 4)]
zeroarawsd <- sdamb[seq(1, length(sda), 4)]
zerobrawsd <- sdb[seq(1, length(sdb), 4)]
zerocrawsd <- sdc[seq(1, length(sdc), 4)]
zerodrawsd <- sdd[seq(1, length(sdd), 4)]

noambraw <- aveamb[seq(2,length(aveamb),4)]
noaraw <- avea[seq(2,length(avea),4)]
nobraw <- aveb[seq(2,length(aveb),4)]
nocraw <- avec[seq(2,length(avec),4)]
nodraw <- aved[seq(2,length(aved),4)]

noambrawsd <- sdamb[seq(2,length(sdamb),4)]
noarawsd <- sda[seq(2,length(sda),4)]
nobrawsd <- sdb[seq(2,length(sdb),4)]
nocrawsd <- sdc[seq(2,length(sdc),4)]
nodrawsd <- sdd[seq(2,length(sdd),4)]

blc1amb <- aveamb[seq(4, length(aveamb),4)]
blc1a <- avea[seq(4, length(avea),4)]
blc1b <- aveb[seq(4, length(aveb),4)]
blc1c <- avec[seq(4, length(avec),4)]
blc1d <- aved[seq(4, length(aved),4)]

blc1ambsd <- sdamb[seq(4, length(sdamb),4)]
blc1asd <- sda[seq(4, length(sda),4)]
blc1bsd <- sdb[seq(4, length(sdb),4)]
blc1csd <- sdc[seq(4, length(sdc),4)]
blc1dsd <- sdd[seq(4, length(sdd),4)]

blc2a <- avea[seq(3, length(avea),4)]
blc2b <- aveb[seq(3, length(aveb),4)]
blc2c <- avec[seq(3, length(avec),4)]
blc2d <- aved[seq(3, length(aved),4)]
blc2amb <- aveamb[seq(3, length(aveamb),4)]

blc2asd <- sda[seq(3, length(sda),4)]
blc2bsd <- sdb[seq(3, length(sdb),4)]
blc2csd <- sdc[seq(3, length(sdc),4)]
blc2dsd <- sdd[seq(3, length(sdd),4)]
blc2ambsd <- sdamb[seq(3, length(sdamb),4)]

## Measurement by Gas ##

noacounts <- noaraw - zeroaraw
nobcounts <- nobraw - zerobraw
noccounts <- nocraw - zerocraw
nodcounts <- nodraw - zerodraw
noambcounts <- noambraw - zeroambraw

noacountssd <- sqrt(noarawsd^2 + zeroarawsd^2)
nobcountssd <- sqrt(nobrawsd^2 + zerobrawsd^2)
noccountssd <- sqrt(nocrawsd^2 + zerocrawsd^2)
nodcountssd <- sqrt(nodrawsd^2 + zerodrawsd^2)
noambcountssd <- sqrt(noambrawsd^2 + zeroambrawsd^2)

blc2acounts <- blc2a - noaraw
blc2bcounts <- blc2b - nobraw
blc2ccounts <- blc2c - nocraw
blc2dcounts <- blc2d - nodraw
blc2ambcounts <- blc2amb - noambraw

blc2acountssd <- sqrt(blc2asd^2 + noarawsd^2)
blc2bcountssd <- sqrt(blc2bsd^2 + nobrawsd^2)
blc2ccountssd <- sqrt(blc2csd^2 + nocrawsd^2)
blc2dcountssd <- sqrt(blc2dsd^2 + nodrawsd^2)
blc2ambcountssd <- sqrt(blc2ambsd^2 + noambrawsd^2)

blc1acounts <- blc1a - noaraw
blc1bcounts <- blc1b - nobraw
blc1ccounts <- blc1c - nocraw
blc1dcounts <- blc1d - nodraw
blc1ambcounts <- blc1amb - noambraw

blc1acountssd <- sqrt(blc1asd^2 + noarawsd^2)
blc1bcountssd <- sqrt(blc1bsd^2 + nobrawsd^2)
blc1ccountssd <- sqrt(blc1csd^2 + nocrawsd^2)
blc1dcountssd <- sqrt(blc1dsd^2 + nodrawsd^2)
blc1ambcountssd <- sqrt(blc1ambsd^2 + noambrawsd^2)

## Gas Concentration Equation (with conversion efficiency) ##

## C is the slope of the calibration curve ##

if (any(TRUE, noxfile$day[1] == c(31,1,2,3,4,5)))
{calibration <- 1600.3
} else if (any(TRUE, noxfile$day[1] == c(6,7)))
{calibration <- 1705.5
} else if (any(TRUE, noxfile$day[1] == (8)))
{calibration <- 2668.7
}  else if (any(TRUE, noxfile$day[1] == c(9,10)))
{calibration <- 2794.4
}  else if (any(TRUE, noxfile$day[1] == c(11,12)))
{calibration <- 3008.1
}  else if (any(TRUE, noxfile$day[1] == c(13,14,15)))
{calibration <- 2865
} else if (any(TRUE, noxfile$day[1] == c(16,17)))
{calibration <- 2939.9
} else
{}

c <- calibration

## Calculation for NO

noa <- noacounts/c
nob <- nobcounts/c
noc <- noccounts/c
nod <- nodcounts/c
noamb <- noambcounts/c

noasd <- noacountssd/c
nobsd <- nobcountssd/c
nocsd <- noccountssd/c
nodsd <- nodcountssd/c
noambsd <- noambcountssd/c

## Calculation for NO2, CE1 is the conversion efficiency of blc2

ceNO2 <- 0.666

ceHONO1 <- .40229
ceHONO2 <- .16458

honoa <- (blc1acounts-blc2acounts)/((ceHONO1-ceHONO2)*c)
honob <- (blc1bcounts-blc2bcounts)/((ceHONO1-ceHONO2)*c)
honoc <- (blc1ccounts-blc2ccounts)/((ceHONO1-ceHONO2)*c)
honod <- (blc1dcounts-blc2dcounts)/((ceHONO1-ceHONO2)*c)
honoamb <- (blc1ambcounts-blc2ambcounts)/((ceHONO1-ceHONO2)*c)

honoasd <- sqrt((blc1acountssd^2 + blc2acountssd^2))/((ceHONO1-ceHONO2)*c)
honobsd <- sqrt((blc1bcountssd^2 + blc2bcountssd^2))/((ceHONO1-ceHONO2)*c)
honocsd <- sqrt((blc1ccountssd^2 + blc2ccountssd^2))/((ceHONO1-ceHONO2)*c)
honodsd <- sqrt((blc1dcountssd^2 + blc2dcountssd^2))/((ceHONO1-ceHONO2)*c)
honoambsd <- sqrt((blc1ambcountssd^2 + blc2ambcountssd^2))/((ceHONO1-ceHONO2)*c)

noa[noa < 0] <- 0
nob[nob < 0] <- 0
noc[noc < 0] <- 0
nod[nod < 0] <- 0
noamb[noamb < 0] <- 0

honoa[honoa < 0] <- 0
honob[honob < 0] <- 0
honoc[honoc < 0] <- 0
honod[honod < 0] <- 0
honoamb[honoamb < 0] <- 0

no2a <- c()
no2b <- c()
no2c <- c()
no2d <- c()
no2amb <- c()

no2asd <- c()
no2bsd <- c()
no2csd <- c()
no2dsd <- c()
no2ambsd <- c()

i = 1

for (i in 1:length(honoa)){
if (honoa[i] > 0)
{ no2a[i] <- (blc2acounts[i]-blc2acounts[i]*ceHONO2)/(ceNO2*c)
  no2asd[i] <- blc2acountssd[i]/(c*ceNO2)
} else
{ no2a[i] <- blc2acounts[i]/(c*ceNO2)
  no2asd[i] <- blc2acountssd[i]/(c*ceNO2)}
}  

i = 1

for (i in 1:length(honob)) {
if (honob[i] > 0) 
  { no2b[i] <- (blc2bcounts[i] - blc2bcounts[i] * ceHONO2)/(ceNO2 * c)
    no2bsd[i] <- blc2bcountssd[i]/(c*ceNO2)
} else 
{ no2b[i] <- blc2bcounts[i]/(c*ceNO2)
no2bsd[i] <- blc2bcountssd[i]/(c*ceNO2)}
}

i = 1

for (i in 1:length(honoc)) {
if (honoc[i] > 0)
{ no2c[i] <- (blc2ccounts[i] - blc2ccounts[i] * ceHONO2)/(ceNO2 * c)
no2csd[i] <- blc2ccountssd[i]/(c*ceNO2)
} else 
{ no2c[i] <- blc2ccounts[i]/(c*ceNO2)
no2csd[i] <- blc2ccountssd[i]/(c*ceNO2)}
}

i = 1

for (i in 1:length(honod)){
if (honod[i] > 0)  { 
  no2d[i] <- (blc2dcounts[i] - blc2dcounts[i] * ceHONO2)/(ceNO2 * c)
  no2dsd[i] <- blc2dcountssd[i]/(c*ceNO2)
} else 
{ no2d[i] <- blc2dcounts[i] /(c*ceNO2)
no2dsd[i] <- blc2dcountssd[i]/(c*ceNO2)}
}

i = 1
  
for (i in 1:length(honoamb))  {
if (honoamb[i] > 0)
{ no2amb[i] <- (blc2ambcounts[i] - blc2ambcounts[i] * ceHONO2)/(ceNO2 * c)
no2ambsd[i] <- blc2ambcountssd[i]/(c*ceNO2)
} else 
{ no2amb[i] <- blc2ambcounts[i] /(c*ceNO2)}
  no2ambsd[i] <- blc2ambcountssd[i]/(c*ceNO2)}

no2a[no2a < 0] <- 0
no2b[no2b < 0] <- 0
no2c[no2c < 0] <- 0
no2d[no2d < 0] <- 0
no2amb[no2amb < 0] <- 0

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

finalchambera <- data.frame(timea, noa, noasd, no2a, no2asd, honoa, honoasd)
finalchamberb <- data.frame(timeb, nob, nobsd, no2b, no2bsd, honob, honobsd)
finalchamberc <- data.frame(timec, noc, nocsd, no2c, no2csd, honoc, honocsd)
finalchamberd <- data.frame(timed, nod, nodsd, no2d, no2dsd, honod, honodsd)
finalchamberamb <- data.frame(timeamb, noamb, noambsd, no2amb, no2ambsd, honoamb, honoambsd)

## Write to excel file ##

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

final <- cbind.fill(finalchambera, finalchamberb,finalchamberc,finalchamberd,finalchamberamb)
colnames(final) <- c('timea', 'noa', 'noasd','no2a', 'no2asd', 'honoa', 'honoasd', 
                     'timeb', 'nob', 'nobsd','no2b', 'no2bsd', 'honob', 'honobsd',
                     'timec', 'noc', 'nocsd','no2c', 'no2csd', 'honoc', 'honocsd', 
                     'timed', 'nod', 'nodsd', 'no2d', 'no2dsd', 'honod', 'honodsd', 
                     'timeamb', 'noamb', 'noambsd', 'no2amb','no2ambsd', 'honoamb', 'honoambsd')

write.csv(final, file = (paste0(f[k], '/NOx.csv')), row.names = FALSE)

}