#### NOx Measurements Chamber ####

library(TTR)
library(plyr)
library(dplyr)
library(zoo)

## Variables used for this file ##

## Start Times, Calibration Constants, and File Name ##

starttime <- 17
c <- 2846.5
name <- '4'

## Choose 1st File ##

noxfile1 <- file.choose()

noxfile1 <- read.table(noxfile1, sep="\t", header = TRUE)
noxfile1 <- subset(noxfile1, select=c(TheTime, CH1_Hzx))
colnames(noxfile1) <- c('TheTime', 'CH1_Hz')

## Choose 2nd File ##

noxfile2 <- file.choose()

noxfile2 <- read.table(noxfile2, sep="\t", header = TRUE)
noxfile2 <- subset(noxfile2, select=c(TheTime, CH1_Hzx))
colnames(noxfile2) <- c('TheTime', 'CH1_Hz')

## Combine the two files, 1st and then 2nd ##

noxfile <- rbind(noxfile1, noxfile2)

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

## Delete data up until the beginning of a measurement hour##

noxfile <- noxfile[match(starttime,noxfile$hour):nrow(noxfile),]

while(!(noxfile$sec[nrow(noxfile)] == 59 & (noxfile$min[nrow(noxfile)] == 59)))
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

## Constants useful for determining gas concentrations (calibration constant and conversion efficiency) ##
  
ceNO22 <- 0.751
ceNO21 <- 0.588

ceNO22error <- 0.005
ceNO21error <- 0.010

ceHONO2 <- .0438
ceHONO1 <- .0986

ceHONO2error <- 0.0028
ceHONO1error <- 0.0040

d = ceNO21
e = ceNO22
f = ceHONO1
g = ceHONO2

## For determining error ##

da = blc1countssd
db = blc2countssd
dd = ceNO21error
de = ceNO22error
df = ceHONO1error
dg = ceHONO2error

Da = g/(c*(e*f-d*g)) * da
Db = f/(c*(e*f-d*g)) * db
Dd = g*(blc2countsave*f-blc1countsave*g)/(c*(e*f-g*d)^2) * dd
De = f*(blc2countsave*f-blc1countsave*g)/(c*(e*f-d*g)^2) * de
Df = g*(e*blc1countsave-blc2countsave*d)/(c*(e*f-d*g)^2) * df
Dg = (-e*blc1countsave*f+blc2countsave*d*f)/(c*(e*f-d*g)^2) * dg

DaHONO = e/(c*(d*g-e*f)) * da
DbHONO = d/(c*(d*g-e*f)) * db
DdHONO = (e*(blc1countsave*g-blc2countsave*f))/(c*(d*g-e*f)^2) * dd
DeHONO = (d*(blc2countsave*f-blc1countsave*g))/(c*(d*g-e*f)^2) * de
DfHONO = (e*(blc2countsave*d-e*blc1countsave))/(c*(d*g-e*f)^2) * df
DgHONO = (d*(blc2countsave*d-e*blc1countsave))/(c*(d*g-e*f)^2) * dg

## Solve for the concentration of each gas ##

no_ppb <- nocountsave/c
no_ppb_sd <- nocountssd/c

hono_ppb <- ((1/c) * (blc2countsave - (e/d)*blc1countsave))/(g - (e/d) * f)
hono_ppb_sd <- sqrt(DaHONO^2+DbHONO^2+DdHONO^2+DeHONO^2+DfHONO^2+DgHONO^2)

no2_ppb <- c()

no2_ppb <- ((1/c)*(blc2countsave - g/f * blc1countsave))/(e - g/f * d)
no2_ppb_sd <- sqrt(Da^2+Db^2+Dd^2+De^2+Df^2+Dg^2)

## Export the raw concentration file

rawtime <- trimsignal$TheTime[seq(45,length(trimsignal$TheTime),45)]
time <- rawtime[seq(1,length(rawtime),4)]
time <- as.POSIXct(time - 59)
time <- as.POSIXlt(time, format = '%Y-%m-%d %H:%M:%S')

## Subset by time, we will only be looking at the 2nd measurement made for each chamber to allow for the system to come to equillibrium ##

finalcounts <- data.frame(time, no_ppb, no_ppb_sd, no2_ppb, no2_ppb_sd, hono_ppb, hono_ppb_sd)
finalcounts$time <-  as.POSIXlt(finalcounts$time, format = '%Y-%m-%d %H:%M:%S')

chambera <- subset(finalcounts, finalcounts$time$min == 10)
chamberb <- subset(finalcounts, finalcounts$time$min == 25)
chamberc <- subset(finalcounts, finalcounts$time$min == 40)
chamberd <- subset(finalcounts, finalcounts$time$min == 55)

chamberamba <- subset(finalcounts, finalcounts$time$min == 0)
chamberambb <- subset(finalcounts, finalcounts$time$min == 15)
chamberambc <- subset(finalcounts, finalcounts$time$min == 30)
chamberambd <- subset(finalcounts, finalcounts$time$min == 45)

## For the chamber measurements we will want to subtract the blank chamber measurements from the chamber measurements to get the true concentration

chambera$no_ppb <- chambera$no_ppb - chamberamba$no_ppb
chambera$no_ppb_sd <- sqrt(chambera$no_ppb_sd^2 + chamberamba$no_ppb_sd^2)
chambera$no2_ppb <- chambera$no_ppb - chamberamba$no2_ppb
chambera$no2_ppb_sd <- sqrt(chambera$no2_ppb_sd^2 + chamberamba$no2_ppb_sd^2)
chambera$hono_ppb <- chambera$hono_ppb - chamberamba$hono_ppb
chambera$hono_ppb_sd <- sqrt(chambera$hono_ppb_sd^2 + chamberamba$hono_ppb_sd^2)
chambera$hour <- seq(0,nrow(chambera)-1)

chamberb$no_ppb <- chamberb$no_ppb - chamberambb$no_ppb
chamberb$no_ppb_sd <- sqrt(chamberb$no_ppb_sd^2 + chamberambb$no_ppb_sd^2)
chamberb$no2_ppb <- chamberb$no_ppb - chamberambb$no2_ppb
chamberb$no2_ppb_sd <- sqrt(chamberb$no2_ppb_sd^2 + chamberambb$no2_ppb_sd^2)
chamberb$hono_ppb <- chamberb$hono_ppb - chamberambb$hono_ppb
chamberb$hono_ppb_sd <- sqrt(chamberb$hono_ppb_sd^2 + chamberambb$hono_ppb_sd^2)
chamberb$hour <- seq(0,nrow(chamberb)-1)

chamberc$no_ppb <- chamberc$no_ppb - chamberambc$no_ppb
chamberc$no_ppb_sd <- sqrt(chamberc$no_ppb_sd^2 + chamberambc$no_ppb_sd^2)
chamberc$no2_ppb <- chamberc$no_ppb - chamberambc$no2_ppb
chamberc$no2_ppb_sd <- sqrt(chamberc$no2_ppb_sd^2 + chamberambc$no2_ppb_sd^2)
chamberc$hono_ppb <- chamberc$hono_ppb - chamberambc$hono_ppb
chamberc$hono_ppb_sd <- sqrt(chamberc$hono_ppb_sd^2 + chamberambc$hono_ppb_sd^2)
chamberc$hour <- seq(0,nrow(chamberc)-1)

chamberd$no_ppb <- chamberd$no_ppb - chamberambd$no_ppb
chamberd$no_ppb_sd <- sqrt(chamberd$no_ppb_sd^2 + chamberambd$no_ppb_sd^2)
chamberd$no2_ppb <- chamberd$no_ppb - chamberambd$no2_ppb
chamberd$no2_ppb_sd <- sqrt(chamberd$no2_ppb_sd^2 + chamberambd$no2_ppb_sd^2)
chamberd$hono_ppb <- chamberd$hono_ppb - chamberambd$hono_ppb
chamberd$hono_ppb_sd <- sqrt(chamberd$hono_ppb_sd^2 + chamberambd$hono_ppb_sd^2)
chamberd$hour <- seq(0,nrow(chamberd)-1)

filenamea <- paste0('C:/Users/Zachary/Desktop/Working Files/', name, ' NOy Concentrations Chamber A.csv')
filenameb <- paste0('C:/Users/Zachary/Desktop/Working Files/', name, ' NOy Concentrations Chamber B.csv')
filenamec <- paste0('C:/Users/Zachary/Desktop/Working Files/', name, ' NOy Concentrations Chamber C.csv')
filenamed <- paste0('C:/Users/Zachary/Desktop/Working Files/', name, ' NOy Concentrations Chamber D.csv')

write.csv(chambera, file = filenamea, row.names = FALSE)
write.csv(chamberb, file = filenameb, row.names = FALSE)
write.csv(chamberc, file = filenamec, row.names = FALSE)
write.csv(chamberd, file = filenamed, row.names = FALSE)
