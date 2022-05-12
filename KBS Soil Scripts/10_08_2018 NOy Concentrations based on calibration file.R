#### NOx Measurements Chamber ####

library(TTR)
library(plyr)
library(dplyr)
library(zoo)

## Choose File ##

noxfile <- file.choose()

noxfile <- read.table(noxfile, sep="\t", header = TRUE)
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

## Start Time ##

starttime <- 15

## Delete data up until the beginning of a measurement hour##

while(!(noxfile$sec[1] == 0 & (noxfile$min[1] == 0)))
{noxfile <- noxfile[-c(1),]}
while(!(noxfile$sec[nrow(noxfile)] == 59 & ((noxfile$min[nrow(noxfile)] == 14) | (noxfile$min[nrow(noxfile)] == 29) | (noxfile$min[nrow(noxfile)] == 44) | (noxfile$min[nrow(noxfile)] == 59))))
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

calibrationfile <- read.csv(file = "C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/NOx Calibration Log.txt", header = TRUE, stringsAsFactors = FALSE)
calibrationfile$Date <- as.POSIXct(calibrationfile$Date, format = "%m/%d/%Y")

matchcal <- match(as.Date(datetime[1]), as.Date(calibrationfile$Date))

c <- calibrationfile$Calibration[matchcal]
  
ceNO22 <- 0.724
ceNO21 <- 0.731

ceNO22error <- 0.012
ceNO21error <- 0.010

ceHONO2 <- .041
ceHONO1 <- .086

ceHONO2error <- 0.0003
ceHONO1error <- 0.0007

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

chamberb$no_ppb <- chamberb$no_ppb - chamberambb$no_ppb
chamberb$no_ppb_sd <- sqrt(chamberb$no_ppb_sd^2 + chamberambb$no_ppb_sd^2)
chamberb$no2_ppb <- chamberb$no_ppb - chamberambb$no2_ppb
chamberb$no2_ppb_sd <- sqrt(chamberb$no2_ppb_sd^2 + chamberambb$no2_ppb_sd^2)
chamberb$hono_ppb <- chamberb$hono_ppb - chamberambb$hono_ppb
chamberb$hono_ppb_sd <- sqrt(chamberb$hono_ppb_sd^2 + chamberambb$hono_ppb_sd^2)

chamberc$no_ppb <- chamberc$no_ppb - chamberambc$no_ppb
chamberc$no_ppb_sd <- sqrt(chamberc$no_ppb_sd^2 + chamberambc$no_ppb_sd^2)
chamberc$no2_ppb <- chamberc$no_ppb - chamberambc$no2_ppb
chamberc$no2_ppb_sd <- sqrt(chamberc$no2_ppb_sd^2 + chamberambc$no2_ppb_sd^2)
chamberc$hono_ppb <- chamberc$hono_ppb - chamberambc$hono_ppb
chamberc$hono_ppb_sd <- sqrt(chamberc$hono_ppb_sd^2 + chamberambc$hono_ppb_sd^2)

chamberd$no_ppb <- chamberd$no_ppb - chamberambd$no_ppb
chamberd$no_ppb_sd <- sqrt(chamberd$no_ppb_sd^2 + chamberambd$no_ppb_sd^2)
chamberd$no2_ppb <- chamberd$no_ppb - chamberambd$no2_ppb
chamberd$no2_ppb_sd <- sqrt(chamberd$no2_ppb_sd^2 + chamberambd$no2_ppb_sd^2)
chamberd$hono_ppb <- chamberd$hono_ppb - chamberambd$hono_ppb
chamberd$hono_ppb_sd <- sqrt(chamberd$hono_ppb_sd^2 + chamberambd$hono_ppb_sd^2)

filenamea <- 'C:/Users/Zachary/Desktop/Working Files/NOy Concentrations Chamber A.csv'
filenameb <- 'C:/Users/Zachary/Desktop/Working Files/NOy Concentrations Chamber B.csv'
filenamec <- 'C:/Users/Zachary/Desktop/Working Files/NOy Concentrations Chamber C.csv'
filenamed <- 'C:/Users/Zachary/Desktop/Working Files/NOy Concentrations Chamber D.csv'

write.csv(chambera, file = filenamea, row.names = FALSE)
write.csv(chamberb, file = filenameb, row.names = FALSE)
write.csv(chamberc, file = filenamec, row.names = FALSE)
write.csv(chamberd, file = filenamed, row.names = FALSE)
