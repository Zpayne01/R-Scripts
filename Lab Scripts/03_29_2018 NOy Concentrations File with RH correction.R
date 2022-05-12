#### NOx Measurements Chamber ####

library(TTR)
library(plyr)
library(dplyr)
library(zoo)

## Choose File ##

noxfile <- file.choose()

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

## Counts Adjusted RH

RH <- 50.79

nocountsave <- nocountsave/(-0.0010479*RH+1.00)
nocountssd <-  nocountssd/(-0.0010479*RH+1.00)

blc2countsave <- blc2countsave/(-0.0010479*RH+1.00)
blc2countssd <- blc2countssd/(-0.0010479*RH+1.00)

blc1countsave <- blc1countsave/(-0.0010479*RH+1.00)
blc1countssd <- blc1countssd/(-0.0010479*RH+1.00)


## Constants useful for determining gas concentrations (calibration constant and conversion efficiency) ##

c <- 2687.2
  
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

no2_ppb <- ((1/c)*(blc2countsave - g/f * blc1countsave))/(e - g/f * d)
no2_ppb_sd <- sqrt(Da^2+Db^2+Dd^2+De^2+Df^2+Dg^2)

## Export the raw concentration file

rawtime <- trimsignal$TheTime[seq(45,length(trimsignal$TheTime),45)]
time <- rawtime[seq(1,length(rawtime),4)]
time <- as.POSIXct(time - 59)

finalcounts <- data.frame(time, no_ppb, no_ppb_sd, no2_ppb, no2_ppb_sd, hono_ppb, hono_ppb_sd)
filename <- 'C:/Users/Zachary/Desktop/NOy Concentrations.csv'

write.csv(finalcounts, file = filename, row.names = FALSE)
