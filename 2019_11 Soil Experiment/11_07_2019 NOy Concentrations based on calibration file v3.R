## v3 uses start and end times to try and get exactly what I need out, if trying to find calibrations use v2

## When using this file make sure to update the calibration file located in the KBS soil folder as well as the startime located within this file

#### NOx Measurements Chamber ####

#### If you do not have these packages installed then use install.packages() to get each of them in turn

library(TTR)
library(plyr)
library(dplyr)
library(zoo)


## Choose File ##
## This can allow automated file processing instead of clicking through each file, if you would like to click through each file
## you can delete the for loop and replace it with file <- file.choose()

filelist <- list.files("D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/NOx/Raw Data/") #Use you directory name containing your raw data

for (i in 1:length(filelist)) {

filename <- paste0("D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/NOx/Raw Data/",filelist[i])  

## This information will vary based on the output of your instrumentation. Generally you just want [NO] and [NO2]
  
noxfile <- read.csv(filename, stringsAsFactors = FALSE, header = TRUE)
noxfile <- subset(noxfile, select=c(TheTime, CH1_Hz))
colnames(noxfile) <- c('TheTime', 'CH1_Hz')
noxfile$TheTime <- as.POSIXct(noxfile$TheTime*(60*60*24), origin = "1899-12-30", tz = 'UTC')

## Remove repeated times and values less than 0 ##

noxfile <- noxfile[!duplicated(noxfile[,c('TheTime')]),]
noxfile$CH1_Hz[noxfile$CH1_Hz < 0] <- NA 

## Obtain Date and Time ##

datetime <- strptime(noxfile$TheTime, format = '%Y-%m-%d %H:%M:%S',tz = "EST")

## Adding 3 seconds to get rid of the annoying peaks due to three way valve switching ##

datetime <- datetime 
datetime <- as.POSIXlt(datetime)
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec
datetime <- as.POSIXct(datetime)

noxfile$TheTime <- datetime

## Insert non values for missing points ##

start <- as.POSIXct(noxfile$TheTime[1])

end <- as.POSIXct(noxfile$TheTime[nrow(noxfile)]) 

ts <- seq.POSIXt(start, end, by = "sec")

timedf <- data.frame(timestamp=ts)
colnames(timedf) <- c('TheTime')

noxfile <- full_join(timedf, noxfile)

## Estimate missing time based on nearby data and remove repeated data points

noxfilenaapprox <- na.approx(noxfile$CH1_Hz)
noxfile$CH1_Hz <- noxfilenaapprox
noxfile <- noxfile[match(unique(noxfile$TheTime), noxfile$TheTime),]


## Start and End Times

starttimefile <- read.csv(file = "D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Start and End Times.txt", header = TRUE, stringsAsFactors = FALSE)
starttimefile$Date <- as.POSIXct(starttimefile$Date, format = "%m/%d/%Y", tz = 'EST')

matchstarttime <- match(as.Date(datetime[1], tz = 'EST'), as.Date(starttimefile$Date))

starttime <- starttimefile$starttime[matchstarttime]
endtime <- starttimefile$endtime[matchstarttime]

## Delete data up until the beginning of a measurement hour##

## Change end match if not dealing with 48 hour measurements

startmatch <- match(as.POSIXct(paste0(as.Date(datetime[1], tz = 'EST'), ' ', starttime, ':00:00'), tz = 'EST'), noxfile$TheTime)

noxfile <- noxfile[startmatch:nrow(noxfile),]

endmatch <- match(as.POSIXct(paste0(as.Date(datetime[1]+86400*1, tz = 'EST'), ' ', endtime, ':00:00'), tz = 'EST'), noxfile$TheTime)

noxfile <- noxfile[1:endmatch,]


# while(!(noxfile$sec[1] == 0 & (noxfile$hour[1] == starttime)))
# {noxfile <- noxfile[-c(1),]}
# while(!(noxfile$hour[nrow(noxfile)] == endtime-1 &  noxfile$sec[nrow(noxfile)] == 59 & noxfile$min[nrow(noxfile)] == 59))
# {noxfile <- noxfile[-c(nrow(noxfile)),]}

## Put in Minutes again ##

datetime <- as.POSIXlt(noxfile$TheTime, tz = "est")
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec
filedate <- gsub('-','_',as.Date(datetime[1]))

## Plot out the NOx Raw Signal for analysis

filedate <- gsub('-','_',as.Date(datetime[1]))

jpeg(paste0('D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/NOx/Raw Plot/',filedate,'.jpeg'))

noxplot <- plot(noxfile$TheTime, noxfile$CH1_Hz, type = "l", xlab = 'Time (EST)', ylab = 'Counts per Second', 
                ylim=c(0,8000), main = paste0('Raw File for ', filedate))

dev.off()

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

calibrationfile <- read.csv(file = "D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/NOx Calibration Log.txt", header = TRUE, stringsAsFactors = FALSE)
calibrationfile$Date <- as.POSIXct(calibrationfile$Date, format = "%m/%d/%Y")

matchcal <- match(as.Date(datetime[1]), as.Date(calibrationfile$Date))

c <- calibrationfile$Calibration[matchcal]
  
ceNO22 <- 0.9320
ceNO21 <- 0.8507

ceNO22error <- 0.0130
ceNO21error <- 0.0140

ceHONO2 <- .0485
ceHONO1 <- .0968

ceHONO2error <- 0.0002
ceHONO1error <- 0.0002

d = ceNO21 #385 LED NO2 conversion efficiency
e = ceNO22 #395 LED NO2 conversion efficiency
f = ceHONO1 #385 LED HONO conversion efficiency
g = ceHONO2 #395 LED HONO conversion efficiency

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

## Shorten to 22 or 22 hour time frames for ease

chambera <- chambera[1:22,]
chamberb <- chamberb[1:22,]
chamberc <- chamberc[1:22,]
chamberd <- chamberd[1:22,]

chamberamba <- chamberamba[1:22,]
chamberambb <- chamberambb[1:22,]
chamberambc <- chamberambc[1:22,]
chamberambd <- chamberambd[1:22,]

chambera$hour <- seq(1,22,1)
chamberb$hour <- seq(1,22,1)
chamberc$hour <- seq(1,22,1)
chamberd$hour <- seq(1,22,1)

## Determine the true concentration of each species by correcting for absolute humidity

h2ofile <- list.files("D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Absolute Humidity/")
h2o <- read.csv(paste0("D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Absolute Humidity/",h2ofile[i]), header = TRUE, stringsAsFactors = FALSE)

chambera[2:7] <- data.frame(sapply(chambera[2:7], function(x) {x/(-0.0093*h2o[2]+1)}))
chamberb[2:7] <- data.frame(sapply(chamberb[2:7], function(x) {x/(-0.0093*h2o[4]+1)}))
chamberc[2:7] <- data.frame(sapply(chamberc[2:7], function(x) {x/(-0.0093*h2o[6]+1)}))
chamberd[2:7] <- data.frame(sapply(chamberd[2:7], function(x) {x/(-0.0093*h2o[8]+1)}))

## For the chamber measurements we will want to subtract the blank chamber measurements from the chamber measurements to get the true concentration

chambera$no_ppb <- chambera$no_ppb - (chamberamba$no_ppb+chamberambb$no_ppb)/2
chambera$no_ppb_sd <- sqrt(chambera$no_ppb_sd^2 + chamberamba$no_ppb_sd^2)
chambera$no2_ppb <- chambera$no2_ppb - (chamberamba$no2_ppb+chamberambb$no2_ppb)/2
chambera$no2_ppb_sd <- sqrt(chambera$no2_ppb_sd^2 + chamberamba$no2_ppb_sd^2)
chambera$hono_ppb <- chambera$hono_ppb - (chamberamba$hono_ppb+chamberambb$hono_ppb)/2
chambera$hono_ppb_sd <- sqrt(chambera$hono_ppb_sd^2 + chamberamba$hono_ppb_sd^2)

chamberb$no_ppb <- chamberb$no_ppb - (chamberambb$no_ppb+chamberambc$no_ppb)/2
chamberb$no_ppb_sd <- sqrt(chamberb$no_ppb_sd^2 + chamberambb$no_ppb_sd^2)
chamberb$no2_ppb <- chamberb$no2_ppb - (chamberambb$no2_ppb+chamberambc$no2_ppb)/2
chamberb$no2_ppb_sd <- sqrt(chamberb$no2_ppb_sd^2 + chamberambb$no2_ppb_sd^2)
chamberb$hono_ppb <- chamberb$hono_ppb - (chamberambb$hono_ppb+chamberambc$hono_ppb)/2
chamberb$hono_ppb_sd <- sqrt(chamberb$hono_ppb_sd^2 + chamberambb$hono_ppb_sd^2)

chamberc$no_ppb <- chamberc$no_ppb - (chamberambc$no_ppb+chamberambd$no_ppb)/2
chamberc$no_ppb_sd <- sqrt(chamberc$no_ppb_sd^2 + chamberambc$no_ppb_sd^2)
chamberc$no2_ppb <- chamberc$no2_ppb - (chamberambc$no2_ppb+chamberambd$no2_ppb)/2
chamberc$no2_ppb_sd <- sqrt(chamberc$no2_ppb_sd^2 + chamberambc$no2_ppb_sd^2)
chamberc$hono_ppb <- chamberc$hono_ppb - (chamberambc$hono_ppb+chamberambd$hono_ppb)/2
chamberc$hono_ppb_sd <- sqrt(chamberc$hono_ppb_sd^2 + chamberambc$hono_ppb_sd^2)

chamberd$no_ppb <- chamberd$no_ppb - (chamberambd$no_ppb+c(chamberamba$no_ppb[2:length(chamberamba$no_ppb)], chamberambd$no_ppb[length(chamberambd$no_ppb)]))/2
chamberd$no_ppb_sd <- sqrt(chamberd$no_ppb_sd^2 + chamberambd$no_ppb_sd^2)
chamberd$no2_ppb <- chamberd$no2_ppb - (chamberambd$no2_ppb+c(chamberamba$no2_ppb[2:length(chamberamba$no2_ppb)], chamberambd$no2_ppb[length(chamberambd$no2_ppb)]))/2
chamberd$no2_ppb_sd <- sqrt(chamberd$no2_ppb_sd^2 + chamberambd$no2_ppb_sd^2)
chamberd$hono_ppb <- chamberd$hono_ppb - (chamberambd$hono_ppb+c(chamberamba$hono_ppb[2:length(chamberamba$hono_ppb)], chamberambd$hono_ppb[length(chamberambd$hono_ppb)]))/2
chamberd$hono_ppb_sd <- sqrt(chamberd$hono_ppb_sd^2 + chamberambd$hono_ppb_sd^2)

#Flux measurements

## Flow Rates

flowfile <- read.csv(file = "D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Flow Rates.txt", header = TRUE, stringsAsFactors = FALSE)
flowfile$Date <- as.POSIXct(flowfile$Date, format = "%m/%d/%Y")

flowdate <- match(as.Date(datetime[1]), as.Date(flowfile$Date))

flowb <- flowfile$FlowRateB[flowdate]
flowc <- flowfile$FlowRateC[flowdate]
flowd <- flowfile$FlowRateD[flowdate]
flowe <- flowfile$FlowRateE[flowdate]

## Mass of Dried Soil

massfile <- read.csv(file = "D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Mass of Dried Soil.txt", header = TRUE, stringsAsFactors = FALSE)
massfile$Date <- as.POSIXct(massfile$Date, format = '%m/%d/%Y')

massdate <- match(as.Date(datetime[1]), as.Date(massfile$Date))

massb <- massfile$chamberBMass[massdate]
massc <- massfile$chamberCMass[massdate]
massd <- massfile$chamberDMass[massdate]
masse <- massfile$chamberEMass[massdate]

## Contents of each Chamber ##

chamberfile <- read.csv(file = "D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Chamber Log.txt", header = TRUE, stringsAsFactors = FALSE)
chamberfile$Date <- as.POSIXct(chamberfile$Date, format = '%m/%d/%Y')

chamberdate <- match(as.Date(datetime[1]), as.Date(chamberfile$Date))

contentsb <- chamberfile[chamberdate, 2]
contentsc <- chamberfile[chamberdate, 4]
contentsd <- chamberfile[chamberdate, 6]
contentse <- chamberfile[chamberdate, 8]

plotb <- chamberfile[chamberdate, 3]
plotc <- chamberfile[chamberdate, 5]
plotd <- chamberfile[chamberdate, 7]
plote <- chamberfile[chamberdate, 9]

## Flux Measurement 

chamberbflux <- data.frame(chambera[1],sapply(chambera[2:7], function(x) {x/10^9/24.1*flowb*14*10^9*60/massb}),chambera[8], paste0(contentsb,'-',plotb))
chambercflux <- data.frame(chamberb[1],sapply(chamberb[2:7], function(x) {x/10^9/24.1*flowc*14*10^9*60/massc}),chamberb[8], paste0(contentsc,'-',plotc))
chamberdflux <- data.frame(chamberc[1],sapply(chamberc[2:7], function(x) {x/10^9/24.1*flowd*14*10^9*60/massd}),chamberc[8], paste0(contentsd,'-',plotd))
chambereflux <- data.frame(chamberd[1],sapply(chamberd[2:7], function(x) {x/10^9/24.1*flowe*14*10^9*60/masse}),chamberd[8], paste0(contentse,'-',plote))

names <- c('time', 'no_flux', 'no_flux_sd', 'no2_flux', 'no2_flux_sd', 'hono_flux', 'hono_flux_sd', 'hour', "chamber")

colnames(chamberbflux) <- names
colnames(chambercflux) <- names
colnames(chamberdflux) <- names
colnames(chambereflux) <- names

flux <- rbind(chamberbflux, chambercflux, chamberdflux, chambereflux) 

## Plot the flux

noplot <- ggplot(data = flux, aes(x = hour, y = no_flux, color = chamber)) + geom_line() +
  labs(title = paste('NO flux for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time Since Start (hr.)', 
       y = expression(paste('NO flux (ug N kg  '^-1,'dry soil hr  '^-1,')')),
       color = "Treatment - Plot")

noplot <- noplot + theme_bw() + theme(plot.title = element_text(size = 32, hjust = 0.5))

png(paste0('D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/NOx/NO Flux Plots/',filedate,'.png'))

print(noplot)

dev.off()


no2plot <- ggplot(data = flux, aes(x = hour, y = no2_flux, color = chamber)) + geom_line() +
  labs(title = paste('no2 flux for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time Since Start (hr.)', 
       y = expression(paste('NO '[2], 'flux (ug N kg  '^-1,'dry soil hr  '^-1,')')),
       color = "Treatment - Plot")

no2plot <- no2plot + theme_bw() + theme(plot.title = element_text(size = 32, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1) 

png(paste0('D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/NOx/NO2 Flux Plots/',filedate,'.png'))

print(no2plot)

dev.off()


honoplot <- ggplot(data = flux, aes(x = hour, y = hono_flux, color = chamber)) + geom_line() +
  labs(title = paste('HONO flux for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time Since Start (hr.)', 
       y = expression(paste('HONO flux (ug N kg  '^-1,'dry soil hr  '^-1,')')),
       color = "Treatment - Plot")

honoplot <- honoplot + theme_bw() + theme(plot.title = element_text(size = 32, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1) 

png(paste0('D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/NOx/HONO Flux Plots/',filedate,'.png'))

print(honoplot)

dev.off()

## Export the flux

filedate <- gsub('-','_',as.Date(datetime[1]))

filename <- paste0('D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/NOx/NOy Flux/',filedate,' NOy Fluxes.csv')

write.csv(flux, file = filename, row.names = FALSE)

}

