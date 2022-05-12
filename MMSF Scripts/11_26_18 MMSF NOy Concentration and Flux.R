## This program was used with the Morgan Monroe State Forest Soil ####

library(TTR)
library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)

## Choose the file or list of files ## 

filelist <- list.files('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/Raw NOx', pattern = 'data')

##file <- file.choose()

for (i in 1:length(filelist)) {
file <- paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/Raw NOx/',filelist[i])
noxfile <- read.delim(file, header = TRUE, stringsAsFactors = FALSE)
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

## Start and End Times

## Delete data up until a whole measurement ##

while(!(noxfile$sec[1] == 0 & (noxfile$min[1] == 0) | (noxfile$min[1] == 20) | (noxfile$min[1] == 40)))
{noxfile <- noxfile[-c(1),]}
while(!(noxfile$sec[nrow(noxfile)] == 59 &  (noxfile$min[nrow(noxfile)] == 19) | (noxfile$min[nrow(noxfile)] == 39) | (noxfile$min[nrow(noxfile)] == 59)))
{noxfile <- noxfile[-c(nrow(noxfile)),]}

## Insert non values for missing points ##

start <- as.POSIXct(noxfile$TheTime[1])

end <- as.POSIXct(noxfile$TheTime[nrow(noxfile)]) 

ts <- seq.POSIXt(start, end, by = "sec")

timedf <- data.frame(timestamp=ts)
colnames(timedf) <- c('TheTime')

noxfile <- full_join(timedf, noxfile)

## Approximate missing values

noxfilenaapprox <- na.approx(noxfile$CH1_Hz)
noxfile$CH1_Hz <- noxfilenaapprox

## Put in Minutes again ##

datetime <- as.POSIXlt(noxfile$TheTime, tz = "est")
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec
filedate <- gsub('-','_',as.Date(datetime[1]))

## Trim the signal into useful subsets for subsequent averaging (we only want to average 45 of the zero, no, blc1 and blc2)##

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

## Insert the calibration parameters

c <- 2745.7

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

## Stop, its chamber time ## 

chambera <- data.frame(subset(finalcounts, finalcounts$time$min == 10), 'AM')
chamberb <- data.frame(subset(finalcounts, finalcounts$time$min == 30), 'ECM')
chamberc <- data.frame(subset(finalcounts, finalcounts$time$min == 50), 'Blank')

chamberamba <- subset(finalcounts, finalcounts$time$min == 0)
chamberambb <- subset(finalcounts, finalcounts$time$min == 20)
chamberambc <- subset(finalcounts, finalcounts$time$min == 40)

## Export the concentrations before determining the flux ##

chambernames <- c('time', 'no_ppb', 'no_ppb_sd', 'no2_ppb', 'no2_ppb_sd', 'hono_ppb', 'hono_ppb_sd', 'chamber')

colnames(chambera) <- chambernames
colnames(chamberb) <- chambernames
colnames(chamberc) <- chambernames

chamberplot <- rbind(chambera, chamberb, chamberc)

## GGPlot2 NO Plot

noplot <- ggplot(data = chamberplot, aes(x = time, y = no_ppb, color = chamber)) + geom_line() +
  labs(title = paste('NO Concentration for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time', 
       y = '[NO] (ppb)',
       color = "Chamber Contents")

noplot <- noplot + theme_bw() + theme(plot.title = element_text(size = 24, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1) 

png(paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/NOy Concentration Plots/NO/',filedate,'.png'))

print(noplot)

dev.off()

## GGPlot2 NO2 Plot

no2plot <- ggplot(data = chamberplot, aes(x = time, y = no2_ppb, color = chamber)) + geom_line() +
  labs(title = paste('NO2 Concentration for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time', 
       y = '[NO2] (ppb)',
       color = "Chamber Contents")

no2plot <- no2plot + theme_bw() + theme(plot.title = element_text(size = 24, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1) 

png(paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/NOy Concentration Plots/NO2/',filedate,'.png'))

print(no2plot)

dev.off()

## GGPlot2 HONO Plot

honoplot <- ggplot(data = chamberplot, aes(x = time, y = hono_ppb, color = chamber)) + geom_line() +
  labs(title = paste('hono Concentration for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time', 
       y = '[hono] (ppb)',
       color = "Chamber Contents")

honoplot <- honoplot + theme_bw() + theme(plot.title = element_text(size = 24, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1) 

png(paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/NOy Concentration Plots/hono/',filedate,'.png'))

print(honoplot)

dev.off()

# ## Determine the true concentration of each species by correcting for absolute humidity (This should be done if I can find AH measurements)
# 
# h2ofile <- list.files("C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/Absolute Humidity", pattern = gsub("-","_",as.Date(datetime[1])))
# h2o <- read.csv(paste0("C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/Absolute Humidity/",h2ofile), header = TRUE, stringsAsFactors = FALSE)
# 
# chambera[2:7] <- data.frame(sapply(chambera[2:7], function(x) {x/(-0.0052*h2o[2]+1)}))
# chamberb[2:7] <- data.frame(sapply(chamberb[2:7], function(x) {x/(-0.0052*h2o[4]+1)}))
# chamberc[2:7] <- data.frame(sapply(chamberc[2:7], function(x) {x/(-0.0052*h2o[6]+1)}))
# chamberd[2:7] <- data.frame(sapply(chamberd[2:7], function(x) {x/(-0.0052*h2o[8]+1)}))

## For the chamber measurements we will want to subtract the blank chamber measurements from the chamber measurements to get the true concentration

chambera$no_ppb <- chambera$no_ppb - chamberamba$no_ppb
chambera$no_ppb_sd <- sqrt(chambera$no_ppb_sd^2 + chamberamba$no_ppb_sd^2)
chambera$no2_ppb <- chambera$no2_ppb - chamberamba$no2_ppb
chambera$no2_ppb_sd <- sqrt(chambera$no2_ppb_sd^2 + chamberamba$no2_ppb_sd^2)
chambera$hono_ppb <- chambera$hono_ppb - chamberamba$hono_ppb
chambera$hono_ppb_sd <- sqrt(chambera$hono_ppb_sd^2 + chamberamba$hono_ppb_sd^2)

chamberb$no_ppb <- chamberb$no_ppb - chamberambb$no_ppb
chamberb$no_ppb_sd <- sqrt(chamberb$no_ppb_sd^2 + chamberambb$no_ppb_sd^2)
chamberb$no2_ppb <- chamberb$no2_ppb - chamberambb$no2_ppb
chamberb$no2_ppb_sd <- sqrt(chamberb$no2_ppb_sd^2 + chamberambb$no2_ppb_sd^2)
chamberb$hono_ppb <- chamberb$hono_ppb - chamberambb$hono_ppb
chamberb$hono_ppb_sd <- sqrt(chamberb$hono_ppb_sd^2 + chamberambb$hono_ppb_sd^2)

chamberc$no_ppb <- chamberc$no_ppb - chamberambc$no_ppb
chamberc$no_ppb_sd <- sqrt(chamberc$no_ppb_sd^2 + chamberambc$no_ppb_sd^2)
chamberc$no2_ppb <- chamberc$no2_ppb - chamberambc$no2_ppb
chamberc$no2_ppb_sd <- sqrt(chamberc$no2_ppb_sd^2 + chamberambc$no2_ppb_sd^2)
chamberc$hono_ppb <- chamberc$hono_ppb - chamberambc$hono_ppb
chamberc$hono_ppb_sd <- sqrt(chamberc$hono_ppb_sd^2 + chamberambc$hono_ppb_sd^2)

#Flux measurements

## Flow Rates

flowa <- 21
flowb <- 21
flowc <- 21

## Flux Measurement (ug N m-2 hr-1)

chamberaflux <- data.frame(chambera[1],sapply(chambera[2:7], function(x) {x/(10^9)/22.4*flowa*14*60/1065*10000*10^6}),chambera[8])
chamberbflux <- data.frame(chamberb[1],sapply(chamberb[2:7], function(x) {x/(10^9)/22.4*flowb*14*60/1065*10000*10^6}),chamberb[8])
chambercflux <- data.frame(chamberc[1],sapply(chamberc[2:7], function(x) {x/(10^9)/22.4*flowc*14*60/1065*10000*10^6}),chamberc[8])

names <- c('time', 'no_flux', 'no_flux_sd', 'no2_flux', 'no2_flux_sd', 'hono_flux', 'hono_flux_sd', "chamber")

colnames(chamberaflux) <- names
colnames(chamberbflux) <- names
colnames(chambercflux) <- names

flux <- rbind(chamberaflux, chamberbflux, chambercflux) 

## Plot the flux

noplot <- ggplot(data = flux, aes(x = time, y = no_flux, color = chamber)) + geom_line() +
  labs(title = paste('NO flux for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time Since Start (hr.)',
       y = expression(paste('NO flux (ug N m  '^-2,'hr  '^-1,')')),
       color = "Treatment - Plot")

noplot <- noplot + theme_bw() + theme(plot.title = element_text(size = 24, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1)

png(paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/NOy Flux Plots/NO/',filedate,'.png'))

print(noplot)

dev.off()


no2plot <- ggplot(data = flux, aes(x = time, y = no2_flux, color = chamber)) + geom_line() +
  labs(title = paste('no2 flux for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time Since Start (hr.)',
       y = expression(paste('NO '[2], 'flux (ug N m  '^-2,'hr  '^-1,')')),
       color = "Treatment - Plot")

no2plot <- no2plot + theme_bw() + theme(plot.title = element_text(size = 24, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1)

png(paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/NOy Flux Plots/NO2/',filedate,'.png'))

print(no2plot)

dev.off()


honoplot <- ggplot(data = flux, aes(x = time, y = hono_flux, color = chamber)) + geom_line() +
  labs(title = paste('HONO flux for', format(as.Date(datetime[1]), "%m/%d/%Y")),
       x = 'Time Since Start (hr.)',
       y = expression(paste('HONO flux (ug N m  '^-2,'hr  '^-1,')')),
       color = "Treatment - Plot")

honoplot <- honoplot + theme_bw() + theme(plot.title = element_text(size = 24, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1)

png(paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/NOy Flux Plots/HONO/',filedate,'.png'))

print(honoplot)

dev.off()

## Export the flux

filedate <- gsub('-','_',as.Date(datetime[1]))

filename <- paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/NOy Flux/',filedate,' NOy Fluxes.csv')

write.csv(flux, file = filename, row.names = FALSE)

}
