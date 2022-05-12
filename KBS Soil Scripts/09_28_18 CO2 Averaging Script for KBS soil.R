## Install packages ##

library(lubridate) ## Used for flooring times for averaging
library(ggplot2)
library(tidyverse)

f <- list.files('C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/LICOR Log/')

for(i in 1:length(f))
  
{

## Upload the LICOR log file that will be processed

file <- paste0('C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/LICOR Log/',f[i])
CO2 <- read.delim(file, sep = "", header = FALSE, stringsAsFactors = FALSE)
CO2 <- CO2[-1,]         ## The first few lines need to be removed
CO2 <- CO2[-1,]
CO2$V3 <- as.numeric(CO2$V3)
CO2$V4 <- as.numeric(CO2$V4)
CO2[5:9] <- list(NULL)

colnames(CO2) <- c('date','time','CO2_ppm','H2O_ppt')

## Time time is seperated in the LICOR log between date and time, this will combine the combination readable by R ##

datetime <- paste(CO2$date, CO2$time)
datetime <- strptime((datetime), format='%Y-%m-%d %H:%M:%S', tz = 'EST')
CO2$min <- datetime$min
CO2$day <- datetime$mday
CO2$hour <- datetime$hour
datetime <- format(datetime, "%m/%d/%Y %H:%M:%S")
datetime <- as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%S")
CO2$datetime <- datetime
CO2$date <- NULL
CO2$time <- NULL

## Start and End Times

starttimefile <- read.csv(file = "C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/Start and End Times.txt", header = TRUE, stringsAsFactors = FALSE)
starttimefile$Date <- as.POSIXct(starttimefile$Date, format = "%m/%d/%Y")

matchstarttime <- match(as.Date(datetime[1]), as.Date(starttimefile$Date))

starttime <- starttimefile$starttime[matchstarttime]

startmatch <- match(starttime, CO2$hour)

CO2 <- CO2[startmatch:nrow(CO2),]

## Sepeated based on the indoor incubation experimental setup ##

chambera <- subset(CO2, subset = (min > 1 & min < 4) | (min > 16 & min < 19) | (min > 31 & min < 34) | (min > 46 & min < 49))
chamberb <- subset(CO2, subset = min > 5 & min < 15)
chamberc <- subset(CO2, subset = min > 20 & min < 30)
chamberd <- subset(CO2, subset = min > 35 & min < 45)
chambere <- subset(CO2, subset = min > 50 & min < 60)

## Shorten Chamber Data Frames to be reasonable readable##

chambera <- data.frame(chambera$CO2_ppm, chambera$H2O_ppt, chambera$datetime)
chamberb <- data.frame(chamberb$CO2_ppm, chamberb$H2O_ppt, chamberb$datetime)
chamberc <- data.frame(chamberc$CO2_ppm, chamberc$H2O_ppt, chamberc$datetime)
chamberd <- data.frame(chamberd$CO2_ppm, chamberd$H2O_ppt, chamberd$datetime)
chambere <- data.frame(chambere$CO2_ppm, chambere$H2O_ppt, chambere$datetime)

## Reapply the correct column names as the above function changes them ##

names <- c('CO2_ppm', 'H2O_ppt', 'datetime')

colnames(chambera) <- names
colnames(chamberb) <- names
colnames(chamberc) <- names
colnames(chamberd) <- names
colnames(chambere) <- names

## Apply the floor_date function from lubridate to make the dates similar for aggregation ##

chambera$datetime <- floor_date(chambera$datetime, unit = "15 minutes")
chamberb$datetime <- floor_date(chamberb$datetime, unit = "hour")
chamberc$datetime <- floor_date(chamberc$datetime, unit = "hour")
chamberd$datetime <- floor_date(chamberd$datetime, unit = "hour")
chambere$datetime <- floor_date(chambere$datetime, unit = "hour")

## Average the Chambers besed on the their floors ##

aveco2a <- aggregate(CO2_ppm ~ datetime, chambera, mean)
aveco2b <- aggregate(CO2_ppm ~ datetime, chamberb, mean)
aveco2c <- aggregate(CO2_ppm ~ datetime, chamberc, mean)
aveco2d <- aggregate(CO2_ppm ~ datetime, chamberd, mean)
aveco2e <- aggregate(CO2_ppm ~ datetime, chambere, mean)

## Also calculate the standard deviation

sdco2b <- aggregate(CO2_ppm ~ datetime, chamberb, sd)
sdco2c <- aggregate(CO2_ppm ~ datetime, chamberc, sd)
sdco2d <- aggregate(CO2_ppm ~ datetime, chamberd, sd)
sdco2e <- aggregate(CO2_ppm ~ datetime, chambere, sd)

## Add appropriate time so that the measurements match the time in which they were measured 

aveco2b$datetime <- aveco2b$datetime  
aveco2c$datetime <- aveco2c$datetime + 900
aveco2d$datetime <- aveco2d$datetime + 1800
aveco2e$datetime <- aveco2e$datetime + 2700

## Determine the difference in CO2 between the blank and the measurement based on matching the times

matchb <- match(aveco2b$datetime, aveco2a$datetime)
matchc <- match(aveco2c$datetime, aveco2a$datetime)
matchd <- match(aveco2d$datetime, aveco2a$datetime)
matche <- match(aveco2e$datetime, aveco2a$datetime)

aveco2b$CO2_ppm <- aveco2b$CO2_ppm - aveco2a$CO2_ppm[matchb]
aveco2c$CO2_ppm <- aveco2c$CO2_ppm - aveco2a$CO2_ppm[matchc]
aveco2d$CO2_ppm <- aveco2d$CO2_ppm - aveco2a$CO2_ppm[matchd]
aveco2e$CO2_ppm <- aveco2e$CO2_ppm - aveco2a$CO2_ppm[matche]

## Trim the CO2 for hour 1-22

aveco2b <- aveco2b[1:22,]
aveco2c <- aveco2c[1:22,]
aveco2d <- aveco2d[1:22,]
aveco2e <- aveco2e[1:22,]

sdco2b <- sdco2b[1:22,]
sdco2c <- sdco2c[1:22,]
sdco2d <- sdco2d[1:22,]
sdco2e <- sdco2e[1:22,]

## CO2 flux

## Flow Rates

flowfile <- read.csv(file = "C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/Flow Rates.txt", header = TRUE, stringsAsFactors = FALSE)
flowfile$Date <- as.POSIXct(flowfile$Date, format = "%m/%d/%Y")

flowdate <- match(as.Date(datetime[1]), as.Date(flowfile$Date))

flowb <- flowfile$FlowRateB[flowdate]
flowc <- flowfile$FlowRateC[flowdate]
flowd <- flowfile$FlowRateD[flowdate]
flowe <- flowfile$FlowRateE[flowdate]

## Mass of Dried Soil

massfile <- read.csv(file = "C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/Mass of Dried Soil.txt", header = TRUE, stringsAsFactors = FALSE)
massfile$Date <- as.POSIXct(massfile$Date, format = '%m/%d/%Y')

massdate <- match(as.Date(datetime[1]), as.Date(massfile$Date))

massb <- massfile$chamberBMass[massdate]
massc <- massfile$chamberCMass[massdate]
massd <- massfile$chamberDMass[massdate]
masse <- massfile$chamberEMass[massdate]

## What is in each chamber?? ##

chamberfile <- read.csv(file = "C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/Chamber Log.txt", header = TRUE, stringsAsFactors = FALSE)
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

## CO2 flux ug C g-1 dry soil hr-1 

co2FluxB <- aveco2b$CO2_ppm*(1/10^6)*(1/24.05)*(12/1)*(10^6/1)*(flowb)*(60)*(1/massb)
co2FluxC <- aveco2c$CO2_ppm*(1/10^6)*(1/24.05)*(12/1)*(10^6/1)*(flowc)*(60)*(1/massc)
co2FluxD <- aveco2d$CO2_ppm*(1/10^6)*(1/24.05)*(12/1)*(10^6/1)*(flowd)*(60)*(1/massd)
co2FluxE <- aveco2e$CO2_ppm*(1/10^6)*(1/24.05)*(12/1)*(10^6/1)*(flowe)*(60)*(1/masse)

co2FluxB[co2FluxB < -.5 | co2FluxB > 15] <- 'NA'  
co2FluxC[co2FluxC < -.5 | co2FluxC > 15] <- 'NA'  
co2FluxD[co2FluxD < -.5 | co2FluxD > 15] <- 'NA'  
co2FluxE[co2FluxE < -.5 | co2FluxE > 15] <- 'NA'  

co2FluxB <- as.numeric(co2FluxB)
co2FluxC <- as.numeric(co2FluxC)
co2FluxD <- as.numeric(co2FluxD)
co2FluxE <- as.numeric(co2FluxE)

## 

filedate <- gsub('-','_',as.Date(datetime[1]))

co2FluxComb <- data.frame(seq(1:length(co2FluxB)),co2FluxB, co2FluxC, co2FluxD, co2FluxE)
colnames(co2FluxComb) <- c('hour', paste0(contentsb,'-',plotb), paste0(contentsc,'-',plotc), paste0(contentsd,'-',plotd), paste0(contentse,'-',plote))

dateForPlots <- format(as.Date(datetime[1]), "%m/%d/%Y")

co2FluxCombLong <- gather(co2FluxComb, plot, measurement, paste0(contentsb,'-',plotb):paste0(contentse,'-',plote), factor_key=TRUE)

co2plot <- ggplot(data = co2FluxCombLong, aes(x = hour, y = measurement, color = plot)) + geom_line() +
                                            labs(title = bquote(CO[2] ~ 'flux for' ~ .(dateForPlots)),
                                            x = 'Time Since Start (hr.)', 
                                            y = expression(paste("CO"[2], ' flux', ' (ug C g'^-1,'dry soil hr'^-1,')')),
                                            color = "Treatment - Plot")
  
co2plot <- co2plot + theme_bw() + theme(plot.title = element_text(size = 32, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1) 

png(paste0('C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/CO2 Flux Plots/',filedate,'.png'))

print(co2plot)

dev.off()

## Average the Chambers besed on the their floors ##

aveh2oa <- aggregate(H2O_ppt ~ datetime, chambera, mean)
aveh2ob <- aggregate(H2O_ppt ~ datetime, chamberb, mean)
aveh2oc <- aggregate(H2O_ppt ~ datetime, chamberc, mean)
aveh2od <- aggregate(H2O_ppt ~ datetime, chamberd, mean)
aveh2oe <- aggregate(H2O_ppt ~ datetime, chambere, mean)

## Also calculate the standard deviation

sdh2ob <- aggregate(H2O_ppt ~ datetime, chamberb, sd)
sdh2oc <- aggregate(H2O_ppt ~ datetime, chamberc, sd)
sdh2od <- aggregate(H2O_ppt ~ datetime, chamberd, sd)
sdh2oe <- aggregate(H2O_ppt ~ datetime, chambere, sd)

## Add appropriate time so that the measurements match the time in which they were measured 

aveh2ob$datetime <- aveh2ob$datetime  
aveh2oc$datetime <- aveh2oc$datetime + 900
aveh2od$datetime <- aveh2od$datetime + 1800
aveh2oe$datetime <- aveh2oe$datetime + 2700

## Determine the difference in h2o between the blank and the measurement based on matching the times

matchb <- match(aveh2ob$datetime, aveh2oa$datetime)
matchc <- match(aveh2oc$datetime, aveh2oa$datetime)
matchd <- match(aveh2od$datetime, aveh2oa$datetime)
matche <- match(aveh2oe$datetime, aveh2oa$datetime)

aveh2ob$H2O_ppt <- aveh2ob$H2O_ppt - aveh2oa$H2O_ppt[matchb]
aveh2oc$H2O_ppt <- aveh2oc$H2O_ppt - aveh2oa$H2O_ppt[matchc]
aveh2od$H2O_ppt <- aveh2od$H2O_ppt - aveh2oa$H2O_ppt[matchd]
aveh2oe$H2O_ppt <- aveh2oe$H2O_ppt - aveh2oa$H2O_ppt[matche]

## Trim the h2o for hour 1-22

aveh2ob <- aveh2ob[1:22,]
aveh2oc <- aveh2oc[1:22,]
aveh2od <- aveh2od[1:22,]
aveh2oe <- aveh2oe[1:22,]

sdh2ob <- sdh2ob[1:22,]
sdh2oc <- sdh2oc[1:22,]
sdh2od <- sdh2od[1:22,]
sdh2oe <- sdh2oe[1:22,]

## Convert water in ppth to absolute humidity (g m^-3)

aveh2ob$H2O_ppt <- aveh2ob$H2O_ppt/1000/24.05*1000*18
aveh2oc$H2O_ppt <- aveh2oc$H2O_ppt/1000/24.05*1000*18
aveh2od$H2O_ppt <- aveh2od$H2O_ppt/1000/24.05*1000*18
aveh2oe$H2O_ppt <- aveh2oe$H2O_ppt/1000/24.05*1000*18

colnames(aveh2ob) <- c('datetime', 'AH')
colnames(aveh2oc) <- c('datetime', 'AH')
colnames(aveh2od) <- c('datetime', 'AH')
colnames(aveh2oe) <- c('datetime', 'AH')

sdh2ob$H2O_ppt <- sdh2ob$H2O_ppt/1000/24.05*1000*18
sdh2oc$H2O_ppt <- sdh2oc$H2O_ppt/1000/24.05*1000*18
sdh2od$H2O_ppt <- sdh2od$H2O_ppt/1000/24.05*1000*18
sdh2oe$H2O_ppt <- sdh2oe$H2O_ppt/1000/24.05*1000*18

colnames(sdh2ob) <- c('datetime', 'AH')
colnames(sdh2oc) <- c('datetime', 'AH')
colnames(sdh2od) <- c('datetime', 'AH')
colnames(sdh2oe) <- c('datetime', 'AH')

## Plot the absolute humidity

h2oComb <- data.frame(seq(1:nrow(aveh2ob)),aveh2ob$AH, aveh2oc$AH, aveh2od$AH, aveh2oe$AH)
colnames(h2oComb) <- c('hour', paste0(contentsb,'-',plotb), paste0(contentsc,'-',plotc), paste0(contentsd,'-',plotd), paste0(contentse,'-',plote))

h2oCombLong <- gather(h2oComb, plot, measurement, paste0(contentsb,'-',plotb):paste0(contentse,'-',plote), factor_key=TRUE)

h2oplot <- ggplot(data = h2oCombLong, aes(x = hour, y = measurement, color = plot)) + geom_line() +
  labs(title = bquote('Absolute Humidity for' ~ .(dateForPlots)),
       x = 'Time Since Start (hr.)', 
       y = expression(paste("Absolute Humidity (g H  "[2],"O m"^-3,")")),
       color = "Treatment - Plot")

h2oplot <- h2oplot + theme_bw() + theme(plot.title = element_text(size = 26, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1)

png(paste0('C:/Users/Zachary/Documents/Lab Data/09_24_18 KBS Soil Experiment/Absolute Humidity Plots/',filedate,'.png'))

print(h2oplot)

dev.off()

## Create a row countaining hours

hour <- seq(1,22,1)

## Combine into a singal exportable file

export <- co2FluxComb
colnames(export) <- c("hour", "CO2_ug_C_g-1_soil_hr-1_B", "CO2_ug_C_g-1_soil_hr-1_C", "CO2_ug_C_g-1_soil_hr-1_D", "CO2_ug_C_g-1_soil_hr-1_E")

filedate <- gsub('-','_',as.Date(datetime[1]))
filename <- paste0("C:/Users/Zachary/Desktop/Working CO2/",filedate," CO2.csv")
filename2 <- paste0("C:/Users/Zachary/Desktop/Working H2O/",filedate," H2O.csv")

export2 <- data.frame(hour, aveh2ob$AH, sdh2ob$AH, aveh2oc$AH, sdh2oc$AH, aveh2od$AH, sdh2od$AH, aveh2oe$AH, sdh2oe$AH)
colnames(export2) <- c("hour", "H2O_g_m^-3_b", "H2O_sd_b", "H2O_g_m^-3_c", "H2O_sd_c",  "H2O_g_m^-3_d", "H2O_sd_d",  "H2O_g_m^-3_e", "H2O_sd_e")

write.csv(export, file = filename, row.names = FALSE)
write.csv(export2, file = filename2, row.names = FALSE)

}