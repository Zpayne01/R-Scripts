## Install packages ##

library(lubridate) ## Used for flooring times for averaging
library(ggplot2)
library(tidyverse)

f <- list.files('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/MMSF test LICOR/')

for(i in 1:length(f))

{

## Upload the LICOR log file that will be processed

file <- paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/MMSF test LICOR/',f[i])
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

## Sepeated based on the indoor incubation experimental setup ##

chambera <- subset(CO2, subset = (min > 1 & min < 4) | (min > 21 & min < 24) | (min > 41 & min < 44))
chamberb <- subset(CO2, subset = min > 15 & min < 19)
chamberc <- subset(CO2, subset = min > 35 & min < 39)
chamberd <- subset(CO2, subset = min > 55 & min < 59)

## Shorten Chamber Data Frames to be reasonable readable##

chambera <- data.frame(chambera$CO2_ppm, chambera$H2O_ppt, chambera$datetime)
chamberb <- data.frame(chamberb$CO2_ppm, chamberb$H2O_ppt, chamberb$datetime)
chamberc <- data.frame(chamberc$CO2_ppm, chamberc$H2O_ppt, chamberc$datetime)
chamberd <- data.frame(chamberd$CO2_ppm, chamberd$H2O_ppt, chamberd$datetime)

## Reapply the correct column names as the above function changes them ##

names <- c('CO2_ppm', 'H2O_ppt', 'datetime')

colnames(chambera) <- names
colnames(chamberb) <- names
colnames(chamberc) <- names
colnames(chamberd) <- names

## Apply the floor_date function from lubridate to make the dates similar for aggregation ##

chambera$datetime <- floor_date(chambera$datetime, unit = "5 minutes")
chamberb$datetime <- floor_date(chamberb$datetime, unit = "hour")
chamberc$datetime <- floor_date(chamberc$datetime, unit = "hour")
chamberd$datetime <- floor_date(chamberd$datetime, unit = "hour")

## Average the Chambers besed on the their floors ##

aveco2a <- aggregate(CO2_ppm ~ datetime, chambera, mean)
aveco2b <- aggregate(CO2_ppm ~ datetime, chamberb, mean)
aveco2c <- aggregate(CO2_ppm ~ datetime, chamberc, mean)
aveco2d <- aggregate(CO2_ppm ~ datetime, chamberd, mean)

## Also calculate the standard deviation

sdco2b <- aggregate(CO2_ppm ~ datetime, chamberb, sd)
sdco2c <- aggregate(CO2_ppm ~ datetime, chamberc, sd)
sdco2d <- aggregate(CO2_ppm ~ datetime, chamberd, sd)

## Add appropriate time so that the measurements match the time in which they were measured 

aveco2b$datetime <- aveco2b$datetime  
aveco2c$datetime <- aveco2c$datetime + 1200
aveco2d$datetime <- aveco2d$datetime + 2400

## Determine the difference in CO2 between the blank and the measurement based on matching the times

matchb <- match(aveco2b$datetime, aveco2a$datetime)
matchc <- match(aveco2c$datetime, aveco2a$datetime)
matchd <- match(aveco2d$datetime, aveco2a$datetime)

aveco2b$CO2_ppm <- aveco2b$CO2_ppm - aveco2a$CO2_ppm[matchb]
aveco2c$CO2_ppm <- aveco2c$CO2_ppm - aveco2a$CO2_ppm[matchc]
aveco2d$CO2_ppm <- aveco2d$CO2_ppm - aveco2a$CO2_ppm[matchd]

## CO2 flux

## Flow Rates

flowb <- 21
flowc <- 21
flowd <- 21

## CO2 flux g C m-2 hr-1 

co2FluxB <- aveco2b$CO2_ppm*(1/10^6)*(1/22.4)*(12/1)*(flowb)*(60)/1065*10000
co2FluxC <- aveco2c$CO2_ppm*(1/10^6)*(1/22.4)*(12/1)*(flowc)*(60)/1065*10000
co2FluxD <- aveco2d$CO2_ppm*(1/10^6)*(1/22.4)*(12/1)*(flowd)*(60)/1065*10000

co2FluxB <- data.frame(aveco2b$datetime, as.numeric(co2FluxB), 'AM')
co2FluxC <- data.frame(aveco2c$datetime, as.numeric(co2FluxC), 'ECM')
co2FluxD <- data.frame(aveco2d$datetime, as.numeric(co2FluxD), 'Blank')

names <- c('datetime', 'co2flux', 'plot')

colnames(co2FluxB) <- names
colnames(co2FluxC) <- names
colnames(co2FluxD) <- names

## 

filedate <- gsub('-','_',as.Date(datetime[1]))

co2FluxComb <- rbind(co2FluxB, co2FluxC, co2FluxD)

dateForPlots <- format(as.Date(datetime[1]), "%m/%d/%Y")

co2plot <- ggplot(data = co2FluxComb, aes(x = datetime, y = co2flux, color = plot)) + geom_line() +
                                            labs(title = bquote(CO[2] ~ 'flux for' ~ .(dateForPlots)),
                                            x = 'Time Since Start (hr.)', 
                                            y = expression(paste("CO"[2], ' flux', ' (g C m'^-2, 'hr'^-1,')')),
                                            color = "Plot")
  
co2plot <- co2plot + theme_bw() + theme(plot.title = element_text(size = 32, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1) 

png(paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/CO2 Flux Plots/',filedate,'.png'))

print(co2plot)

dev.off()

## Average the Chambers besed on the their floors ##

aveh2oa <- aggregate(H2O_ppt ~ datetime, chambera, mean)
aveh2ob <- aggregate(H2O_ppt ~ datetime, chamberb, mean)
aveh2oc <- aggregate(H2O_ppt ~ datetime, chamberc, mean)
aveh2od <- aggregate(H2O_ppt ~ datetime, chamberd, mean)

## Also calculate the standard deviation

sdh2ob <- aggregate(H2O_ppt ~ datetime, chamberb, sd)
sdh2oc <- aggregate(H2O_ppt ~ datetime, chamberc, sd)
sdh2od <- aggregate(H2O_ppt ~ datetime, chamberd, sd)

## Add appropriate time so that the measurements match the time in which they were measured 

aveh2ob$datetime <- aveh2ob$datetime  
aveh2oc$datetime <- aveh2oc$datetime + 1200
aveh2od$datetime <- aveh2od$datetime + 2400

## Determine the difference in h2o between the blank and the measurement based on matching the times

# matchb <- match(aveh2ob$datetime, aveh2oa$datetime)
# matchc <- match(aveh2oc$datetime, aveh2oa$datetime)
# matchd <- match(aveh2od$datetime, aveh2oa$datetime)
# 
# aveh2ob$H2O_ppt <- aveh2ob$H2O_ppt - aveh2oa$H2O_ppt[matchb]
# aveh2oc$H2O_ppt <- aveh2oc$H2O_ppt - aveh2oa$H2O_ppt[matchc]
# aveh2od$H2O_ppt <- aveh2od$H2O_ppt - aveh2oa$H2O_ppt[matchd]

## Convert water in ppth to absolute humidity (g m^-3)

aveh2ob$H2O_ppt <- aveh2ob$H2O_ppt/1000/24.05*1000*18
aveh2oc$H2O_ppt <- aveh2oc$H2O_ppt/1000/24.05*1000*18
aveh2od$H2O_ppt <- aveh2od$H2O_ppt/1000/24.05*1000*18

colnames(aveh2ob) <- c('datetime', 'AH')
colnames(aveh2oc) <- c('datetime', 'AH')
colnames(aveh2od) <- c('datetime', 'AH')

sdh2ob$H2O_ppt <- sdh2ob$H2O_ppt/1000/24.05*1000*18
sdh2oc$H2O_ppt <- sdh2oc$H2O_ppt/1000/24.05*1000*18
sdh2od$H2O_ppt <- sdh2od$H2O_ppt/1000/24.05*1000*18

colnames(sdh2ob) <- c('datetime', 'AH')
colnames(sdh2oc) <- c('datetime', 'AH')
colnames(sdh2od) <- c('datetime', 'AH')

AHB <- data.frame(aveh2ob, 'AM')
AHC <- data.frame(aveh2oc, 'ECM')
AHD <- data.frame(aveh2od, 'Blank')

names <- c('datetime', 'AH', 'plot')

colnames(AHB) <- names
colnames(AHC) <- names
colnames(AHD) <- names

## Plot the absolute humidity

h2oComb <- rbind(AHB, AHC, AHD)

h2oplot <- ggplot(data = h2oComb, aes(x = datetime, y = AH, color = plot)) + geom_line() +
  labs(title = bquote('Absolute Humidity for' ~ .(dateForPlots)),
       x = 'Time Since Start (hr.)', 
       y = expression(paste("Absolute Humidity (g H  "[2],"O m"^-3,")")),
       color = "Plot")

h2oplot <- h2oplot + theme_bw() + theme(plot.title = element_text(size = 26, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1)

png(paste0('C:/Users/Zachary/Documents/Lab Data/10_2018 MMSF Field Study/Absolute Humidity Plots/',filedate,'.png'))

print(h2oplot)

dev.off()

## Combine into a singal exportable file

export <- co2FluxComb

filedate <- gsub('-','_',as.Date(datetime[1]))
filename <- paste0("C:/Users/Zachary/Desktop/Working CO2/",filedate," CO2.csv")
filename2 <- paste0("C:/Users/Zachary/Desktop/Working H2O/",filedate," H2O.csv")

export2 <- h2oComb

write.csv(export, file = filename, row.names = FALSE)
write.csv(export2, file = filename2, row.names = FALSE)

}