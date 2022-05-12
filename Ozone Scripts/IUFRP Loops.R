#### Ozone Measurements ######

## Before 7/22/16 07:00:00 PM - Ambient Measurements for 5 mins - Chamber Measurements for 10 ##
## After 7/22/16 07:00:00 PM - Ambient Measurements for 7 mins - Chamber Measurements for 8 mins ##

## Total Ozone Measurements ##

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/Ozone", pattern = "_", full.names = TRUE)

for (i in 1:length(f))
	{ assign(f[i], read.csv(f[i], header = FALSE))	}

for (i in 1:length(f))
	{ csv <- paste0(f[i],"/",list.files(f[i], pattern = "2017"))
	  ozone <- read.csv(csv, header = TRUE)

colnames(ozone) <- c("Ozone_ppb","Cell_temp", "Cell_Presure", "Flow_Rate", "PDV", "date", "time")
ozone <- data.frame(ozone$Ozone_ppb, ozone$date, ozone$time)
colnames(ozone) <- c("Ozone_ppb" , "date" , "time")
datetime <- paste(ozone$date, ozone$time)
datetime <- strptime((datetime), format="%d/%m/%Y %H:%M:%S", tz = "EST")
ozone$min <- datetime$min
ozone$day <- datetime$mday
ozone$hour <- datetime$hour
datetime$year <- datetime$year + 2000L
datetime <- format(datetime, "%m/%d/%Y %H:%M:%S")
ozone$datetime <- datetime
ozone$date <- NULL
ozone$time <- NULL


## Seperate Data by Subsets if after 7/22/16 ##

## amba <- subset(ozone, subset = min > 0 & min < 7)
## ambb <- subset(ozone, subset = min > 15 & min < 22)
## ambc <- subset(ozone, subset = min > 30 & min < 37)
## ambd <- subset(ozone, subset = min > 45 & min < 52)

## chambera <- subset(ozone, subset = min > 7 & min < 15)
## chamberb <- subset(ozone, subset = min > 22 & min < 30)
## chamberc <- subset(ozone, subset = min > 37 & min < 45)
## chamberd <- subset(ozone, subset = min > 52 & min < 60)

## Seperate Data by Subsets if before 7/22/16 ##

amba <- subset(ozone, subset = min > 1 & min < 5)
ambb <- subset(ozone, subset = min > 16 & min < 20)
ambc <- subset(ozone, subset = min > 31 & min < 35)
ambd <- subset(ozone, subset = min > 46 & min < 50)

chambera <- subset(ozone, subset = min > 6 & min < 15)
chamberb <- subset(ozone, subset = min > 21 & min < 30)
chamberc <- subset(ozone, subset = min > 36 & min < 45)
chamberd <- subset(ozone, subset = min > 51 & min < 60)

## Combined Ambient Data ##

amb <- rbind(amba, ambb, ambc, ambd)
amb <- data.frame(amb$Ozone_ppb, amb$datetime)

## Shorten Chamber Data Frames ##

chambera <- data.frame(chambera$Ozone_ppb, chambera$datetime)
chamberb <- data.frame(chamberb$Ozone_ppb, chamberb$datetime)
chamberc <- data.frame(chamberc$Ozone_ppb, chamberc$datetime)
chamberd <- data.frame(chamberd$Ozone_ppb, chamberd$datetime)

## Combined Chamber Data ##

library(plyr)

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

chambers <- cbind.fill(chambera, chamberb, chamberc, chamberd)
colnames(chambers) <- c('ozone_a', 'datetime_a', 'ozone_b', 'datetime_b', 'ozone_c', 'datetime_c', 'ozone_d', 'datetime_d')

ambient <- "Ozone_amb_2.csv"
chamberfile <- "Ozone_chambers.csv"

write.table(amb[1:2], file = paste0(f[i],'/', ambient), append = FALSE, sep = ",", row.names = FALSE)
write.table(chambers, file = paste0(f[i],'/', chamberfile), append = FALSE, sep = ",", row.names = FALSE)


}