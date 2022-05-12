## This program runs a running average of the ozone ##

file <- file.choose()
ozone <- read.csv(file, header = TRUE)

if (ncol(ozone) == 2)

{

colnames(ozone) <- c('datetime', 'ozone')
datetime <- strptime((ozone$datetime), format = '%m/%d/%Y %H:%M:%S')

ozone$datetime <- datetime

## Seperates based off the 15 minute floors ##

ozone$datetimefloor <- as.POSIXct(floor(as.numeric(ozone$datetime) / (5 * 60))* (5 * 60), origin='1970-01-01')
ozonemeanfloor <- aggregate(ozone ~ datetimefloor, ozone, mean)
ozonesdfloor <- aggregate(ozone ~ datetimefloor, ozone, sd)

stats <- cbind(ozonemeanfloor, ozonesdfloor$ozone)
colnames(stats) <- c('datetime','ave','sd')

filename <- "C:/Users/Zachary/Desktop/Ozone_Stats_amb.csv"
write.table(stats, file = filename, append = FALSE, sep = ",", row.names = FALSE)

}	else

{

## Change time to POSIXct ##

datetime_a <- strptime((ozone$datetime_a), format = '%m/%d/%Y %H:%M:%S')
datetime_b <- strptime((ozone$datetime_b), format = '%m/%d/%Y %H:%M:%S')
datetime_c <- strptime((ozone$datetime_c), format = '%m/%d/%Y %H:%M:%S')
datetime_d <- strptime((ozone$datetime_d), format = '%m/%d/%Y %H:%M:%S')

## Put new date time into old ##

ozone$datetime_a <- datetime_a
ozone$datetime_b <- datetime_b
ozone$datetime_c <- datetime_c
ozone$datetime_d <- datetime_d

## Average by hour ##

avea <- aggregate(list(avea = ozone$ozone_a), 
          list(hourofday = cut(ozone$datetime_a, "1 hour")), 
          mean)

sda <- aggregate(list(sda = ozone$ozone_a), 
          list(hourofday = cut(ozone$datetime_a, "1 hour")), 
          sd)

aveb <- aggregate(list(aveb = ozone$ozone_b), 
          list(hourofday = cut(ozone$datetime_b, "1 hour")), 
          mean)

sdb <- aggregate(list(sdb = ozone$ozone_b), 
          list(hourofday = cut(ozone$datetime_b, "1 hour")), 
          sd)

avec <- aggregate(list(avec = ozone$ozone_c), 
          list(hourofday = cut(ozone$datetime_c, "1 hour")), 
          mean)

sdc <- aggregate(list(sdc = ozone$ozone_c), 
          list(hourofday = cut(ozone$datetime_c, "1 hour")), 
          sd)

aved <- aggregate(list(aved = ozone$ozone_d), 
          list(hourofday = cut(ozone$datetime_d, "1 hour")), 
          mean)

sdd <- aggregate(list(sdd = ozone$ozone_d), 
          list(hourofday = cut(ozone$datetime_d, "1 hour")), 
          sd)

statsa <- cbind(avea, sda$sd) 
statsb <- cbind(aveb, sdb$sd)
statsc <- cbind(avec, sdc$sd)
statsd <- cbind(aved, sdd$sd)

colnames(statsa) <- c('datetime_a', 'ave_Ozone_a', 'sd_Ozone_a')
colnames(statsb) <- c('datetime_b', 'ave_Ozone_b', 'sd_Ozone_b')
colnames(statsc) <- c('datetime_c', 'ave_Ozone_c', 'sd_Ozone_c')
colnames(statsd) <- c('datetime_d', 'ave_Ozone_d', 'sd_Ozone_d')

library(plyr)

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

stats <- cbind.fill(statsa,statsb, statsc, statsd)
colnames(stats) <- c('datetime_a', 'ave_Ozone_a', 'sd_Ozone_a', 'datetime_b', 'ave_Ozone_b', 'sd_Ozone_b',
			'datetime_c', 'ave_Ozone_c', 'sd_Ozone_c', 'datetime_d', 'ave_Ozone_d', 'sd_Ozone_d')

filename <- "C:/Users/Zachary/Desktop/Ozone_Stats_chambers.csv"
write.table(stats, file = filename, append = FALSE, sep = ",", row.names = FALSE)

}