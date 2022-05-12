## This program runs a running average of CO2 and H2O ##

file <- file.choose()
CO2 <- read.csv(file, header = TRUE)

if (ncol(CO2) == 3)

{

colnames(CO2) <- c('CO2', 'H2O', 'datetime')
datetime <- strptime((CO2$datetime), format = '%m/%d/%Y %H:%M:%S')

CO2$datetime <- datetime

CO2$datetimefloor <- as.POSIXct(floor(as.numeric(CO2$datetime) / (5 * 60)) * (5 * 60), origin='1970-01-01')
CO2meanfloor <- aggregate(CO2 ~ datetimefloor, CO2, mean)

CO2$datetimefloor <- as.POSIXct(floor(as.numeric(CO2$datetime) / (5 * 60)) * (5 * 60), origin='1970-01-01')
CO2sdfloor <- aggregate(CO2 ~ datetimefloor, CO2, sd)

CO2$datetimefloor <- as.POSIXct(floor(as.numeric(CO2$datetime) / (5 * 60)) * (5 * 60), origin='1970-01-01')
H2Omeanfloor <- aggregate(H2O ~ datetimefloor, CO2, mean)

CO2$datetimefloor <- as.POSIXct(floor(as.numeric(CO2$datetime) / (5 * 60)) * (5 * 60), origin='1970-01-01')
H2Osdfloor <- aggregate(H2O ~ datetimefloor, CO2, sd)

stats <- cbind(CO2meanfloor, CO2sdfloor$CO2, H2Omeanfloor$H2O, H2Osdfloor$H2O)
colnames(stats) <- c('datetime', 'ave_CO2', 'sd_CO2', 'ave_H2O', 'sd_H2O')

filename <- "C:/Users/Zachary/Desktop/CO2_Stats_amb.csv"
write.table(stats, file = filename, append = FALSE, sep = ",", row.names = FALSE)
	
}	else

{

## Change time to POSIXct ##

datetime_a <- strptime((CO2$datetime_a), format = '%m/%d/%Y %H:%M:%S')
datetime_b <- strptime((CO2$datetime_b), format = '%m/%d/%Y %H:%M:%S')
datetime_c <- strptime((CO2$datetime_c), format = '%m/%d/%Y %H:%M:%S')
datetime_d <- strptime((CO2$datetime_d), format = '%m/%d/%Y %H:%M:%S')

## Put new date time into old ##

CO2$datetime_a <- datetime_a
CO2$datetime_b <- datetime_b
CO2$datetime_c <- datetime_c
CO2$datetime_d <- datetime_d

## Average by hour ##

avea_CO2 <- aggregate(list(avea = CO2$CO2_ppm_a), 
          list(hourofday = cut(CO2$datetime_a, "1 hour")), 
          mean)

sda_CO2 <- aggregate(list(sda = CO2$CO2_ppm_a), 
          list(hourofday = cut(CO2$datetime_a, "1 hour")), 
          sd)

aveb_CO2 <- aggregate(list(aveb = CO2$CO2_ppm_b), 
          list(hourofday = cut(CO2$datetime_b, "1 hour")), 
          mean)

sdb_CO2 <- aggregate(list(sdb = CO2$CO2_ppm_b), 
          list(hourofday = cut(CO2$datetime_b, "1 hour")), 
          sd)

avec_CO2 <- aggregate(list(avec = CO2$CO2_ppm_c), 
          list(hourofday = cut(CO2$datetime_c, "1 hour")), 
          mean)

sdc_CO2 <- aggregate(list(sdc = CO2$CO2_ppm_c), 
          list(hourofday = cut(CO2$datetime_c, "1 hour")), 
          sd)

aved_CO2 <- aggregate(list(aved = CO2$CO2_ppm_d), 
          list(hourofday = cut(CO2$datetime_d, "1 hour")), 
          mean)

sdd_CO2 <- aggregate(list(sdd = CO2$CO2_ppm_d), 
          list(hourofday = cut(CO2$datetime_d, "1 hour")), 
          sd)

## Average H20 ##

avea_H2O <- aggregate(list(avea = CO2$H2O_ppt_a), 
          list(hourofday = cut(CO2$datetime_a, "1 hour")), 
          mean)

sda_H2O <- aggregate(list(sda = CO2$H2O_ppt_a), 
          list(hourofday = cut(CO2$datetime_a, "1 hour")), 
          sd)

aveb_H2O <- aggregate(list(aveb = CO2$H2O_ppt_b), 
          list(hourofday = cut(CO2$datetime_b, "1 hour")), 
          mean)

sdb_H2O <- aggregate(list(sdb = CO2$H2O_ppt_b), 
          list(hourofday = cut(CO2$datetime_b, "1 hour")), 
          sd)

avec_H2O <- aggregate(list(avec = CO2$H2O_ppt_c), 
          list(hourofday = cut(CO2$datetime_c, "1 hour")), 
          mean)

sdc_H2O <- aggregate(list(sdc = CO2$H2O_ppt_c), 
          list(hourofday = cut(CO2$datetime_c, "1 hour")), 
          sd)

aved_H2O <- aggregate(list(aved = CO2$H2O_ppt_d), 
          list(hourofday = cut(CO2$datetime_d, "1 hour")), 
          mean)

sdd_H2O <- aggregate(list(sdd = CO2$H2O_ppt_d), 
          list(hourofday = cut(CO2$datetime_d, "1 hour")), 
          sd)

statsa <- cbind(avea_CO2, sda_CO2$sd, avea_H2O$ave, sda_H2O$sd)
statsb <- cbind(aveb_CO2, sdb_CO2$sd, aveb_H2O$ave, sdb_H2O$sd)
statsc <- cbind(avec_CO2, sdc_CO2$sd, avec_H2O$ave, sdc_H2O$sd)
statsd <- cbind(aved_CO2, sdd_CO2$sd, aved_H2O$ave, sdd_H2O$sd)

library(plyr)

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

stats <- cbind.fill(statsa, statsb, statsc, statsd)
colnames(stats) <- c('datetime_a', 'ave_CO2_a', 'sd_CO2_a', 'ave_H2O_a', 'sd_H2O_a', 
			   'datetime_b', 'ave_CO2_b', 'sd_CO2_b', 'ave_H2O_b', 'sd_H2O_b',
			   'datetime_c', 'ave_CO2_c', 'sd_CO2_c', 'ave_H2O_c', 'sd_H2O_c', 
			   'datetime_d', 'ave_CO2_d', 'sd_CO2_d', 'ave_H2O_d', 'sd_H2O_d')

filename <- "C:/Users/Zachary/Desktop/CO2_Stats_chambers.csv"
write.table(stats, file = filename, append = FALSE, sep = ",", row.names = FALSE)

}