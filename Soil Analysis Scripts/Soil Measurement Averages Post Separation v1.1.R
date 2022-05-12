## This program is used to average files that have been been seperated by the soil seperation program
## This should be run on files containing the 'ON\OFF Soil Paramaters X' files

file <- file.choose() 
chamber <- substring(file, nchar(file)-4, nchar(file))
cycle <- substring(file, nchar(file)-23, nchar(file)-21)

## Read the CSV

csv <- read.csv(file, stringsAsFactors = FALSE)

## Change the column names so they are more easily worked with

colnames(csv) <- c('datetime', 'airtemp', 'RH', 'soiltemp', 'soilmoisture', 'VWC', 'AH')

csv$VWC <- as.numeric(csv$VWC)

## Change the date format from character (default for strings) to a POSIXlt (used for lubridate)

csv$datetime <- as.POSIXlt(csv$datetime, format = '%Y-%m-%d %H:%M:%S')

## Create a Floor, use lubridate for this purpose ##

library(lubridate)
csv$datetimefloor <- floor_date(csv$datetime, unit = '1 hour')

csv$datetimefloor <- as.POSIXct(csv$datetimefloor, format = '%Y-%m-%d %H:%M:%S')

airtemp <- aggregate(airtemp ~ datetimefloor, csv, mean)
RH <- aggregate(RH ~ datetimefloor, csv, mean)
soiltemp <- aggregate(soiltemp ~ datetimefloor, csv, mean)
soilmoisture <- aggregate(soilmoisture ~ datetimefloor, csv, mean)
VWC <- aggregate(VWC ~ datetimefloor, csv, mean)
AH <- aggregate(AH ~ datetimefloor, csv, mean)

airtempsd <- aggregate(airtemp ~ datetimefloor, csv, sd)
RHsd <- aggregate(RH ~ datetimefloor, csv, sd)
soiltempsd <- aggregate(soiltemp ~ datetimefloor, csv, sd)
soilmoisturesd <- aggregate(soilmoisture ~ datetimefloor, csv, sd)
VWCsd <- aggregate(VWC ~ datetimefloor, csv, sd)
AHsd <- aggregate(AH ~ datetimefloor, csv, sd)

airtempsum <- aggregate(airtemp ~ datetimefloor, csv, sum)
RHsum <- aggregate(RH ~ datetimefloor, csv, sum)
soiltempsum <- aggregate(soiltemp ~ datetimefloor, csv, sum)
soilmoisturesum <- aggregate(soilmoisture ~ datetimefloor, csv, sum)
VWCsum <- aggregate(VWC ~ datetimefloor, csv, sum)
AHsum <- aggregate(AH ~ datetimefloor, csv, sum)

airtempobs <- airtempsum$airtemp/airtemp$airtemp
RHobs <- RHsum$RH/RH$RH
soiltempobs <- soiltempsum$soiltemp/soiltemp$soiltemp
soilmoistureobs <- soilmoisturesum$soilmoisture/soilmoisture$soilmoisture
VWCobs <- VWCsum$VWC/VWC$VWC
AHobs <- AHsum$AH/AH$AH

airtempexport <- data.frame(airtemp, airtempsd$airtemp, airtempobs)
RHexport <- data.frame(RH, RHsd$RH, RHobs)
soiltempexport <- data.frame(soiltemp, soiltempsd$soiltemp, soiltempobs)
soilmoistureexport <- data.frame(soilmoisture, soilmoisturesd$soilmoisture, soilmoistureobs)
VWCexport <- data.frame(VWC, VWCsd$VWC, VWCobs)
AHexport <- data.frame(AH, AHsd$AH, AHobs)

if (chamber == 'A.csv'){
  colnames(airtempexport) <- c('datetime_A_airtemp', 'airtemp_A_ave', 'airtemp_A_sd', 'airtemp_A_obs')
  colnames(RHexport) <- c('datetime_A_RH', 'RH_A_ave', 'RH_A_sd', 'RH_A_obs')
  colnames(soiltempexport) <- c('datetime_A_soiltemp', 'soiltemp_A_ave', 'soiltemp_A_sd', 'soiltemp_A_obs')
  colnames(soilmoistureexport) <- c('datetime_A_soilmoisture', 'soilmoisture_A_ave', 'soilmoisture_A_sd', 'soilmoisture_A_obs')
  colnames(VWCexport) <- c('datetime_A_VWC', 'VWC_A_ave', 'VWC_A_sd', 'VWC_A_obs')
  colnames(AHexport) <- c('datetime_A_AH', 'AH_A_ave', 'AH_A_sd', 'AH_A_obs')
  print('Saving as A')
} else if (chamber == 'B.csv') {
  colnames(airtempexport) <- c('datetime_B_airtemp', 'airtemp_B_ave', 'airtemp_B_sd', 'airtemp_B_obs')
  colnames(RHexport) <- c('datetime_B_RH', 'RH_B_ave', 'RH_B_sd', 'RH_B_obs')
  colnames(soiltempexport) <- c('datetime_B_soiltemp', 'soiltemp_B_ave', 'soiltemp_B_sd', 'soiltemp_B_obs')
  colnames(soilmoistureexport) <- c('datetime_B_soilmoisture', 'soilmoisture_B_ave', 'soilmoisture_B_sd', 'soilmoisture_B_obs')
  colnames(VWCexport) <- c('datetime_B_VWC', 'VWC_B_ave', 'VWC_B_sd', 'VWC_B_obs')
  colnames(AHexport) <- c('datetime_B_AH', 'AH_B_ave', 'AH_B_sd', 'AH_B_obs')
  print('Saving as B')
} else if (chamber == 'C.csv') {
  colnames(airtempexport) <- c('datetime_C_airtemp', 'airtemp_C_ave', 'airtemp_C_sd', 'airtemp_C_obs')
  colnames(RHexport) <- c('datetime_C_RH', 'RH_C_ave', 'RH_C_sd', 'RH_C_obs')
  colnames(soiltempexport) <- c('datetime_C_soiltemp', 'soiltemp_C_ave', 'soiltemp_C_sd', 'soiltemp_C_obs')
  colnames(soilmoistureexport) <- c('datetime_C_soilmoisture', 'soilmoisture_C_ave', 'soilmoisture_C_sd', 'soilmoisture_C_obs')
  colnames(VWCexport) <- c('datetime_C_VWC', 'VWC_C_ave', 'VWC_C_sd', 'VWC_C_obs')
  colnames(AHexport) <- c('datetime_C_AH', 'AH_C_ave', 'AH_C_sd', 'AH_C_obs')
  print('Saving as C')
} else if (chamber == 'D.csv') {
  colnames(airtempexport) <- c('datetime_D_airtemp', 'airtemp_D_ave', 'airtemp_D_sd', 'airtemp_D_obs')
  colnames(RHexport) <- c('datetime_D_RH', 'RH_D_ave', 'RH_D_sd', 'RH_D_obs')
  colnames(soiltempexport) <- c('datetime_D_soiltemp', 'soiltemp_D_ave', 'soiltemp_D_sd', 'soiltemp_D_obs')
  colnames(soilmoistureexport) <- c('datetime_D_soilmoisture', 'soilmoisture_D_ave', 'soilmoisture_D_sd', 'soilmoisture_D_obs')
  colnames(VWCexport) <- c('datetime_D_VWC', 'VWC_D_ave', 'VWC_D_sd', 'VWC_D_obs')
  colnames(AHexport) <- c('datetime_D_AH', 'AH_D_ave', 'AH_D_sd', 'AH_D_obs')
  print('Saving as D')
} else {
  print('ERROR ERROR')
}

if (cycle == 'Off'){
  filename0 <- paste0('C:/Users/Zachary/Desktop/Off Airtemp',chamber)
  filename1 <- paste0('C:/Users/Zachary/Desktop/Off RH', chamber)
  filename2 <- paste0('C:/Users/Zachary/Desktop/Off Soiltemp', chamber)
  filename3 <- paste0('C:/Users/Zachary/Desktop/Off SoilMoisture', chamber)
  filename4 <- paste0('C:/Users/Zachary/Desktop/Off VWC', chamber)
  filename5 <- paste0('C:/Users/Zachary/Desktop/Off AH', chamber)
} else {
  filename0 <- paste0('C:/Users/Zachary/Desktop/On Airtemp', chamber)
  filename1 <- paste0('C:/Users/Zachary/Desktop/On RH', chamber)
  filename2 <- paste0('C:/Users/Zachary/Desktop/On Soiltemp', chamber)
  filename3 <- paste0('C:/Users/Zachary/Desktop/On SoilMoisture', chamber)
  filename4 <- paste0('C:/Users/Zachary/Desktop/On VWC', chamber)
  filename5 <- paste0('C:/Users/Zachary/Desktop/On AH', chamber)
}  

write.csv(airtempexport, filename0, row.names = FALSE)
write.csv(RHexport, filename1, row.names = FALSE)
write.csv(soiltempexport, filename2, row.names = FALSE)
write.csv(soilmoistureexport, filename3, row.names = FALSE)
write.csv(VWCexport, filename4, row.names = FALSE)
write.csv(AHexport, filename5, row.names = FALSE)
