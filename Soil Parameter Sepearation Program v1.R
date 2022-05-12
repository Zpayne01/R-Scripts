## This program is used to seperate parameters obtained from each chamber depending on the chamber status (ON/OFF)
## This requires that you run Combined Files.R before use as that program will combine all chamber meta data into a usable format
## To use this program in a different study, simply change the times in the sections ()

## Choose a file with Complete X.csv character, this will determine what chamber it came from

file <- file.choose()
chamber <- substring(file, nchar(file)-4, nchar(file))

## Read the CSV for the combined file, strings as factors = FALSE makes sure we get numerics

csv <- read.csv(file, stringsAsFactors = FALSE)

## Change the column names so its worked with more easily

colnames(csv) <- c('datetime', 'airtemp', 'RH', 'soiltemp', 'soilmoisture')

## The files still need VWC and AH

csv$VWC <- csv$soilmoisture/max(na.omit(csv$soilmoisture))
csv$AH <- (6.112 * exp((17.67*csv$airtemp)/(csv$airtemp+243.5)) * csv$RH * 2.1674)/(273.15 + csv$airtemp)

## Change the date format from character (default for strings) to a POSIXlt (used for lubridate)

csv$datetime <- as.POSIXlt(csv$datetime, format = '%m/%d/%Y %H:%M')

## Next we will seperate times based on the file name

if(chamber == 'A.csv')  {
  print('Subsetting for chamber A')
  on <- subset(csv, csv$datetime$min >= 0 & csv$datetime$min < 15)
  off <- subset(csv, csv$datetime$min >= 15)
  colnames(on) <- c('datetime_A_ON', 'airtemp_A_ON', 'RH_A_ON', 'soiltemp_A_ON', 'soilmoisture_A_ON', 'VWC_A_ON')
  colnames(off) <- c('datetime_A_OFF', 'airtemp_A_OFF', 'RH_A_OFF', 'soiltemp_A_OFF', 'soilmoisture_A_OFF', 'VWC_A_OFF')
} else if (chamber == 'B.csv')  {
  print('Subsetting for chamber B')
  on <- subset(csv, csv$datetime$min >= 15 & csv$datetime$min < 30)
  off <- subset(csv, csv$datetime$min >= 30 | csv$datetime$min < 15)
  colnames(on) <- c('datetime_B_ON', 'airtemp_B_ON', 'RH_B_ON', 'soiltemp_B_ON', 'soilmoisture_B_ON', 'VWC_B_ON')
  colnames(off) <- c('datetime_B_OFF', 'airtemp_B_OFF', 'RH_B_OFF', 'soiltemp_B_OFF', 'soilmoisture_B_OFF', 'VWC_B_OFF')
} else if (chamber == 'C.csv')  {
  print('Subsetting for chamber C')
  on <- subset(csv, csv$datetime$min >= 30 & csv$datetime$min < 45)
  off <- subset(csv, csv$datetime$min >= 45  | csv$datetime$min < 30)
  colnames(on) <- c('datetime_C_ON', 'airtemp_C_ON', 'RH_C_ON', 'soiltemp_C_ON', 'soilmoisture_C_ON', 'VWC_C_ON')
  colnames(off) <- c('datetime_C_OFF', 'airtemp_C_OFF', 'RH_C_OFF', 'soiltemp_C_OFF', 'soilmoisture_C_OFF', 'VWC_C_OFF')
} else if (chamber == 'D.csv')  {
  print('Subsetting for chamber D')
  on <- subset(csv, csv$datetime$min >= 45)
  off <- subset(csv, csv$datetime$min < 45)
  colnames(on) <- c('datetime_D_ON', 'airtemp_D_ON', 'RH_D_ON', 'soiltemp_D_ON', 'soilmoisture_D_ON', 'VWC_D_ON')
  colnames(off) <- c('datetime_D_OFF', 'airtemp_D_OFF', 'RH_D_OFF', 'soiltemp_D_OFF', 'soilmoisture_D_OFF', 'VWC_D_OFF')
} else  {
  print('ERROR, file name not reporting chamber')
}

## Create filenames for each deping on the chamber
filenameon <- paste0('C:/Users/Zachary/Desktop/On Soil Parameters ', chamber)
filenameoff <- paste0('C:/Users/Zachary/Desktop/Off Soil Paramters ', chamber)

## Export the files
write.csv(on, filenameon, row.names = FALSE)
write.csv(off, filenameoff, row.names = FALSE)
  