# IURTP 2020 field work

weatherfile <- 'D:/Surface 3/Main Folder/Documents/Lab Data/2020_09 IURTP/met_data/1 min average/Weather Station.csv'
chamberbfile <- 'D:/Surface 3/Main Folder/Documents/Lab Data/2020_09 IURTP/met_data/1 min average/Chamber B.csv'
chambercfile <- 'D:/Surface 3/Main Folder/Documents/Lab Data/2020_09 IURTP/met_data/1 min average/Chamber C.csv'
chamberdfile <- 'D:/Surface 3/Main Folder/Documents/Lab Data/2020_09 IURTP/met_data/1 min average/Chamber D.csv'

weathercsv <- read.csv(weatherfile, stringsAsFactors = FALSE)
chamberbcsv <- read.csv(chamberbfile, stringsAsFactors = FALSE)
chamberccsv <- read.csv(chambercfile, stringsAsFactors = FALSE)
chamberdcsv <- read.csv(chamberdfile, stringsAsFactors = FALSE)

# 

weathercsv$Datetime <- as.POSIXlt(weathercsv$Datetime, format = "%m/%d/%Y %H:%M")
chamberbcsv$Datetime <- as.POSIXlt(chamberbcsv$Datetime, format = "%m/%d/%Y %H:%M")
chamberccsv$Datetime <- as.POSIXlt(chamberccsv$Datetime, format = "%m/%d/%Y %H:%M")
chamberdcsv$Datetime <- as.POSIXlt(chamberdcsv$Datetime, format = "%m/%d/%Y %H:%M")

#

weathermeas <- subset(weathercsv, weathercsv$Datetime$min %% 20 %in% c(0,1,2,3,4))
chamberbmeas <- subset(chamberbcsv, chamberbcsv$Datetime$min %% 20 %in% c(5,6,7,8,9))
chambercmeas <- subset(chamberccsv, chamberccsv$Datetime$min %% 20 %in% c(10,11,12,13,14))
chamberdmeas <- subset(chamberdcsv, chamberdcsv$Datetime$min %% 20 %in% c(15,16,17,18,19))

# Temperature and pressure Data Frame 

temp <- data.frame(
  datetime = c(weathermeas$Datetime, chamberbmeas$Datetime, chambercmeas$Datetime, chamberdmeas$Datetime)
)