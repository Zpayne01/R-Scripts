## This script is used for the 2018 modeling project for the 2017 IURTP data it utilizes the ON soil parameters data
## located in the IURTP/soil parameters/(Chamber) Finished folders and the NOy flux data that has been seperated by rain
## events located in the IURTP/12_06_18 Modeling Work/NO flux and temperature folder

library(lubridate)

## file should be your NO flux file

file <- file.choose()

csv <- read.csv(file, header = TRUE, stringsAsFactors =  FALSE)
csv$Time <- as.POSIXct(csv$Time, format = "%m/%d/%Y %H:%M")

## file2 should be the soil parameters file

file2 <- file.choose()

csv2 <- read.csv(file2, header = TRUE, stringsAsFactors = FALSE)

## Change colnames based on the data being measured 

colnames(csv2) <- c("datetime", "air_temp", "RH", "soil_temp", "soil_moisture", "VWC", "AH")

## Check date format in R before running this program

csv2$datetime <- as.POSIXlt(csv2$datetime, format = "%Y-%m-%d %H:%M:%S")

## The date has to be in the lt format to find a starting date, change the starting date based on the
## beginning of your measurement period

dateMatch <- match(as.Date("2017-08-01"), as.Date(csv2$datetime))
csv2 <- csv2[dateMatch:nrow(csv2),]

csv2$datetime <- floor_date(csv2$datetime, unit = "1 hour")
csv2$datetime <- as.POSIXct(csv2$datetime, format = "%Y-%m-%d %H:%M:%S")
csv2 <- aggregate(. ~ datetime, csv2, mean)

## If you have problems, check the csv$Time and csv$datetime to make sure they are the same format
## and the same time scale (i.e. every hour or every 15 minutes)

timeMatch <- match(csv$Time, csv2$datetime)
names <- colnames(csv2)

for (i in 2:ncol(csv2)) {
csv[, names[i]] <- csv2[timeMatch, names[i]]
}

filename <- 'C:/Users/zacpayne/Desktop/Chamber MetaData Comparison Chamber NAME (No Rain).csv'

write.csv(csv, file = filename, row.names = FALSE)
