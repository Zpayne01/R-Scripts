## This program seeks to obtain information about absolute humidty (g/m^3) from relative humidity (%) 

file <- file.choose()
soil <- read.csv(file)

colnames(soil) <- c('datetime', 'Air_Temp', 'RH', 'Soil_Temp', 'Soil_Moisture')

soil$datetime <- as.POSIXlt(soil$datetime, format = "%m/%d/%Y %H:%M:%S")
soil$Air_Temp <- as.numeric(soil$Air_Temp)
soil$RH <- as.numeric(soil$RH)
soil$Soil_Temp <- as.numeric(soil$Soil_Temp)
soil$Soil_Moisture <- as.numeric(soil$Soil_Moisture)

datetime <- soil$datetime
soil$min <- datetime$min

soil <- subset(soil, soil$min >= 50 & soil$min < 60)

soil[ soil < 0] <- 'NaN'

soil$AH <- (6.112 * exp((17.67 * soil$Air_Temp)/(soil$Air_Temp+ 243.5)) * soil$RH *2.1674)/(273.15 + soil$Air_Temp)

library(lubridate)

soil$datetime <- floor_date(soil$datetime, unit = 'hour')
soil$datetime <- as.POSIXct(soil$datetime, format = "%Y-%m-%d %H:%M:%S")

AHave <- aggregate(AH ~ datetime, soil, mean)

## Change the file name for the correct chamber

file2 <- "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Complete D AH.csv"
write.csv(AHave, file = file2)
