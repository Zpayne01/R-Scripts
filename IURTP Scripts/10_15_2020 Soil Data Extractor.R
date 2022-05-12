## Soil Met Processing ##

soilfile <- file.choose()
soilcsv <- read.csv(soilfile, header = FALSE, stringsAsFactors = FALSE)

soilcsv <- soilcsv[c(1,2,4,6,8,10)]
colnames(soilcsv) <- c('Date', 'Time', 'Air_Temp', 'RH', 'Soil_Temp', 'Soil_Moisture')
soilcsv$dateime <- as.POSIXct(paste(soilcsv$Date, soilcsv$Time), format = "%m/%d/%Y %H:%M:%S")

soilexp <- ddply(soilcsv, .(floor_date(soilcsv$dateime, unit = '1 minute')), summarize,
            Air_Temp = mean(Air_Temp),
            RH = mean(RH),
            Soil_Temp = mean(Soil_Temp),
            Soil_Moisture = mean(Soil_Moisture))

colnames(soilexp) <- c('Datetime', 'Air_Temp', 'RH', 'Soil_Temp', 'Soil_Moisture') 

write.csv(soilexp, file = 'C:/Users/zacpayne/Desktop/Chamber D.csv', row.names = FALSE)
