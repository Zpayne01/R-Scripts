file <- file.choose()
csv <- read.csv(file)

library(lubridate)

head(csv)

class(csv$datetime)

csv$datetime <- as.POSIXct(csv$datetime, format = '%m/%d/%Y %H:%M:%S')

csv$datetime <- round_date(csv$datetime, unit = '15 minutes')

csv$Broadband.Flux <- as.numeric(csv$Broadband.Flux)
csv$UV <- as.numeric(csv$UV)
csv$Wind.Speed <- as.numeric(csv$Wind.Speed)
csv$Wind.Direction <- as.numeric(csv$Wind.Direction)
csv$Rain.Accumulation <- as.numeric(csv$Rain.Accumulation)
csv$Rain.Duration <- as.numeric(csv$Rain.Duration)
csv$Rain.Intensity <- as.numeric(csv$Rain.Intensity)
csv$RH <- as.numeric(csv$RH)
csv$Air.Pressure <- as.numeric(csv$Air.Pressure)
csv$Temperature <- as.numeric(csv$Temperature)
csv$absolute_humidity <- as.numeric(csv$absolute_humidity)

avebroadband <- aggregate(Broadband.Flux ~ datetime, csv, mean)
aveUV <- aggregate(UV ~ datetime, csv, mean)
aveWindS <- aggregate(Wind.Speed ~ datetime, csv, mean)
aveWindD <- aggregate(Wind.Direction ~ datetime, csv, mean)
aveRainA <- aggregate(Rain.Accumulation ~ datetime, csv, mean)
aveRainD <- aggregate(Rain.Duration ~ datetime, csv, mean)
aveRainI <- aggregate(Rain.Intensity ~ datetime, csv, mean)
aveRH <- aggregate(RH ~ datetime, csv, mean)
aveairpres <- aggregate(Air.Pressure ~ datetime, csv, mean)
avetemp <- aggregate(Temperature ~ datetime, csv, mean)
aveabsH <- aggregate(absolute_humidity ~ datetime, csv, mean)

sdbroadband <- aggregate(Broadband.Flux ~ datetime, csv, sd)
sdUV <- aggregate(UV ~ datetime, csv, sd)
sdWindS <- aggregate(Wind.Speed ~ datetime, csv, sd)
sdWindD <- aggregate(Wind.Direction ~ datetime, csv, sd)
sdRainA <- aggregate(Rain.Accumulation ~ datetime, csv, sd)
sdRainD <- aggregate(Rain.Duration ~ datetime, csv, sd)
sdRainI <- aggregate(Rain.Intensity ~ datetime, csv, sd)
sdRH <- aggregate(RH ~ datetime, csv, sd)
sdairpres <- aggregate(Air.Pressure ~ datetime, csv, sd)
sdtemp <- aggregate(Temperature ~ datetime, csv, sd)
sdabsH <- aggregate(absolute_humidity ~ datetime, csv, sd)

average15min <- data.frame(aveWindS$datetime, aveWindS$Wind.Speed, aveWindD$Wind.Direction, aveRainA$Rain.Accumulation, 
                 aveRainD$Rain.Duration, aveRainI$Rain.Intensity, aveRH$RH, aveairpres$Air.Pressure, 
                 avetemp$Temperature, aveabsH$absolute_humidity)
colnames(average15min) <- c('datetime', 'Wind_Speed', 'Wind_Direction', 'Rain_Accumulation', 'Rain_Duration', 'Rain_Intensity', 'RH', 'Air_Pressure', 'Air_Temperature', 'Absolute_Humidity')

write.csv(average15min, file = 'C:/Users/Zachary/Desktop/IURTP 15 min average.csv', row.names = FALSE)
