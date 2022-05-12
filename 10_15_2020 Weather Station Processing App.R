## Weather Station Processing ##

weatherfile <- file.choose()
weathercsv <- read.csv(weatherfile, header = FALSE, stringsAsFactors = FALSE)

weather1 <- weathercsv[seq(1,nrow(weathercsv), 3),]
weather2 <- weathercsv[seq(2,nrow(weathercsv), 3),]
weather3 <- weathercsv[seq(3,nrow(weathercsv), 3),]

weathercomb <- data.frame(weather1$V1, weather1$V2, weather1$V4, weather1$V6, weather1$V8,
                                                    weather2$V4, weather2$V6, weather2$V8,
                                                    weather3$V4, weather3$V6, weather3$V8, weather3$V10)
colnames(weathercomb) <- c('Date', 'Time', 'Broadband_Flux',
                           'UV', 'Wind_Speed', 'Wind_Dir',
                           'Rain_Accumulation', 'Rain_Duration',
                           'Rain_Intensity', 'RH', 'Air_Pressure',
                           'Air_Temperature')

head(weathercomb)

weathercomb$Datetime <- paste(weathercomb$Date, weathercomb$Time)
weathercomb$Datetime <- as.POSIXct(weathercomb$Datetime, format = '%m/%d/%Y %H:%M:%S')

weatherexp <- ddply(weathercomb, .(floor_date(weathercomb$Datetime, unit = '1 minute')), summarize,
                    Broadband_Flux = mean(Broadband_Flux),
                    UV = mean(UV),
                    Wind_Speed = mean(Wind_Speed),
                    Wind_dir = mean(Wind_Dir),
                    Rain_Accum = mean(Rain_Accumulation),
                    Rain_Dur = mean(Rain_Duration),
                    Rain_Intensity = mean(Rain_Intensity),
                    RH = mean(RH),
                    Air_Pressure = mean(Air_Pressure),
                    Air_Temperature = mean(Air_Temperature)
                    )

write.csv(weatherexp, file = 'C:/Users/zacpayne/Desktop/Weather Station.csv', row.names = FALSE)
