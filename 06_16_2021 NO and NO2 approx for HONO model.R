file <- file.choose()
data <- read.csv(file)

head(data, 2)
tail(data, 2)

data$datetime <- as.POSIXct(data$time, format = '%m/%d/%Y %H:%M')
timeseq <- seq.POSIXt(as.POSIXct('08/03/2017 0:00', format = '%m/%d/%Y %H:%M'), as.POSIXct('08/16/2017 23:00:00', format = '%m/%d/%Y %H:%M'), by = '1 hour')

matchtime <- match(timeseq, data$datetime)

#data2 <- data.frame(datetime = timeseq, 
#                     NO_ave = na.approx(data$NO_ave_ppb[matchtime]),
#                     NO2_ave = na.approx(data$NO2_ave_ppb[matchtime]),
#                     O3_ave = na.approx(data$O3_ave_ppb[matchtime])
#                     )

# filename <- 'C:/Users/zacpayne/Desktop/NOx and O3 with missing times.csv'

data2 <- data.frame(time = timeseq,
                    N2O_ave = na.approx(data$n2o_ppb[matchtime]),
                    CO_ave = na.approx(data$co_ppb[matchtime]))

write.csv(data2, file = filename, row.names = F)

