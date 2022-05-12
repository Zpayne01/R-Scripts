library(lubridate)

file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

head(csv)

workingCsv <- csv[,c('Time', 'X.N2O.d_ppm','X.N2O.d_ppm_sd','X.CO.d_ppm','X.CO.d_ppm_sd')]
colnames(workingCsv) <- c('time', 'n2o_ppb', 'n2o_ppb_sd', 'co_ppb', 'co_ppb_sd')

workingCsv$n2o_ppb <- workingCsv$n2o_ppb * 1000
workingCsv$n2o_ppb_sd <- workingCsv$n2o_ppb_sd * 1000
workingCsv$co_ppb <- workingCsv$co_ppb * 1000
workingCsv$co_ppb_sd <- workingCsv$co_ppb_sd * 1000

workingCsv$time <- as.POSIXlt(workingCsv$time, format = '%m/%d/%Y %H:%M:%S')

head(workingCsv)

workingCsv <- subset(workingCsv, workingCsv$time$min == 2 | workingCsv$time$min == 3 |
                     workingCsv$time$min == 4 | workingCsv$time$min == 17 | workingCsv$time$min ==  18 |
                     workingCsv$time$min == 19 | workingCsv$time$min == 32 | workingCsv$time$min == 33 |
                     workingCsv$time$min == 34 | workingCsv$time$min == 47 | workingCsv$time$min == 48 | 
                     workingCsv$time$min == 49)

workingCsv$time <- floor_date(workingCsv$time, unit = '1 hour')

workingCsv$time <- as.POSIXct(workingCsv$time, format = '%Y-%m-%d %H:%M:%S')

n2o_average <- aggregate(n2o_ppb ~ time, workingCsv, mean)
n2o_sd <- aggregate(n2o_ppb ~ time, workingCsv, sd)

co_average <- aggregate(co_ppb ~ time, workingCsv, mean)
co_sd <- aggregate(co_ppb ~ time, workingCsv, sd)

table <- data.frame(n2o_average, n2o_sd$n2o_ppb, co_average$co_ppb, co_sd$co_ppb)

write.table(table, file = 'clipboard', sep = '\t', row.names = FALSE)

