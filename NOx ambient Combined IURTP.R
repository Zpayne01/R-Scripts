## File for transforming the 15 minute averages from the ambient measurements into hourly averages

file <- file.choose()

csv <- read.csv(file, stringsAsFactors = FALSE)

colnames(csv) <- c('time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')

csv$time <- as.POSIXct(csv$time, format = '%m/%d/%Y %H:%M')
csv$time <- floor_date(csv$time, unit = '1 hour')

uniquetimes <- unique(csv$time)

csv$hono_ave[csv$hono_ave == 0] <- 10^-11

no_average <- aggregate(no_ave ~ time, csv, mean)
no2_average <- aggregate(no2_ave ~ time, csv, mean)
hono_average <- aggregate(hono_ave ~ time, csv, mean)

no_ES <- aggregate(no_sd^2 * 44 ~ time, csv, sum)
no2_ES <- aggregate(no2_sd^2 * 44 ~ time, csv, sum)
hono_ES <- aggregate(hono_sd^2 * 44 ~ time, csv, sum)

no_sum <- aggregate(no_ave ~ time, csv, sum)
no2_sum <- aggregate(no2_ave ~ time, csv, sum)
hono_sum <- aggregate(hono_ave ~ time, csv, sum)

no_GS <- aggregate((csv$no_ave - no_average[match(csv$time, no_average$time),]$no_ave)^2 * 45 ~ time, csv, sum)
no2_GS <- aggregate((csv$no2_ave - no2_average[match(csv$time, no2_average$time),]$no2_ave)^2 * 45 ~ time, csv, sum)                
hono_GS <- aggregate((csv$hono_ave - hono_average[match(csv$time, hono_average$time),]$hono_ave)^2 * 45 ~ time, csv, sum)

colnames(no_ES) <- c('time', 'ES')
colnames(no2_ES) <- c('time', 'ES')
colnames(hono_ES) <- c('time', 'ES')

colnames(no_GS) <- c('time', 'GS')
colnames(no2_GS) <- c('time', 'GS')
colnames(hono_GS) <- c('time', 'GS')

no_GV <- sqrt((no_ES$ES + no_GS$GS)/((45 * no_sum$no_ave/no_average$no_ave) - 1))
no2_GV <- sqrt((no2_ES$ES + no2_GS$GS)/((45 * no2_sum$no2_ave/no2_average$no2_ave) - 1))
hono_GV <- sqrt((hono_ES$ES + hono_GS$GS)/((45 * hono_sum$hono_ave/hono_average$hono_ave) - 1)

hono_average <- data.frame(hono_average, hono_GV)                
hono_match <- match(no_average$time, hono_average$time)                
                
export <- data.frame(no_average, no_GV, no2_average$no2_ave, no2_GV, hono_average$hono_ave[hono_match], hono_average$hono_GV[hono_match])
colnames(export) <- c('time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')

write.csv(export, file = 'C:/Users/Zachary/Desktop/Ambient.csv', row.names = FALSE)
