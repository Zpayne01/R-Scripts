## Used to find flux in mg N m(-2) hr(-1)

file1 <- file.choose()
file2 <- file.choose()

csv1 <- read.csv(file1)
csv2 <- read.csv(file2)

colnames(csv1) <- c('time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')
colnames(csv2) <- c('time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')

csv1$time <- as.POSIXct(csv1$time, format = '%m/%d/%Y %H:%M')
csv2$time <- as.POSIXct(csv2$time, format = '%Y-%m-%d %H:%M:%S')

csv_match <- match(csv2$time, csv1$time)

## micrograms - N m^-2 hr^-1

## Equations Layout
## ppb[chamber] - ppb[A] x mol NO/mol air x mol air/22.4 L x 1000 L/ m^3 x m^3/min (total flow) / m^2 (area of chamber) x molecular weight g x ug x min/hr

flux_no <- ((csv2$no_ave - csv1$no_ave[csv_match])/10^9 * ((1/22.4) * 1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^6)*(60)) 
no_sd <- (sqrt((csv1$no_sd[csv_match])^2 + (csv2$no_sd)^2)/10^9 * ((1/22.4)*1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^6)*(60))

flux_no2 <- ((csv2$no2_ave - csv1$no2_ave[csv_match])/10^9 * ((1/22.4)*1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^6)*(60))
no2_sd <- (sqrt((csv1$no2_sd[csv_match])^2 + (csv2$no2_sd)^2)/10^9 * ((1/22.4)*1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^6)*(60))

flux_hono <- ((csv2$hono_ave - csv1$hono_ave[csv_match])/10^9 * ((1/22.4)*1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^6)*(60))
hono_sd <- (sqrt((csv1$hono_sd[csv_match])^2 + (csv2$hono_sd)^2)/10^9 * ((1/22.4)*1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^6)*(60))

export <- data.frame(csv2$time, flux_no, no_sd, flux_no2, no2_sd, flux_hono, hono_sd)
colnames(export) <- c('Time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')

write.csv(export, file = 'C:/Users/Zachary/Desktop/Flux.csv', row.names = FALSE)

head(csv1)
head(csv2)

