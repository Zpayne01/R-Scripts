

NOx_Flux$datetime <- as.POSIXlt(NOx_Flux$datetime, format = '%m/%d/%Y %H:%M', tz = 'EST')

## Daytime ##

NOx_Flux_day <- subset(NOx_Flux, NOx_Flux$datetime$hour %in% seq(7,21,1))

## Nighttime ##

NOx_Flux_night <- subset(NOx_Flux, NOx_Flux$datetime$hour %in% (c(seq(0,6,1), seq(22,24,1))))

## Rainy split##

NOx_Flux_rain_day <- subset(NOx_Flux_day, NOx_Flux_day$datetime$mday %in% c(7,17,18))
NOx_Flux_rain_night <- subset(NOx_Flux_night, NOx_Flux_night$datetime$mday %in% c(7,17,18))

## Dry Split ##

NOx_Flux_dry_day <- subset(NOx_Flux_day, !(NOx_Flux_day$datetime$mday %in% c(7,17,18)))
NOx_Flux_dry_night <- subset(NOx_Flux_night, !(NOx_Flux_day$datetime$mday %in% c(7,17,18)))

mean(NOx_Flux_dry_day$no_flux_ng_N_m2_s, na.rm = TRUE)
mean(NOx_Flux_rain_day$no_flux_ng_N_m2_s, na.rm = TRUE)

## Write file to desktop

filename1 <- 'C:/Users/zacpayne/Desktop/NOx_flux_B_day.csv'
write.csv(NOx_Flux_day, file = filename1, row.names = FALSE)

filename2 <- 'C:/Users/zacpayne/Desktop/NOx_flux_C_night.csv'
write.csv(NOx_Flux_day, file = filename2, row.names = FALSE)
