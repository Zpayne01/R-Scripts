file <- file.choose() #Choose File
tab <- read.csv(file, header = T)

head(tab)

tab$Time <- as.POSIXlt(tab$Time, format = '%Y-%m-%d %H:%M:%S') #Convert to time object

tab[,2:7] <- (tab[,2:7] + 5)*(1718)^-1 #NO calibration from 02/24
tab[,4] <- (tab[,4])/.9 #Using 90% CE for NO2
AH <- 8 #RH was % 46 temp was 21 C

tab[,2:7] <- tab[,2:7]/(-0.0055 * AH + 1)


colnames(tab) <- c('Time', 'NO_ppb', 'NO_sd', 'NO2_ppb', 'NO2_sd', 'HONO_ppb', 'HONO_sd')

dev.new(5, 5)
plot(NO2_ppb ~ as.POSIXct(Time), tab, type = 'l')
points(HONO_ppb ~ as.POSIXct(Time), tab, col = 'red')
points(NO_ppb ~ as.POSIXct(Time), tab, col = 'blue')

tab2 <- tab
tab2$Time <- as.POSIXct(tab2$Time) + 180

write.csv(tab2, file = 'C:/Users/zacpayne/Desktop/02_25_22 NOx Intercomparison.csv')
