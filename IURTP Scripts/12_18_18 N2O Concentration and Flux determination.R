library(lubridate)
library(dplyr)
library(ggplot2)

## Select n2o file, in this case it is correct_n2o_concentrations from the Sally corrected N2O

file <- file.choose()
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
csv <- csv[1:3]
colnames(csv) <- c('datetime', 'n2o_despiked_ppb', 'n2o_prelim_corr_ppb')

csv$datetime <- as.POSIXct(csv$datetime, format = "%m/%d/%Y %H:%M")
#csv$datetime <- csv$datetime + years(1) + days(1) is this necessary?
csv$datetime <- as.POSIXlt(csv$datetime)
csv$min <- as.numeric(csv$datetime$min)

csv <- csv[csv$min %in% c(2,3,17,18,32,33,47,48,12,13,27,28,42,43,57,58),]

csv$datetime <- as.POSIXct(csv$datetime)
csv$floor <- floor_date(csv$datetime, unit = '5 minutes')

## Find the background of the raw data ##

csv <- na.omit(csv)

csvbkgd <- subset(csv, as.POSIXlt(csv$datetime)$min %in% c(2,3,17,18,32,33,47,48))

pred <- smooth.spline(as.integer(csvbkgd$datetime), csvbkgd$n2o_prelim_corr_ppb)

plot(csv$datetime, csv$n2o_prelim_corr_ppb, type = 'p')
lines(pred, col = 'red')

plot(csvbkgd$datetime, csvbkgd$n2o_prelim_corr_ppb - pred$y, type = 'p')



## Predict Mean bkgd line ## 

preddf <- data.frame(time = predict(pred)$x,
                     n2o = predict(pred)$y)
preddf$time <- as.POSIXct(csvbkgd$datetime)

preddf$floor <- floor_date(csvbkgd$datetime, unit = '5 min')

predave <-  ddply(preddf, .(floor), summarize, n2o_ave = mean(n2o),
                  n2o_sd = sd(n2o))

## Take the mean of the datetime

n2o <- ddply(csv, .(floor), summarize, n2o_ave = mean(n2o_prelim_corr_ppb),
             n2o_sd = sd(n2o_prelim_corr_ppb))

n2o$datetime <- as.POSIXlt(n2o$floor)

## Calculate the difference for each of the points ## 

p <- predict(pred, as.integer(n2o$floor))
plot(n2o$floor, n2o$n2o_ave-p$y, type = 'p')

n2odiff <- n2o
n2odiff$n2o_ave <- n2o$n2o_ave - p$y

## Sepearte into each of the flux measurements ##

n2odiffb <- subset(n2odiff, n2odiff$datetime$min %in% c(25))
n2odiffc <- subset(n2odiff, n2odiff$datetime$min %in% c(40))
n2odiffd <- subset(n2odiff, n2odiff$datetime$min %in% c(55))

## export each difference column ## 

filenameb <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Difference_Chamber_B.csv'
filenamec <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Difference_Chamber_C.csv'
filenamed <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Difference_Chamber_D.csv'

write.csv(n2odiffb, file = filenameb, row.names = FALSE)
write.csv(n2odiffc, file = filenamec, row.names = FALSE)
write.csv(n2odiffd, file = filenamed, row.names = FALSE)

## Export these files and match with temperature and pressure ##

r <- 8.314E3 #mol L-1 K-1

chamberbfile <- file.choose()
chambercfile <- file.choose()
chamberdfile <- file.choose()

chamberb <- read.csv(chamberbfile)
chamberc <- read.csv(chambercfile)
chamberd <- read.csv(chamberdfile)

chamberb$Lmol <- 1/(chamberb$Air.Pressure..Hpa.*100/(chamberb$air_temp_ave..oC.+273.15 * r))
chamberc$Lmol <- 1/(chamberc$Air.Pressure..HPa.*100/(chamberc$air_temp_ave..oC.+273.15 * r))
chamberd$Lmol <- 1/(chamberd$Air.Pressure..HPa.*100/(chamberd$air_temp_ave..oC.+273.15 * r))

## N2O Flux (ng_m-2_s)

chamberb$n2o_flux <- chamberb$n2o_ave/10^9/chamberb$Lmol*1000*(0.02+0.0027)/0.106535*28*10^9/60
chamberc$n2o_flux <- chamberc$n2o_ave/10^9/chamberc$Lmol*1000*(0.02+0.0027)/0.106535*28*10^9/60
chamberd$n2o_flux <- chamberd$n2o_ave/10^9/chamberd$Lmol*1000*(0.02+0.0027)/0.106535*28*10^9/60

filenameb <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Flux_Chamber_B.csv'
filenamec <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Flux_Chamber_C.csv'
filenamed <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Flux_Chamber_D.csv'

write.csv(chamberb, file = filenameb, row.names = FALSE)
write.csv(chamberc, file = filenamec, row.names = FALSE)
write.csv(chamberd, file = filenamed, row.names = FALSE)

## Create measurement labels for each of the measurement periods (old version below)

# csv$measurement <- ifelse(csv$min %in% c(2,3), 'amba',
#                       ifelse(csv$min %in% c(17,18), 'ambb',
#                       ifelse(csv$min %in% c(32,33), 'ambc',
#                       ifelse(csv$min %in% c(47,48), 'ambd',
#                       ifelse(csv$min %in% c(12,13), 'chambera',
#                       ifelse(csv$min %in% c(27,28), 'chamberb',
#                       ifelse(csv$min %in% c(42,43), 'chamberc',
#                       ifelse(csv$min %in% c(57,58), 'chamberd',
#                              'NA'))))))))

## Remove periods not averaged for concentration measurements

csv <- csv[csv$measurement != 'NA',]

csv$datetime <- as.POSIXct(csv$datetime, format = '%Y-%m-%d %H:%M:%S')

## Floor the date for averaging purposes

csv$datetime <- floor_date(csv$datetime, unit = '5 minutes')

csv2 <- csv %>%
  group_by(datetime, measurement) %>%
  summarise(mean_n2o_ppb=mean(na.omit(n2o_prelim_corr_ppb)), sd_n2o_ppb=sd(na.omit(n2o_prelim_corr_ppb)))

## Rearrange the table and remove times that were missing averages

csv2 <- arrange(csv2, measurement)
csv2 <- na.omit(csv2)

## Save the files, change depending on the computer you are using

filename <- "C:/Users/zacpayne/Desktop/Working Files/Corrected N2O Concentrations Grouped.csv"

write.csv(csv2, file = filename, row.names = FALSE)

## Plot the n2o concentrations, change depending on the computer you are using

n2oplot <- ggplot(data = csv2, aes(x = datetime, y = mean_n2o_ppb, color = measurement)) + geom_line() + 
  labs((title = 'N2O flux for IURTP'),
  x = 'Date',
  y = '[N2O] (ppb)',
  color = "Measurement")

n2oplot <- n2oplot + theme_bw() + theme(plot.title = element_text(size = 32, hjust = 0.5), axis.title = element_text(size = 16), legend.title = element_text(size=12, face='bold'), legend.text = element_text(size=10), legend.background = element_rect(color='black',size=.5, linetype="solid"), aspect.ratio = 1)

png(paste0('C:/Users/zacpayne/Desktop/Working Files/N2O Fluxes.png'))
print(n2oplot)

dev.off()

## Flux Measurement ##

amba <- subset(csv2, csv2$measurement == 'amba')
ambb <- subset(csv2, csv2$measurement == 'ambb')
ambc <- subset(csv2, csv2$measurement == 'ambc')
ambd <- subset(csv2, csv2$measurement == 'ambd')

chambera <- subset(csv2, csv2$measurement == 'chambera')
chamberb <- subset(csv2, csv2$measurement == 'chamberb')
chamberc <- subset(csv2, csv2$measurement == 'chamberc')
chamberd <- subset(csv2, csv2$measurement == 'chamberd')

ambb$datetime <- floor_date(ambb$datetime, unit = "1 hour")
ambc$datetime <- floor_date(ambc$datetime, unit = "1 hour")
ambd$datetime <- floor_date(ambd$datetime, unit = "1 hour")

chambera$datetime <- floor_date(chambera$datetime, unit = "1 hour")
chamberb$datetime <- floor_date(chamberb$datetime, unit = "1 hour")
chamberc$datetime <- floor_date(chamberc$datetime, unit = "1 hour")
chamberd$datetime <- floor_date(chamberd$datetime, unit = "1 hour")

matchaa <- match(chambera$datetime, amba$datetime)
matchab <- match(chambera$datetime, ambb$datetime)

matchbb <- match(chamberb$datetime, ambb$datetime)
matchbc <- match(chamberb$datetime, ambc$datetime)

matchcc <- match(chamberc$datetime, ambc$datetime)
matchcd <- match(chamberc$datetime, ambd$datetime)

matchdd <- match(chamberd$datetime, ambd$datetime)
matchda <- match(chamberd$datetime + 3600, amba$datetime)

## Difference in Concentration

chambera$difference <- chambera$mean_n2o_ppb - rowMeans(data.frame(amba$mean_n2o_ppb[matchaa], ambb$mean_n2o_ppb[matchab]), na.rm = TRUE)
chamberb$difference <- chamberb$mean_n2o_ppb - rowMeans(data.frame(ambb$mean_n2o_ppb[matchbb], ambc$mean_n2o_ppb[matchbc]), na.rm = TRUE)
chamberc$difference <- chamberc$mean_n2o_ppb - rowMeans(data.frame(ambc$mean_n2o_ppb[matchcc], ambd$mean_n2o_ppb[matchcd]), na.rm = TRUE)
chamberd$difference <- chamberd$mean_n2o_ppb - rowMeans(data.frame(ambd$mean_n2o_ppb[matchdd], amba$mean_n2o_ppb[matchda]), na.rm = TRUE)

## N2O Flux (ug_m-2_hr)

chambera$n2o_flux <- chambera$difference/10^9/22.4*1000*(0.02+0.0027)/0.106535*28*10^6*60
chamberb$n2o_flux <- chamberb$difference/10^9/22.4*1000*(0.02+0.0027)/0.106535*28*10^6*60
chamberc$n2o_flux <- chamberc$difference/10^9/22.4*1000*(0.02+0.0027)/0.106535*28*10^6*60
chamberd$n2o_flux <- chamberd$difference/10^9/22.4*1000*(0.02+0.0027)/0.106535*28*10^6*60

filenamea <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Flux_Chamber_A.csv'
filenameb <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Flux_Chamber_B.csv'
filenamec <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Flux_Chamber_C.csv'
filenamed <- 'C:/Users/zacpayne/Desktop/Working Files/N2O_Flux_Chamber_D.csv'

write.csv(chambera, file = filenamea, row.names = FALSE)
write.csv(chamberb, file = filenameb, row.names = FALSE)
write.csv(chamberc, file = filenamec, row.names = FALSE)
write.csv(chamberd, file = filenamed, row.names = FALSE)