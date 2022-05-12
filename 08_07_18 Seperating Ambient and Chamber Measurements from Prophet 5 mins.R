file <- "C:/Users/Zachary/Desktop/Outdoor Test NOy.csv"
csv <- read.csv(file, stringsAsFactors = FALSE)

head(csv)

chambera <- csv[1:7]
chamberb <- csv[8:14]
chamberc <- csv[15:21]
chamberd <- csv[22:28]

colnames(chambera) <- c("time", "no", "nosd", "no2", "no2sd", "hono", "honosd")
colnames(chamberb) <- c("time", "no", "nosd", "no2", "no2sd", "hono", "honosd")
colnames(chamberc) <- c("time", "no", "nosd", "no2", "no2sd", "hono", "honosd")
colnames(chamberd) <- c("time", "no", "nosd", "no2", "no2sd", "hono", "honosd")

chambera$time <- as.POSIXlt(chambera$time, format = '%Y-%m-%d %H:%M:%S')
chamberb$time <- as.POSIXlt(chamberb$time, format = '%Y-%m-%d %H:%M:%S')
chamberc$time <- as.POSIXlt(chamberc$time, format = '%Y-%m-%d %H:%M:%S')
chamberd$time <- as.POSIXlt(chamberd$time, format = '%Y-%m-%d %H:%M:%S')

chambera$min <- chambera$time$min
chamberb$min <- chamberb$time$min
chamberc$min <- chamberc$time$min
chamberd$min <- chamberd$time$min

chambera$hour <- chambera$time$hour
chamberb$hour <- chamberb$time$hour
chamberc$hour <- chamberc$time$hour
chamberd$hour <- chamberd$time$hour

chambera$wday <- chambera$time$wday
chamberb$wday <- chamberb$time$wday
chamberc$wday <- chamberc$time$wday
chamberd$wday <- chamberd$time$wday

chamberaamb <- subset(chambera, chambera$min == 0)
chamberbamb <- subset(chamberb, chamberb$min == 15)
chambercamb <- subset(chamberc, chamberc$min == 30)
chamberdamb <- subset(chamberd, chamberd$min == 45)

chamberames <- subset(chambera, (chambera$min == 5 | chambera$min == 10) & chambera$no > 0)
chamberbmes <- subset(chamberb, (chamberb$min == 20 | chamberb$min == 25) & chamberb$no > 0)
chambercmes <- subset(chamberc, (chamberc$min == 35 | chamberc$min == 40) & chamberc$no > 0)
chamberdmes <- subset(chamberd, (chamberd$min == 50 | chamberd$min == 55) & chamberd$no > 0)

chamberamesno <- aggregate(no ~ hour, chamberames, mean)
chamberbmesno <- aggregate(no ~ hour, chamberbmes, mean)
chambercmesno <- aggregate(no ~ hour, chambercmes, mean)
chamberdmesno <- aggregate(no ~ hour, chamberdmes, mean)

chamberamatch <- match(chamberaamb$hour, chamberamesno$hour)
chamberbmatch <- match(chamberbamb$hour, chamberbmesno$hour)
chambercmatch <- match(chambercamb$hour, chambercmesno$hour)
chamberdmatch <- match(chamberdamb$hour, chamberdmesno$hour)

chamberanodif <- chamberamesno$no[chamberamatch] - chamberaamb$no
chamberbnodif <- chamberbmesno$no[chamberbmatch] - chamberbamb$no
chambercnodif <- chambercmesno$no[chambercmatch] - chambercamb$no
chamberdnodif <- chamberdmesno$no[chamberdmatch] - chamberdamb$no

combinedamb <- rbind(chamberaamb[1:7], chamberbamb[1:7], chambercamb[1:7], chamberdamb[1:7])
combinedamb <- combinedamb[order(combinedamb$time, decreasing = FALSE),]

## Determine the flux in units of ng N m2-2 s-1

fluxa <- ((chamberanodif)/10^9*(1/(22.4))*1000*(0.02 + 0.0027))/(0.106535)*(14.0*10^9/60)
fluxb <- ((chamberbnodif)/10^9*(1/(22.4))*1000*(0.02 + 0.0027))/(0.106535)*(14.0*10^9/60)
fluxc <- ((chambercnodif)/10^9*(1/(22.4))*1000*(0.02 + 0.0027))/(0.106535)*(14.0*10^9/60)
fluxd <- ((chamberdnodif)/10^9*(1/(22.4))*1000*(0.02 + 0.0027))/(0.106535)*(14.0*10^9/60)

timea <- as.POSIXct(chamberaamb$time, format = "%Y-%m-%d %H:%M:%S")
timeb <- as.POSIXct(chamberbamb$time, format = "%Y-%m-%d %H:%M:%S")
timec <- as.POSIXct(chambercamb$time, format = "%Y-%m-%d %H:%M:%S")
timed <- as.POSIXct(chamberdamb$time, format = "%Y-%m-%d %H:%M:%S")

n <- max(length(fluxa), length(fluxb), length(fluxc), length(fluxd))

length(timea) <- n
length(timeb) <- n
length(timec) <- n
length(timed) <- n

length(fluxa) <- n
length(fluxb) <- n
length(fluxc) <- n
length(fluxd) <- n

timea <- as.POSIXct(timea, origin = "1970/01/01")
timeb <- as.POSIXct(timeb, origin = "1970/01/01")
timec <- as.POSIXct(timec, origin = "1970/01/01")
timed <- as.POSIXct(timed, origin = "1970/01/01")

export <- data.frame(timea, fluxa, timeb, fluxb, timec, fluxc, timed, fluxd)
colnames(export) <- c("Chamber_A_Time", "Flux_A", "Chamber_B_Time", "Flux_B", "Chamber_C_Time", "Flux_C", "Chamber_D_Time", "Flux_D")

write.table(export, file = "clipboard", sep = "\t", row.names = FALSE)
#write.table(combinedamb, file = "clipboard", sep = "\t", row.names = FALSE)
