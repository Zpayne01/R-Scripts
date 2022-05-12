## CO2 Analysis of Ryan's Data

file <- file.choose()
table <- read.delim(file, header = TRUE)

colnames(table) <- c('date', 'hour', 'chamber', 'CO2')

## Seperate the data set by chamber

chambera <- subset(table, table$chamber == 'Blank')
chamberb <- subset(table, table$chamber == 'B')
chamberc <- subset(table, table$chamber == 'C')
chamberd <- subset(table, table$chamber == 'D')
chambere <- subset(table, table$chamber == 'E')

# We have to assume that the Blank chamber is at 0 ppm, we take the average
# of the blank chamber to find what the true concentration is

averageblank <- aggregate(CO2 ~ date, chambera, mean)

matchb <- match(chamberb$date, averageblank$date)
matchc <- match(chamberc$date, averageblank$date)
matchd <- match(chamberd$date, averageblank$date)
matche <- match(chambere$date, averageblank$date)

chamberb$CO2 <- chamberb$CO2 - averageblank$CO2[matchb]
chamberc$CO2 <- chamberc$CO2 - averageblank$CO2[matchc]
chamberd$CO2 <- chamberd$CO2 - averageblank$CO2[matchd]
chambere$CO2 <- chambere$CO2 - averageblank$CO2[matche]

## We need to then take into account the soil mass

massfile <- file.choose()
massfile <- read.delim(massfile, header = TRUE)

colnames(massfile) <- c('date', 'chamber', 'mass')

## Subset by chamber

massb <- subset(massfile, massfile$chamber == 'B')
massc <- subset(massfile, massfile$chamber == 'C')
massd <- subset(massfile, massfile$chamber == 'D')
masse <- subset(massfile, massfile$chamber == 'E')

matchb <- match(chamberb$date, massb$date)
matchc <- match(chamberc$date, massc$date)
matchd <- match(chamberd$date, massd$date)
matche <- match(chambere$date, masse$date)

## Calculate Difference in ug CO2 g^-1 soil hr^-1

flowb <- 2.323
flowc <- 2.367
flowd <- 2.362
flowe <- 2.338

chamberb$CO2 <- chamberb$CO2/10^6/22.4*flowb*60*44/massb$mass[matchb]*10^6
chamberc$CO2 <- chamberc$CO2/10^6/22.4*flowb*60*44/massc$mass[matchc]*10^6
chamberd$CO2 <- chamberd$CO2/10^6/22.4*flowb*60*44/massd$mass[matchd]*10^6
chambere$CO2 <- chambere$CO2/10^6/22.4*flowb*60*44/masse$mass[matche]*10^6

## Seperate by Chambers ##

for(i in 1:nrow(averageblank)) {
  date <- as.character(averageblank$date[i])
  chamberhour <- subset(chamberb$hour, chamberb$date == date)
  chamberbsub <- subset(chamberb$CO2, chamberb$date == date)
  chambercsub <- subset(chamberc$CO2, chamberc$date == date)
  chamberdsub <- subset(chamberd$CO2, chamberd$date == date)
  chamberesub <- subset(chambere$CO2, chambere$date == date)
  export <- data.frame(chamberhour, chamberbsub, chambercsub, chamberdsub, chamberesub)
  filename = paste0('C:/Users/zacpayne/Desktop/CO2 ', gsub('/','_', date),'.csv')
  write.csv(export, file = filename, row.names = FALSE)
}

write.csv(chamberb, file = 'C:/Users/zacpayne/Desktop/Chamber B CO2 each day.csv', row.names = FALSE)
write.csv(chamberc, file = 'C:/Users/zacpayne/Desktop/Chamber C CO2 each day.csv', row.names = FALSE)
write.csv(chamberd, file = 'C:/Users/zacpayne/Desktop/Chamber D CO2 each day.csv', row.names = FALSE)
write.csv(chambere, file = 'C:/Users/zacpayne/Desktop/Chamber E CO2 each day.csv', row.names = FALSE)


## Avereage and sd by hour

meanco2b <- aggregate(CO2 ~ hour, chamberb, mean)
sdco2b <- aggregate(CO2 ~ hour, chamberb, sd)

meanco2c <- aggregate(CO2 ~ hour, chamberc, mean)
sdco2c <- aggregate(CO2 ~ hour, chamberc, sd)

meanco2d <- aggregate(CO2 ~ hour, chamberd, mean)
sdco2d <- aggregate(CO2 ~ hour, chamberd, sd)

meanco2e <- aggregate(CO2 ~ hour, chambere, mean)
sdco2e <- aggregate(CO2 ~ hour, chambere, sd)

exportb <- data.frame(meanco2b, sdco2b$CO2)
exportc <- data.frame(meanco2c, sdco2c$CO2)
exportd <- data.frame(meanco2d, sdco2d$CO2)
exporte <- data.frame(meanco2e, sdco2e$CO2)

colnames(exportb) <- c('hour', 'CO2_ug/g hr-1', 'sd')
colnames(exportc) <- c('hour', 'CO2_ug/g hr-1', 'sd')
colnames(exportd) <- c('hour', 'CO2_ug/g hr-1', 'sd')
colnames(exporte) <- c('hour', 'CO2_ug/g hr-1', 'sd')

write.csv(exportb, file = 'C:/Users/zacpayne/Desktop/Chamber B CO2.csv', row.names = FALSE)
write.csv(exportc, file = 'C:/Users/zacpayne/Desktop/Chamber C CO2.csv', row.names = FALSE)
write.csv(exportd, file = 'C:/Users/zacpayne/Desktop/Chamber D CO2.csv', row.names = FALSE)
write.csv(exporte, file = 'C:/Users/zacpayne/Desktop/Chamber E CO2.csv', row.names = FALSE)



