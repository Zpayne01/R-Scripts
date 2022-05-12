## choose a massfile for analysis

file1 <- file.choose()
massfile <- read.table(file1, sep = '\t', header = TRUE)

colnames(massfile) <- c('date','chamber','treatment', 'mass')
massfile$date <- as.POSIXlt(massfile$date, format = '%m/%d/%Y')
massfile$date <- format(massfile$date, '%m/%d/%Y')

## choose the CO2 file for analysis

file2 <- file.choose()
co2file <- read.csv(file2, header = TRUE)
head(co2file)

co2file$date <- as.POSIXlt(co2file$date, format = '%m/%d/%Y')
co2file$date <- format(co2file$date, '%m/%d/%Y')

## Match the current CO2 file with the correct mass

masssub <- subset(massfile, massfile$date == co2file$date[2])

## Insert flows here 

flowb <- 2.314
flowc <- 2.336
flowd <- 2.302
flowe <- 2.334
  
## Do the flux calculation, units are ug CO2/(g soil hr.)

chamberb <- (co2file$b_co2_ppm - co2file$blank_co2_ppm)/10^6/22.4*flowb*60*44/masssub$mass[1]*10^6
chamberc <- (co2file$c_co2_ppm - co2file$blank_co2_ppm)/10^6/22.4*flowc*60*44/masssub$mass[2]*10^6
chamberd <- (co2file$d_co2_ppm - co2file$blank_co2_ppm)/10^6/22.4*flowd*60*44/masssub$mass[3]*10^6
chambere <- (co2file$e_co2_ppm - co2file$blank_co2_ppm)/10^6/22.4*flowe*60*44/masssub$mass[4]*10^6

## Prepare Flux export file

export <- data.frame(co2file$date, co2file$hour, chamberb, chamberc, chamberd, chambere)
colnames(export) <- c('date', 'hour', 'b_CO2_flux', 'c_CO2_flux', 'd_CO2_flux', 'e_CO2_flux')

filename <- paste0('C:/Users/zacpayne/Desktop/CO2_flux_',gsub('/','_', co2file$date[1]),'.csv')
write.csv(export, file = filename, row.names = FALSE)
