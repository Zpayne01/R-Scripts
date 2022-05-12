## choose a massfile for analysis

file1 <- file.choose()
massfile <- read.table(file1, sep = '\t', header = TRUE)

colnames(massfile) <- c('date','chamber','treatment', 'mass')
massfile$date <- as.POSIXlt(massfile$date, format = '%m/%d/%Y')
massfile$date <- format(massfile$date, '%m/%d/%Y')

## choose the n2o file for analysis

file2 <- file.choose()
n2ofile <- read.csv(file2, header = TRUE)
head(n2ofile)

n2ofile$date <- as.POSIXlt(n2ofile$date, format = '%m/%d/%Y')
n2ofile$date <- format(n2ofile$date, '%m/%d/%Y')

## Match the current n2o file with the correct mass

masssub <- subset(massfile, massfile$date == n2ofile$date[2])

## Insert flows here 

flowb <- 2.314
flowc <- 2.336
flowd <- 2.302
flowe <- 2.334

## Do the flux calculation, units are ug N-N2O/(g soil hr.)

chamberb <- (n2ofile$b_n2o_ppb - n2ofile$blankb_n2o_ppb)/10^6/22.4*flowb*60*28/masssub$mass[1]*10^6
chamberc <- (n2ofile$c_n2o_ppb - n2ofile$blankc_n2o_ppb)/10^6/22.4*flowc*60*28/masssub$mass[2]*10^6
chamberd <- (n2ofile$d_n2o_ppb - n2ofile$blankd_n2o_ppb)/10^6/22.4*flowd*60*28/masssub$mass[3]*10^6
chambere <- (n2ofile$e_n2o_ppb - n2ofile$blanke_n2o_ppb)/10^6/22.4*flowe*60*28/masssub$mass[4]*10^6

## Prepare Flux export file

export <- data.frame(n2ofile$date, n2ofile$hour, chamberb, chamberc, chamberd, chambere)
colnames(export) <- c('date', 'hour', 'b_n2o_flux', 'c_n2o_flux', 'd_n2o_flux', 'e_n2o_flux')

filename <- paste0('C:/Users/zacpayne/Desktop/n2o_flux_',gsub('/','_', n2ofile$date[1]),'.csv')
write.csv(export, file = filename, row.names = FALSE)


## Do the flux calculation, units are ug CO/(g soil hr.)

chamberb <- (n2ofile$b_co_ppb - n2ofile$blankb_co_ppb)/10^6/22.4*flowb*60*28/masssub$mass[1]*10^6
chamberc <- (n2ofile$c_co_ppb - n2ofile$blankc_co_ppb)/10^6/22.4*flowc*60*28/masssub$mass[2]*10^6
chamberd <- (n2ofile$d_co_ppb - n2ofile$blankd_co_ppb)/10^6/22.4*flowd*60*28/masssub$mass[3]*10^6
chambere <- (n2ofile$e_co_ppb - n2ofile$blanke_co_ppb)/10^6/22.4*flowe*60*28/masssub$mass[4]*10^6

## Prepare Flux export file

export <- data.frame(n2ofile$date, n2ofile$hour, chamberb, chamberc, chamberd, chambere)
colnames(export) <- c('date', 'hour', 'b_co_flux', 'c_co_flux', 'd_co_flux', 'e_co_flux')

filename <- paste0('C:/Users/zacpayne/Desktop/co_flux_',gsub('/','_', n2ofile$date[1]),'.csv')
write.csv(export, file = filename, row.names = FALSE)
