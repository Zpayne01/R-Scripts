## Choose the Mass File

file1 <- file.choose()
massfile <- read.table(file1, sep = '\t', header = TRUE)
colnames(massfile) <- c('date', 'chamber', 'soil', 'mass')
massfile$date <- as.POSIXct(massfile$date, format = '%m/%d/%Y')

## Choose the file that contains the N2O and CO data

file2 <- file.choose()
n2ofile <- read.csv(file2, header = TRUE, stringsAsFactors = FALSE)

head(n2ofile)

n2ofile$date <- as.POSIXlt(n2ofile$date, format = '%m/%d/%Y')

## Match the date with the mass file ##

masssub <- subset(massfile, massfile$date == n2ofile$date[2])

## Input flows in liters per minute determine at the time of the measurement ##

flowb <- 2.197
flowc <- 2.156
flowd <- 2.215
flowe <- 2.180
  
## Determine the flux of CO
## Work up on page 285 to determine the concentration
## Units of ng CO/ g soil hr

cob <- as.numeric(n2ofile$b_co_ppb)/10^9/22.4*flowb*28*10^9*60/masssub$mass[1]
coc <- as.numeric(n2ofile$c_co_ppb)/10^9/22.4*flowc*28*10^9*60/masssub$mass[2]
cod <- as.numeric(n2ofile$d_co_ppb)/10^9/22.4*flowd*28*10^9*60/masssub$mass[3]
coe <- as.numeric(n2ofile$e_co_ppb)/10^9/22.4*flowe*28*10^9*60/masssub$mass[4]

## Determine the flux of N2O 
## Units of ng N2O/ g soil hr.

n2ob <- as.numeric(n2ofile$b_n2o_ppb)/10^9/22.4*flowb*28*10^9*60/masssub$mass[1]
n2oc <- as.numeric(n2ofile$c_n2o_ppb)/10^9/22.4*flowc*28*10^9*60/masssub$mass[2]
n2od <- as.numeric(n2ofile$d_n2o_ppb)/10^9/22.4*flowd*28*10^9*60/masssub$mass[3]
n2oe <- as.numeric(n2ofile$e_n2o_ppb)/10^9/22.4*flowe*28*10^9*60/masssub$mass[4]

## Determine the flux of CO2
## Units of ug CO2/ g soil hr

co2b <- as.numeric(n2ofile$b_co2_ppb)/10^6/22.4*flowb*54*60*10^6/masssub$mass[1]
co2c <- as.numeric(n2ofile$c_co2_ppm)/10^6/22.4*flowc*54*60*10^6/masssub$mass[2]
co2d <- as.numeric(n2ofile$d_co2_ppm)/10^6/22.4*flowd*54*60*10^6/masssub$mass[3]
co2e <- as.numeric(n2ofile$e_co2_ppm)/10^6/22.4*flowe*54*60*10^6/masssub$mass[4]

## Export the 2nd species seperatly ##

exportn2o <- data.frame(n2ofile$date, n2ofile$hour, n2ob, n2oc, n2od, n2oe)
colnames(exportn2o) <- c('Date', 'Hour', 'N2O_Flux_B', 'N2O_Flux_C', 'N2O_Flux_D', 'N2O_Flux_E')
filedate <- gsub('-','_',masssub$date[1])
filenamen2o <- paste0('C:/Users/Zachary/Desktop/N2O_Flux_',filedate,'.csv')
write.csv(exportn2o, filenamen2o, row.names = FALSE)

exportco <- data.frame(n2ofile$date, n2ofile$hour, cob, coc, cod, coe)
colnames(exportco) <- c('Date', 'Hour', 'co_Flux_B', 'co_Flux_C', 'co_Flux_D', 'co_Flux_E')
filedate <- gsub('-','_',masssub$date[1])
filenameco <- paste0('C:/Users/Zachary/Desktop/CO_Flux_',filedate,'.csv')
write.csv(exportco, filenameco, row.names = FALSE)

exportco2 <- data.frame(n2ofile$date, n2ofile$hour, co2b, co2c, co2d, co2e)
colnames(exportco2) <- c('Date', 'Hour', 'co2_Flux_B', 'co2_Flux_C', 'co2_Flux_D', 'co2_Flux_E')
filedate <- gsub('-','_',masssub$date[1])
filenameco2 <- paste0('C:/Users/Zachary/Desktop/CO2_Flux_',filedate,'.csv')
write.csv(exportco2, filenameco2, row.names = FALSE)
