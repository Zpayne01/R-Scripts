library(zoo)

file <- file.choose()
data <- read.csv(file)

RHtoAH <- function(RH, temp) { # Adapted from the Vaisala Humidity Conversion Formulas
  # Step one: Solve for the water vapor saturation pressure over water
  A = 6.115441
  m = 7.591386
  Tn = 240.7263
  Pws = A * 10^(m * temp/(temp + Tn)) #Hpa
  # Step two: Calculate the water vapor pressure
  Pw = Pws * RH/100 * 100 #Pa
  # Step three: Calculate the AH
  AH = 2.16679 * Pw/(temp + 273.15) #g/m3
  return(AH)
}

filename <- strsplit(file, '\\\\')[[1]][length(strsplit(file, '\\\\')[[1]])]

data$floor <- as.POSIXct(data$floor, format = '%m/%d/%Y %H:%M')

time <- seq.POSIXt(
  as.POSIXct('8/2/2017 13:00', format = '%m/%d/%Y %H:%M'),
  data$floor[length(data$floor)],
  by = '1 hour')

m <- match(as.integer(time), as.integer(data$floor))

data <- data.frame(floor = time, data[m,2:ncol(data)])

data$n2o_ave[data$n2o_ave < 0] <- NA
data$n2o_flux[data$n2o_flux < 0] <- NA
data$AH <- RHtoAH(data$relative_humidity_ave, data$soil_temp_ave)
data$Minute <- data$Minute[1]

data2 <- data.frame(sapply(data, na.approx))
data2$floor <- as.POSIXct(data$floor)
sapply(data2, length)

output <- paste0('C:/Users/zacpayne/Desktop/Missing_Times_', filename)
write.csv(data2, output, row.names = F)

