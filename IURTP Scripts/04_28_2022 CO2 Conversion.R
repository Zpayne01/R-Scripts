
library(usefulconversion)
library(lubridate)

filea <- file.choose()
csva <- read.csv(filea)

head(csva)
class(csva$datetime_a)

csva$datetime_a <- as.POSIXct(csva$datetime_a)

file2 <- file.choose()
csv2 <- read.csv(file2)
head(csv2)

csv2$datetime <- as.POSIXct(csv2$datetime_d)

file3 <- file.choose()
csv3 <- read.csv(file3)
head(csv3)

csv3$datetime <- as.POSIXct(csv3$datetime, format = '%m/%d/%Y %H:%M')

m1 <- match(csva$datetime_a,csv2$datetime)
print(m1)
m2 <- match(csva$datetime_a, floor_date(csv3$datetime, unit = '1 hour'))
print(m2)

outputdf <- data.frame(
  datetime = csva$datetime_a,
  blankCO2 = csva$ave_CO2_a,
  blankCO2sd = csva$sd_CO2_a,
  chamberCO2 = csv2$ave_CO2_d[m1],
  chamberCO2sd = csv2$sd_CO2_d[m1],
  temperature = csv3$air_temp_ave[m2]
)
head(outputdf)

outputdf$molperL <- 1/(0.0821 * (outputdf$temperature+273.15))

#in g C m-2 h-1
outputdf$CO2flux <- (outputdf$chamberCO2 - outputdf$blankCO2)/1e6 * outputdf$molperL * 1000 * 0.0227 * 60 / 0.1065 * 12

#in mg C m-2 h-1
outputdf$CO2flux <- outputdf$CO2flux * 1000

#in ug C m-2 s-1 
outputdf$CO2flux <- outputdf$CO2flux * 1000/3600

write.csv(outputdf, file = 'C:/Users/zacpayne/Desktop/CO2_flux_D.csv')
