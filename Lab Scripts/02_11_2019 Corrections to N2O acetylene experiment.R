file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

csv <- csv[,1:7]

csv$N2O_corr <- (0.8766 * csv$X.N2O._ppm + 0.0122) * (1.3245 * 10^-6 * csv$X.H2O._ppm + 1.1681 * 10^-5) + (0.8766 * csv$X.N2O._ppm + 0.0122)

csv$Time <- as.POSIXct(csv$Time, format = '%m/%d/%Y %H:%M:%S')

#Change the mass of soil used and the zero concentration on the right

#ECM1: 27.3g - Background = 0.0390 (ug N-N2O/ (g soil h)^-1)
#ECM2: 26.9g
#ECM4: 27.1g
#ECM5: 26.2g
#Sterile: 26.2

csv$N2OFlux <- (csv$N2O_corr-0.012)/10^6/22.4/26.2*1.128*28*10^6*60 
  
plot(csv$Time, csv$N2O_corr, type = 'l')

filename = 'C:/Users/zacpayne/Desktop/Working Files/N2O_Incubation_.csv'
write.csv(csv, file = filename, row.names = FALSE)
