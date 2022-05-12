file <- file.choose()
csv <- read.csv(file)
csv


csv$Wavelength <- round(csv$Wavelength, -1)

export <- aggregate(Absorbance ~ Wavelength, csv, mean)

export$Transmittance <- exp(-export$Absorbance) 

export$Transmittance[export$Transmittance <= .1] <- 0

write.table(export, 'clipboard', sep ='\t', row.names = FALSE )
