
file <- file.choose()
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

colnames(csv) <- c('wavelength395', 'intensity395', 'wavelength385', 'intensity385')

csv$wavelength395 <- round(csv$wavelength395, 0)
csv$wavelength385 <- round(csv$wavelength385, 0)

wavelength395 <- aggregate(intensity395 ~ wavelength395, csv, mean)
wavelength385 <- aggregate(intensity385 ~ wavelength385, csv, mean)

export <- data.frame(wavelength395, wavelength385)
write.table(export, file = 'clipboard', sep = "\t", row.names = FALSE)
