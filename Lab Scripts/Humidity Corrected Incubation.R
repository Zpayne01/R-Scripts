## Choose the NO file that you want to be corrected based on the absolute humidity
## File 1 = NOx csv made by combing the two day incubations
## File 2 = Absolute Humidity File (in g/m^3)

file1 <- file.choose()
file2 <- file.choose()

csv1 <- read.csv(file1, header = TRUE)
csv2 <- read.csv(file2, header = TRUE)

head(csv1)
head(csv2)

match <- match(csv1$hour, csv2$hour)

csv1$nob <- csv1$nob/(-0.0052*csv2$b[match]+1)
csv1$nobsd <- csv1$nobsd/(-0.0052*csv2$b[match]+1)
csv1$no2b <- csv1$no2b/(-0.0052*csv2$b[match]+1)
csv1$no2bsd <- csv1$no2bsd/(-0.0052*csv2$b[match]+1)
csv1$honob <- csv1$honob/(-0.0052*csv2$b[match]+1)
csv1$honobsd <- csv1$honobsd/(-0.0052*csv2$b[match]+1)

csv1$noc <- csv1$noc/(-0.0052*csv2$b[match]+1)
csv1$nocsd <- csv1$nocsd/(-0.0052*csv2$b[match]+1)
csv1$no2c <- csv1$no2c/(-0.0052*csv2$b[match]+1)
csv1$no2csd <- csv1$no2csd/(-0.0052*csv2$b[match]+1)
csv1$honoc <- csv1$honoc/(-0.0052*csv2$b[match]+1)
csv1$honocsd <- csv1$honocsd/(-0.0052*csv2$b[match]+1)

csv1$nod <- csv1$nod/(-0.0052*csv2$b[match]+1)
csv1$nodsd <- csv1$nodsd/(-0.0052*csv2$b[match]+1)
csv1$no2d <- csv1$no2d/(-0.0052*csv2$b[match]+1)
csv1$no2dsd <- csv1$no2dsd/(-0.0052*csv2$b[match]+1)
csv1$honod <- csv1$honod/(-0.0052*csv2$b[match]+1)
csv1$honodsd <- csv1$honodsd/(-0.0052*csv2$b[match]+1)

csv1$noe <- csv1$noe/(-0.0052*csv2$b[match]+1)
csv1$noesd <- csv1$noesd/(-0.0052*csv2$b[match]+1)
csv1$no2e <- csv1$no2e/(-0.0052*csv2$b[match]+1)
csv1$no2esd <- csv1$no2esd/(-0.0052*csv2$b[match]+1)
csv1$honoe <- csv1$honoe/(-0.0052*csv2$b[match]+1)
csv1$honoesd <- csv1$honoesd/(-0.0052*csv2$b[match]+1)

head(csv1)

filename <- "C:/Users/Zachary/Desktop/Humidity Corrected.csv"
write.csv(csv1, file = filename, row.names = FALSE)