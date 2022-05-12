library('dplyr')

file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

f <- list.files('D:/Surface 3/Main Folder/Documents/Lab Data/2018_09 KBS Soil Experiment/NOy Flux')
setwd('D:/Surface 3/Main Folder/Documents/Lab Data/2018_09 KBS Soil Experiment/NOy Flux')

csv <- read.csv(f[1], stringsAsFactors = FALSE)

head(csv)

csv$time <- as.POSIXct(csv$time)
head(csv$time)

csv <- subset(csv, csv$ hour >= 4)

export <- csv %>% group_by(chamber) %>%  summarise(no2 = max(no2_flux), hour = hour[which(no2_flux == max(no2_flux))])

export <- as.data.frame(export)

for (i in 2:length(f)) {
  
  csv <- read.csv(f[i], stringsAsFactors = FALSE)
  
  head(csv)
  
  csv$time <- as.POSIXct(csv$time)
  head(csv$time)
  
  csv <- subset(csv, csv$ hour >= 4)
  
  add <- csv %>% group_by(chamber) %>%  summarise(no2 = max(no2_flux), hour = hour[which(no2_flux == max(no2_flux))])
  
  add <- as.data.frame(add)
  
  export <- rbind(export, add)
}

export$chamber <- gsub('-.*', '', export$chamber)
write.table(newexport, file = 'clipboard', sep = '\t', row.names = FALSE)

newexport <- ddply(export, .(export$chamber), summarise, ave = mean(no2), sd = sd(no2), hour = as.integer(mean(hour)))

      