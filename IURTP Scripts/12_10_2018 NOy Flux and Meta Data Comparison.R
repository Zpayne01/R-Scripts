## Choose a file containing binned metadata for comparison

file <- file.choose()
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
csv$datetime <- as.POSIXct(csv$datetime, format = "%Y-%m-%d %H:%M:%S")

## Choose a file containing the NO Flux values (here C:\Users\zacpayne\Desktop\12_06_18 Modeling Work\NO Flux and Soil Parameters (No Rain))

file2 <- file.choose()
csv2 <- read.csv(file2, header = TRUE, stringsAsFactors = FALSE)
csv2$Time <- as.POSIXct(csv2$Time, format = "%m/%d/%Y %H:%M")

##csv2$Time <- as.POSIXct(csv2$Time, format = "%Y-%m-%d %H:%M:%S")

## Add time to csv2 datetime to match with the corresponding bin, THIS IS IMPORTANT!!!

csv2$Time <- csv2$Time 
matchedTimes <- match(csv2$Time, csv$datetime)
names <- colnames(csv)

for(i in 2:ncol(csv)) {
  csv2[, names[i]] <- csv[matchedTimes, names[i]]
}
  
filename <- 'C:/Users/zacpayne/Desktop/Working Files/NOy Flux with Weather Station Data (No Rain).csv'

write.csv(csv2, file = filename, row.names = FALSE)