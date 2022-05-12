## N2O extraction (.str file)

file <- file.choose()

data <- read.table(file, sep = ' ', skip = 1, header = FALSE)
data <- data[,1:8]

colnames(data) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')

## Change the string to a relevant time format

data$datetime <- as.POSIXct(data$datetime+(3600*4), origin = "1904-01-01 00:00:00", tz = Sys.timezone())
print(head(data))
print(tail(data))

## Save File ##

filenamelist <- strsplit(file, "\\\\")
filenamelistlength <- length(filenamelist[[1]])
filenameout <- strsplit(filenamelist[[1]][filenamelistlength], '\\.')[[1]][1]

filename <- paste0('C:/Users/zacpayne/Desktop/', filenameout, '_str.csv')
write.csv(data, filename, row.names = FALSE)
