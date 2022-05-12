## STC File Processing ##

file <- file.choose()

data <- read.table(file, sep = ',', skip = 1, header = TRUE)
head(data)

## Change the string to a relevant time format

data$time <- as.POSIXct(data$time+(3600*4), origin = "1904-01-01 00:00:00", tz = Sys.timezone())
print(head(data))
print(tail(data))


## STC CSV Output ##

filenamelist <- strsplit(file, "\\\\")
filenamelistlength <- length(filenamelist[[1]])
filenameout <- strsplit(filenamelist[[1]][filenamelistlength], '\\.')[[1]][1]

filename <- paste0('C:/Users/zacpayne/Desktop/', filenameout, '_stc.csv')
write.csv(data, filename, row.names = FALSE)
