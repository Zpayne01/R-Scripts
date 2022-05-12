file1 <- file.choose()
csv1 <- read.csv(file1, stringsAsFactors = FALSE)

file2 <- file.choose()
csv2 <- read.csv(file2, stringsAsFactors = FALSE)

head(csv1)
head(csv2)

colnames(csv1)
colnames(csv2)

## Rename the column containing date and time to datetime

datetime1 <- "datetime"
datetime2 <- "rounddate"

colnames(csv1)[which(colnames(csv1) == datetime1)] <- 'datetime'
colnames(csv2)[which(colnames(csv2) == datetime2)] <- 'datetime'

csv1$datetime[1]
csv2$datetime[2]

## Change the format according to the time in the datetime column, check and make sure it worked
## by checking the first point in each datetime column

csv1$datetime <- as.POSIXlt(csv1$datetime, format = '%Y-%m-%d %H:%M')
csv2$datetime <- as.POSIXlt(csv2$datetime, format = '%m/%d/%Y %H:%M')

csv1$datetime[1]
csv2$datetime[2]

# Match csv1 to the time of csv2

matched <- match(csv1$datetime, csv2$datetime)
colnames(csv2)

# Choose the columns you want to export 

export <- data.frame(csv1, csv2$HO2[matched]) # data.frame(csv1, csv2[matched,2:ncol(csv2)])

write.csv(export, file = 'C:/Users/zacpayne/Desktop/Working Files/Matched.csv', row.names = FALSE)
