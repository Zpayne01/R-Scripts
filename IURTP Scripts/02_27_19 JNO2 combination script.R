library(lubridate)

filelist <- list.files('D:/Surface 3/Main Folder/One Drive/IURTP/JNO2', pattern = '2017')
file <- 'D:/Surface 3/Main Folder/One Drive/IURTP/JNO2/'

csv <- data.frame(datetime = as.POSIXct(character()), jNO2 = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(filelist)) {
  csvhold <- read.csv(paste0(file, filelist[i]), stringsAsFactors = FALSE)[1:4]
  csvhold$realdatetime <- as.POSIXct(csvhold$Datetime, format = "%m/%d/%Y %H:%M:%S") + hours(1)
  colnames(csvhold) <- c('datetime','date','time','jNO2','realdatetime')
  csvtransfer <- data.frame(csvhold$realdatetime, csvhold$jNO2, stringsAsFactors = FALSE)
  colnames(csvtransfer) <- c('datetime', 'jNO2')
  head(csvhold)
  csv <- rbind(csv, csvtransfer)
  head(csv)
}

csv$roundtime <- round_date(csv$datetime, unit = '1 hour')

average <- aggregate(jNO2 ~ roundtime, csv, mean)

write.table(average, file = 'clipboard', sep = '\t', row.names = FALSE)