library(lubridate)

file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

head(csv)

stderror <- function (x) {
  xnum <- as.numeric(x)
  y <- xnum[!is.na(xnum)]
  if (length(y > 0)) {
  sd(y)
  length(y)
  sd(y)/sqrt(length(y))
  } else {
    return('NaN')
  }
}

names <- colnames(csv)
names[1] <- 'middle.sampling.time'
colnames(csv) <- names

csv$middle.sampling.time <- as.POSIXlt(csv$middle.sampling.time, format = "%m/%d/%Y %H:%M")

begin <- match('2015-07-09', as.character(as.Date(csv$middle.sampling.time)))
end <- match('2015-07-24', as.character(as.Date(csv$middle.sampling.time)))

csv <- csv[begin:end,]

csvByHour <- data.frame(csv$middle.sampling.time$hour, csv[2:ncol(csv)])
colnames(csvByHour)[1] <- c('hour')

csvDiurnal <- aggregate(. ~ hour, csvByHour, mean, na.rm = TRUE, na.action = NULL)
csvDiurnalSE <- aggregate(. ~ hour, csvByHour, stderror, na.action = NULL)

csv$middle.sampling.time <- floor_date(csv$middle.sampling.time, unit = '2 hours')

csvBy2Hour <- data.frame(csv$middle.sampling.time$hour, csv[2:ncol(csv)])
colnames(csvBy2Hour)[1] <- c('hour')

csvDiurnal2 <- aggregate(. ~ hour, csvBy2Hour, mean, na.rm = TRUE, na.action = NULL)

filename = 'C:/Users/zacpayne/Desktop/07_09 through 07_24 C6 to C14 VOCs.csv'
filename2 = 'C:/Users/zacpayne/Desktop/07_09 through 07_24 C6 to C14 VOCs Diurnal.csv'
filename3 = 'C:/Users/zacpayne/Desktop/07_09 through 07_24 C6 to C14 VOCs 2-hour diurnal.csv'
filename4 = 'C:/Users/zacpayne/Desktop/07_09 through 07_24 C6 to C14 VOCs Standard Errors.csv'

write.csv(csv, file = filename, row.names = FALSE)
write.csv(csvDiurnal, file = filename2, row.names = FALSE)
write.csv(csvDiurnal2, file = filename3, row.names = FALSE)
write.csv(csvDiurnalSE, file = filename4, row.names = FALSE)
