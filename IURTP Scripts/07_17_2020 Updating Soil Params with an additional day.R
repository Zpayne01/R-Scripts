
file <- file.choose()
csv <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)

output <- data.frame(
  datetime = paste(csv$V1, csv$V2),
  airtemp_C = csv$V4,
  humidity = csv$V6,
  soiltemp_C = csv$V8,
  vwc = csv$V10
)

output$datetime <- as.POSIXct(output$datetime, format = '%m/%d/%Y %H:%M:%S')

library(lubridate)

output$datetime <- floor_date(output$datetime, unit = '1 minute')

filename <- 'C:/Users/zacpayne/Desktop/chamber.csv'

write.csv(output, filename, row.names = FALSE)

output.2 <- output
  
output.2$datetime <- floor_date(output.2$datetime, unit = '15 minute')

output.2.ave <- aggregate(.~ datetime, output.2, mean, na.action = na.omit)
output.2.sd <- aggregate(.~ datetime, output.2, sd, na.action = na.omit)

output.2.comb <- data.frame(output.2.ave, output.2.sd[-1])

filename.2 <- 'C:/Users/zacpayne/Desktop/15 minutes.csv'

write.csv(output.2.comb, filename.2, row.names = FALSE)

output.3 <- output.2.comb

output.3$datetime <- as.POSIXlt(output.3$datetime)

output.3 <- subset(output.3, output.3$datetime$min == 0)

filename.3 <- 'C:/Users/zacpayne/Desktop/closed.csv'

write.csv(output.3, filename.3, row.names = FALSE)
