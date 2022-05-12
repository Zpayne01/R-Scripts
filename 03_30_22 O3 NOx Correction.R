date <- '3/28/2022'
slope <- 1529
intercept <- 7.3

file <- file.choose()

data <- read.csv(file)
head(data)
data <- data.frame(Time=data$TheTime, Counts=data$CH1_Hz)

library(lubridate)

data$Time <- floor_date(as.POSIXct(data$Time*60*60*24, origin = '1899-12-30', tz = 'UTC'), unit ='1 second')
head(data)

file2 <- file.choose()
data2 <- read.csv(file2)

colstart <- 2
colend <- 7

data2[,colstart:colend] <- sapply(data2[,colstart:colend], function(x) paste(date, x))
data2[,colstart:colend] <- sapply(data2[,colstart:colend], function(x) as.POSIXct(x, format = '%m/%d/%Y %H:%M', tz = 'UTC'))

head(data2)

startpoint <- sapply(data2[,colstart:colend], function(x) findclose(x, as.integer(data$Time)))
startpoint

startpoint[,1:6] <- sapply(startpoint, function(x) mean(data$Counts[(x+14):(x+59)]))
output <- data.frame(startpoint)

head(output)

output <- output %>%
  mutate(NO2 = NO2 - NO) %>%
  mutate(NO = NO - Bkgd) %>%
  mutate(Naf.NO2 = Naf.NO2 - Naf.NO) %>%
  mutate(HONO = Naf.NO - Naf.Bkgd - NO) %>%
  mutate(NO2 = NO2 - HONO * 0.08) %>% #Subtract HONO interference
  select(NO, NO2, Naf.NO, Naf.NO2, HONO)

head(output)

output <- (output - intercept)*(slope)^-1
output$NO2 <- output$NO2/.9
output$Naf.NO2 <- output$Naf.NO2/.9

head(output)

write.table(output, file = 'clipboard', sep = '\t', row.names = F)
