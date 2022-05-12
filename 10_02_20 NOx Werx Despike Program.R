## NOx Werx Despike Program ##

library(TTR)
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)

## As list ##

noxcomb <- data.frame(
  TheTime = c(),
  CH1_Hz = c()
)

f <- list.files('D:/Surface 3/Main Folder/Documents/Lab Data/2020_09 IURTP/NOx/Raw Data/Post-Nafion Timing Issue/')

for(i in 1:length(f)) {
  noxfile <- read.csv(paste0('D:/Surface 3/Main Folder/Documents/Lab Data/2020_09 IURTP/NOx/Raw Data/Post-Nafion Timing Issue/', f[i]))
  noxfile <- data.frame(noxfile$TheTime, noxfile$CH1_Hz)
  colnames(noxfile) <- c('TheTime', 'CH1_Hz')
  
  noxcomb <- rbind(noxcomb, noxfile)
}

## For Single File

file <- file.choose()
noxfile <- read.csv(file)
noxcomb <- data.frame(noxfile$TheTime, noxfile$CH1_Hz)
colnames(noxcomb) <- c('TheTime', 'CH1_Hz')

## Delete Duplicated time points

noxcomb <- noxcomb[!duplicated(noxcomb[,c('TheTime')]),]
noxcomb$CH1_Hz[noxcomb$CH1_Hz < 0] <- NA

## Chamber the time format
## Use UTC for ease

noxcomb$TheTime <- floor_date(as.POSIXct(noxcomb$TheTime*60*60*24, origin = '1899-12-30', tz = 'UTC'), unit = '1 second')

## Get start and end times for input ##

print(noxcomb$TheTime[1])
print(noxcomb$TheTime[nrow(noxcomb)])


## Start and end of IURTP measurement campaign
## Use UTC for ease

start <- as.POSIXct('09/20/2020 12:15:00', format = '%m/%d/%Y %H:%M:%S', tz = 'UTC')
end <- as.POSIXct('09/25/2020 11:40:00',format = '%m/%d/%Y %H:%M:%S', tz = 'UTC') - 1

## Delete points before start and after end

startmatch <- match(as.integer(start), as.integer(noxcomb$TheTime))
endmatch <- match(as.integer(end), as.integer(noxcomb$TheTime))

noxcomb <- noxcomb[startmatch:endmatch, ]

## Insert nonvalues for missing points ##

ts <- seq.POSIXt(start, end, by = 'sec')
timedf <- data.frame(TheTime = ts)

noxcomb <- full_join(timedf, noxcomb)

## Estimate values for missing values based on an average of surrounding data points

noxcombnaapprox <- na.approx(noxcomb$CH1_Hz)
noxcomb$CH1_Hz <- noxcombnaapprox

## Put in minutes column again

datetime <- as.POSIXlt(noxcomb$TheTime, tz = 'America/New_York')
noxcomb$min <- datetime$min
noxcomb$sec <- datetime$sec

## Remove first 5 and last 5 seconds

noxcomb <- subset(noxcomb, noxcomb$sec >= 5 & noxcomb$sec <= 55)

## For loop for despike

nrow(noxcomb) / 51
noxcomb$edit <- 0
noxcomb$sum_na <- 0

for(i in 1:(nrow(noxcomb)/51)) {
  start <- 1 + 51 * (i - 1)
  end <- i * 51
  
  if (max(noxcomb$CH1_Hz[start:end], na.rm = TRUE) - min(noxcomb$CH1_Hz[start:end], na.rm = TRUE) > 400) {
    
    change <- noxcomb$CH1_Hz[start:end] 
    change[change >= summary(change)[3]] <- NA
    noxcomb$CH1_Hz[start:end] <- change
    
    noxcomb$edit[start:end] <- 1
    noxcomb$sum_na[start:end] <- sum(is.na(change))
    
  } else {
  }
  
}

## Write File ##

write.csv(noxcomb, file = "C:/Users/zacpayne/Desktop/IURTP NOx Despike.csv", row.names = FALSE)

## Sequence

sequence <- seq(1, nrow(noxcomb), 51)

noxcomb$sum_na[sequence]

## Hour by Hour

start <- '2020-09-20 19:02:05'
end <- '2020-09-20 22:02:55'

s1 <- match(as.POSIXct(start, tz = 'UTC'), noxcomb$TheTime)
s2 <- match(as.POSIXct(end, tz = 'UTC'), noxcomb$TheTime)

plot(noxcomb$TheTime[s1:s2], noxcomb$CH1_Hz[s1:s2], type = 'l')

test <- noxcomb[s1:s2,]
test$CH1_Hz[test$CH1_Hz >= test$CH1_Hz[3]] <- NA
test$CH1_Hz[is.na(test$CH1_Hz)] <- mean(test$CH1_Hz, na.rm = TRUE)

plot(test$TheTime, test$CH1_Hz, type = 'l')
