#### NOx Measurements Chamber ####

library(TTR)
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)

## Use Input

input_starttime <- '09_08_2020 15:50:00' ## Format MM_DD_YYYY HH:MM:SS
input_endtime <- '09_08_2020 17:30:00'

input_no_cal1 <- 280 #Through blank cell
input_no_cal2 <- 260 #Through Nafion
input_no_cal1_int <- 21 
input_no_cal2_int <- 19

input_hono_ce_naf <- 0.52

input_hono_ce_lamp <- 0.043
input_no2_ce_lamp <- 0.935

## Choose File ##

file <- file.choose()

noxfile <- read.table(file, sep=",", header = TRUE)
noxfile <- subset(noxfile, select=c(TheTime, CH1_Hz))
colnames(noxfile) <- c('TheTime', 'CH1_Hz')

## Remove repeated times and values less than 0 ##

noxfile <- noxfile[!duplicated(noxfile[,c('TheTime')]),]
noxfile$CH1_Hz[noxfile$CH1_Hz < 0] <- NA 

## Obtain Date and Time ##

datetime <- floor_date(as.POSIXct(noxfile$TheTime*60*60*24, origin = '1899-12-30', tz = 'UTC'), unit = '1 second')

## Adding 3 seconds to get rid of the annoying peaks due to three way valve switching ##

datetime <- datetime 
datetime <- as.POSIXlt(datetime)
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec
datetime <- as.POSIXct(datetime)

noxfile$TheTime <- datetime

## Insert non-values for missing points

start <- as.POSIXct(noxfile$TheTime[1])
end <- as.POSIXct(noxfile$TheTime[nrow(noxfile)])

ts <- seq.POSIXt(start, end, by = 'sec')

timedf <- data.frame(timestamp = ts)
colnames(timedf) = c('TheTime')

noxfile <- full_join(timedf, noxfile)

## Estimate missing time based on nearby data and remove repeated data points

noxfilenaapprox <- na.approx(noxfile$CH1_Hz)
noxfile$CH1_Hz <- noxfilenaapprox
noxfile <- noxfile[match(unique(noxfile$TheTime), noxfile$TheTime),]

## Delete up to starttime and endtime

starttime <- as.POSIXct(input_starttime, format = '%m_%d_%Y %H:%M:%S', tz = 'UTC')
endtime <- as.POSIXct(input_endtime, format = '%m_%d_%Y %H:%M:%S', tz = 'UTC')

startmatch <- match(as.integer(starttime), as.integer(noxfile$TheTime))
noxfile <- noxfile[startmatch:nrow(noxfile),]

endmatch <- match(as.integer(endtime-1), as.integer(noxfile$TheTime))
noxfile <- noxfile[1:endmatch,]

## Put in Minutes again ##

datetime <- as.POSIXlt(noxfile$TheTime, tz = "est")
noxfile$min <- datetime$min
noxfile$day <- datetime$mday
noxfile$hour <- datetime$hour
noxfile$sec <- datetime$sec

## Subset the last 30 secs of each measurement

trimsignal <- subset(noxfile, subset = (sec >= 30))

## Floor Minute in subset

trimsignal$TheTime <- floor_date(trimsignal$TheTime, unit = '1 minute')
trimsignal <- trimsignal[,1:2]

## Aggregate Data points counts

counts <- ddply(trimsignal, .(TheTime), summarise, mean(CH1_Hz), sd(CH1_Hz))
colnames(counts) <- c('datetime', 'counts_ave', 'counts_sd')

## Seperate by counts

bkgd1counts <- counts$counts_ave[seq(1, nrow(counts), 5)]
nocounts <- counts$counts_ave[seq(2, nrow(counts), 5)]
no2counts <- counts$counts_ave[seq(3, nrow(counts), 5)]
bkgd2counts <- counts$counts_ave[seq(4, nrow(counts), 5)]
honocounts <- counts$counts_ave[seq(5, nrow(counts), 5)]

bkgd1counts_sd <- counts$counts_sd[seq(1, nrow(counts), 5)]
nocounts_sd <- counts$counts_sd[seq(2, nrow(counts), 5)]
no2counts_sd <- counts$counts_sd[seq(3, nrow(counts), 5)]
bkgd2counts_sd <- counts$counts_sd[seq(4, nrow(counts), 5)]
honocounts_sd <- counts$counts_sd[seq(5, nrow(counts), 5)]

time <-  counts$datetime[seq(1, nrow(counts), 5)]

## Find the counts related to differences within the system

no <- nocounts - bkgd1counts
no_sd <- sqrt(nocounts_sd^2 + bkgd1counts_sd^2)

hono <- honocounts - bkgd2counts #This is technically NO + HONO counts
hono_sd <- sqrt(honocounts_sd^2 + bkgd2counts_sd^2)

no2 <- no2counts - nocounts
no2_sd <- sqrt(no2counts_sd^2 + nocounts_sd^2)

## Counts output

counts_output <- data.frame(time, no, no_sd, hono, hono_sd, no2, no2_sd)
colnames <- c('datetime', 'no_counts', 'no_counts_sd', 'hono_counts', 'hono_counts_sd', 'no2_counts', 'no2_counts_sd')

## Write counts file

#filename <- 'C:/Users/zacpayne/Desktop/counts.csv'
#write.csv(counts_output, filename, row.names = FALSE)

## Find Concentration

NOconc <- (no - input_no_cal1_int)*(input_no_cal1)^-1
NOconc[NOconc < 0] = 0
NOconc_sd <- (no_sd)*(input_no_cal1)^-1

NOcounts2 <- (NOconc)*(input_no_cal2) + input_no_cal2_int ## Counts of NO for the nafion converter

HONOconc <- (hono - NOcounts2)*(input_no_cal2)^-1*(input_hono_ce_naf)^-1 ## Concentration of HONO determined by Naf
HONOconc[HONOconc < 0] = 0
honoconc_sd <- (hono_sd)*(input_no_cal2)^-1*(input_hono_ce_naf)^-1

HONOcounts1 <- HONOconc*input_no_cal1*input_hono_ce_lamp ##Counts of HONO from the photolysis cell

no2conc <- (no2-HONOcounts1)*(input_no_cal1)^-1*(input_no2_ce_lamp)^-1 ## Concentration of NO2 by photolysis
no2conc[no2conc < 0] = 0
no2conc_sd <- (no2_sd)*(input_no_cal1)^-1*(input_no2_ce_lamp)^-1

#### Concentration Data.frame ####

concentration <- data.frame(
  datetime = time,
  no_ppb = NOconc,
  no_ppb_sd = NOconc_sd,
  no2_ppb = no2conc,
  no2_ppb_sd = no2conc_sd,
  hono_ppb = HONOconc,
  hono_ppb_sd = honoconc_sd
)

#### Export #####

filename = 'C:/Users/zacpayne/Desktop/Test with Evan.csv'
write.csv(concentration, filename, row.names = FALSE)

