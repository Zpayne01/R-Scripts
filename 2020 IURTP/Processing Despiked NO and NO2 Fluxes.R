## IURTP Despiked data flux measurements

## Inputs

input_no_cal1 <- 280 #Through blank cell
input_no_cal2 <- 260 #Through Nafion
input_no_cal1_int <- 21 
input_no_cal2_int <- 19

input_hono_ce_naf <- 0.52

input_hono_ce_lamp <- 0.043
input_no2_ce_lamp <- 0.935

## Upload Files 

noxfile <- file.choose()
h2ofile <- file.choose()

#

## Choose File ##

file <- file.choose()

noxfile <- read.csv(file, sep=",", header = TRUE)
noxfile <- data.frame(noxfile$TheTime, noxfile$CH1_Hz)
colnames(noxfile) <- c('TheTime', 'CH1_Hz')

noxfile$TheTime <- as.POSIXct(noxfile$TheTime*(60*60*24), origin = "1899-12-30", tz = 'UTC')

## Floor ##

noxfile$TheTime <- floor_date(noxfile$TheTime, unit = '1 minute')
counts <- ddply(noxfile, .(TheTime), summarize, counts_ave = mean(CH1_Hz, na.rm = TRUE),
                 counts_sd = sd(CH1_Hz, na.rm = TRUE) #, edit = sum(CH1_Hz)
                 )

## 

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

time <-  counts$TheTime[seq(1, nrow(counts), 5)]

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

## Concentration

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

filename = 'C:/Users/zacpayne/Desktop/IURTP.csv'
write.csv(concentration, filename, row.names = FALSE)
