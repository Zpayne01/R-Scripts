##### N2O, CO, and H2O Measurements #####

## Open CSV file ##

file <- file.choose()
N2O <- read.csv(file, header = FALSE)

## Delete unnecessary rows and columns ##

N2O <- N2O[-1,]
N2O <- N2O[-1,]
N2O[11:28] <- list(NULL)
N2O <- N2O[-9]
N2O <- N2O[-7]
N2O <- N2O[-5]
N2O <- N2O[-3]

## Name the columns ##

colnames(N2O) <- c('datetime', 'N2O', 'CO', 'H2O', 'N2O_d', 'CO_d')

## Add NA values to blanks in N2O ##

N2O$N2O[N2O$N2O == ""] <- NA

## Get time ##

datetime <- strptime((N2O$datetime), format= '%m/%d/%Y %H:%M:%S', tz = 'EST')
N2O$datetime <- as.POSIXct(datetime)
N2O <- na.omit(N2O)


