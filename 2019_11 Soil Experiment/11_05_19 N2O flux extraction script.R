library(lubridate)

## SEARCH FOR INPUT BEFORE RUNNING THIS SCRIPT ##

# Select str file for measurement

file <- file.choose()
csv <- read.table(file, sep = ' ', skip = 1, header = FALSE) #skip the first line of the table
csv <- csv[,1:8]

# Column names according to STR header 

colnames(csv) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')

csv$datetime <- as.POSIXct(csv$datetime, origin = "1904-01-01 00:00:00", tz = 'UTC')
head(csv)

plot(csv$'446' ~ csv$datetime, type = 'l')

# Input starttime and endtime

starttime <- as.POSIXct("11/02/2019 00:00:01", format = "%m/%d/%Y %H:%M:%S", tz = 'UTC')
endtime <- as.POSIXct("11/02/2019 14:00:00", format = "%m/%d/%Y %H:%M:%S", tz = 'UTC')

startmatch <- match(as.character(starttime), as.character(csv$datetime))
endmatch <- match(as.character(endtime), as.character(csv$datetime))

measured <- csv[startmatch:endmatch,]

# Subset only the last 3 minutes of each measurements

measured$datetime <- as.POSIXlt(measured$datetime)

measured$datetime$min

measuredsubset <- measured[measured$datetime$min %in% c(2,3,4,12,13,14,17,18,19,27,28,29,32,33,34,42,43,44,47,48,49,57,58,59),]

measuredsubset$datetime <- floor_date(measuredsubset$datetime, unit = '5 minutes')
measuredsubset$datetime <- as.POSIXct(measuredsubset$datetime)

average <- aggregate(. ~ datetime, measuredsubset, mean)
standarddeviation <- aggregate(. ~ datetime, measuredsubset, sd)

# Change the datetime back to a list and combine

average$datetime <- as.POSIXlt(average$datetime)
standarddeviation$datetime <- as.POSIXlt(standarddeviation$datetime)

combine <- data.frame(average$datetime, average$'446', standarddeviation$'446', average$'456', standarddeviation$'456',
                      average$'546', standarddeviation$'546', average$'447', standarddeviation$'447', average$N2O, standarddeviation$N2O,
                      average$CO2, standarddeviation$CO2, average$'448', standarddeviation$'448')
colnames(combine) <- c('datetime', '446', '446 sd', '456', '456 sd', '546', '546 sd', '447', '447 sd', 'N2O', 'N2O sd', 'CO2', 'CO2 sd', '448', '448 sd') 
combine$datetime <- as.POSIXlt(combine$datetime)
  
# Seperate into chambers

chamberB <- subset(combine, combine$datetime$min == 10)
chamberC <- subset(combine, combine$datetime$min == 25)
chamberD <- subset(combine, combine$datetime$min == 40)
chamberE <- subset(combine, combine$datetime$min == 55)

bkgd1 <- subset(combine, combine$datetime$min == 15)
bkgd2 <- subset(combine, combine$datetime$min == 30)
bkgd3 <- subset(combine, combine$datetime$min == 45)

# Calculate N2O Fluxes

# Input Flows

flowB <- 3.131
flowC <- 3.186
flowD <- 3.190
flowE <- 3.112

massB <- 29.48
massC <- 28.54
massD <- 28.42
massE <- 28.37

matchB <- match(chamberB$datetime$hour, bkgd1$datetime$hour)
matchC1 <- match(chamberC$datetime$hour, bkgd1$datetime$hour)
matchC2 <- match(chamberC$datetime$hour, bkgd2$datetime$hour)
matchD1 <- match(chamberD$datetime$hour, bkgd2$datetime$hour)
matchD2 <- match(chamberD$datetime$hour, bkgd3$datetime$hour)
matchE <- match(chamberE$datetime$hour, bkgd3$datetime$hour)

difB <- chamberB$'446' - bkgd1[matchB,2]
difC <- chamberC$'446' - rowMeans(data.frame(bkgd1[matchC1,2], bkgd2[matchC2,2]), na.rm = TRUE)
difD <- chamberD$'446' - rowMeans(data.frame(bkgd2[matchD1,2], bkgd3[matchD2,2]), na.rm = TRUE)
difE <- chamberE$'446' - bkgd3[matchE,2]

difBsd <- sqrt(chamberB$'446 sd'^2 + bkgd1[matchB, 3]^2)
difCsd <- sqrt(chamberC$'446 sd'^2 + bkgd2[matchC2, 3]^2)
difDsd <- sqrt(chamberD$'446 sd'^2 + bkgd3[matchD2, 3]^2)
difEsd <- sqrt(chamberE$'446 sd'^2 + bkgd3[matchE, 3]^2)

# Temperature = 69.7 F or 294.04 K
# Pressure = 101325 Pa
# Liters per mol = 24.1

# Flux in ng N (g soil hr)-1 = dC(ppb) * 1 ppb N2O/10^9 ppb air * 1 mol air/ 24.1 L air * flow (L / min) * 60 min/1 hr * 28 g N/ mol N2O * 10^9 ng N/g N / mass soil (g)

fluxB <- difB * 1/10^9 * 1/24.1 * flowB * 60/1 * 28/1 * 10^9/1 / massB
fluxC <- difC * 1/10^9 * 1/24.1 * flowC * 60/1 * 28/1 * 10^9/1 / massC
fluxD <- difD * 1/10^9 * 1/24.1 * flowD * 60/1 * 28/1 * 10^9/1 / massD
fluxE <- difE * 1/10^9 * 1/24.1 * flowE * 60/1 * 28/1 * 10^9/1 / massE

fluxBsd <- difBsd * 1/10^9 * 1/24.1 * flowB * 60/1 * 28/1 * 10^9/1 / massB
fluxCsd <- difCsd * 1/10^9 * 1/24.1 * flowC * 60/1 * 28/1 * 10^9/1 / massC
fluxDsd <- difDsd * 1/10^9 * 1/24.1 * flowD * 60/1 * 28/1 * 10^9/1 / massD
fluxEsd <- difEsd * 1/10^9 * 1/24.1 * flowE * 60/1 * 28/1 * 10^9/1 / massE

## Determining isotopologues 


#Hitran Isotopic Ratios

n2o446 <- .9903
n2o456 <- .003641
n2o465 <- .003641
n2o448 <- .001986

#Calculating Del Value according to Wintel Manual 

d456bkgd1 <- (bkgd1$`456`/bkgd1$`446` - 1) * 1000
d546bkgd1 <- (bkgd1$`546`/bkgd1$`446` - 1) * 1000
d448bkgd1 <- (bkgd1$`448`/bkgd1$`446` - 1) * 1000

d456bkgd2 <- (bkgd2$`456`/bkgd2$`446` - 1) * 1000
d546bkgd2 <- (bkgd2$`546`/bkgd2$`446` - 1) * 1000
d448bkgd2 <- (bkgd2$`448`/bkgd2$`446` - 1) * 1000

d456bkgd3 <- (bkgd3$`456`/bkgd3$`446` - 1) * 1000
d546bkgd3 <- (bkgd3$`546`/bkgd3$`446` - 1) * 1000
d448bkgd3 <- (bkgd3$`448`/bkgd3$`446` - 1) * 1000

d456B <- (chamberB$`456`/chamberB$`446` - 1) * 1000
d546B <- (chamberB$`546`/chamberB$`446` - 1) * 1000
d448B <- (chamberB$`448`/chamberB$`446` - 1) * 1000

d456C <- (chamberC$`456`/chamberC$`446` - 1) * 1000
d546C <- (chamberC$`546`/chamberC$`446` - 1) * 1000
d448C <- (chamberC$`448`/chamberC$`446` - 1) * 1000

d456D <- (chamberD$`456`/chamberD$`446` - 1) * 1000
d546D <- (chamberD$`546`/chamberD$`446` - 1) * 1000
d448D <- (chamberD$`448`/chamberD$`446` - 1) * 1000

d456E <- (chamberE$`456`/chamberE$`446` - 1) * 1000
d546E <- (chamberE$`546`/chamberE$`446` - 1) * 1000
d448E <- (chamberE$`448`/chamberE$`446` - 1) * 1000

#Del difference 

d456Bdif <- d456B - d456bkgd1[matchB]
d546Bdif <- d546B - d546bkgd1[matchB]
d448Bdif <- d448B - d448bkgd1[matchB]

d456Cdif <- d456C - rowMeans(data.frame(d456bkgd1[matchC1], d456bkgd2[matchC2]), na.rm = TRUE)
d546Cdif <- d546C - rowMeans(data.frame(d546bkgd1[matchC1], d546bkgd2[matchC2]), na.rm = TRUE)
d448Cdif <- d448C - rowMeans(data.frame(d448bkgd1[matchC1], d448bkgd2[matchC2]), na.rm = TRUE)

d456Ddif <- d456D - rowMeans(data.frame(d456bkgd2[matchD1], d456bkgd3[matchD2]), na.rm = TRUE)
d546Ddif <- d546D - rowMeans(data.frame(d546bkgd2[matchD1], d546bkgd3[matchD2]), na.rm = TRUE)
d448Ddif <- d448D - rowMeans(data.frame(d448bkgd2[matchD1], d448bkgd3[matchD2]), na.rm = TRUE)

d456Edif <- d456E - d456bkgd3[matchE]
d546Edif <- d546E - d546bkgd3[matchE]
d448Edif <- d448E - d448bkgd3[matchE]

# Output for each chamber

outputB <- data.frame(chamberB$datetime, fluxB, fluxBsd, d456Bdif, d546Bdif, d448Bdif) 
outputC <- data.frame(chamberC$datetime, fluxC, fluxCsd, d456Cdif, d546Cdif, d448Cdif)
outputD <- data.frame(chamberD$datetime, fluxD, fluxDsd, d456Ddif, d546Ddif, d448Ddif)
outputE <- data.frame(chamberE$datetime, fluxE, fluxEsd, d456Edif, d546Edif, d448Edif)

# Output Combined

minhour <- min(c(chamberB$datetime$hour[1], chamberC$datime$hour[1], chamberD$datetime$hour[1], chamberE$datetime$hour[1]))
maxhour <- max(c(chamberB$datetime$hour[nrow(chamberB)], chamberC$datime$hour[nrow(chamberB)], chamberD$datetime$hour[nrow(chamberB)], chamberE$datetime$hour[nrow(chamberB)]))

matchtimeB <- match(seq(minhour, maxhour, 1), chamberB$datetime$hour)
matchtimeC <- match(seq(minhour, maxhour, 1), chamberC$datetime$hour)
matchtimeD <- match(seq(minhour, maxhour, 1), chamberD$datetime$hour)
matchtimeE <- match(seq(minhour, maxhour, 1), chamberE$datetime$hour)

date <- as.Date(chamberB$datetime[1])

output <- data.frame(datetime = paste0(date,' ',seq(minhour, maxhour, 1),':00:00'),
                     hour = seq(minhour, maxhour, 1),
                     fluxB = fluxB[matchtimeB], fluxBsd = fluxBsd[matchtimeB], d456Bdif = d456Bdif[matchtimeB],
                     d546Bdif = d546Bdif[matchtimeB], d448Bdif = d448Bdif[matchtimeB],
                     fluxC = fluxC[matchtimeC], fluxCsd = fluxCsd[matchtimeC], d456Cdif = d456Bdif[matchtimeC],
                     d546Cdif = d546Cdif[matchtimeC], d448Cdif = d448Cdif[matchtimeC],
                     fluxD = fluxD[matchtimeD], fluxDsd = fluxDsd[matchtimeD], d456Ddif = d456Ddif[matchtimeD],
                     d546Ddif = d546Ddif[matchtimeD], d448Ddif = d448Ddif[matchtimeD],
                     fluxE = fluxE[matchtimeE], fluxEsd = fluxEsd[matchtimeE], d456Edif = d456Edif[matchtimeE],
                     d546Edif = d546Edif[matchtimeE], d448Edif = d448Ddif[matchtimeE])

head(output)
filename <- paste0('D:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Fluxes/',date,'.csv')

write.csv(output, file = filename, row.names = FALSE)
