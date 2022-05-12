## N2O processing program

library(lubridate)
library(ggplot2)

#### Automated N2O Processing ####

## Select which sample type you are running

Continuous = FALSE
Trapped = TRUE

## Input folder containing the relevant N2O files

filelist <- list.files('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Raw N2O', pattern = '.str')


## Blank dataframe for output file

n2o <- data.frame(datetime = c(),
                  B = c(),
                  B_sd = c(),
                  C = c(),
                  C_sd = c(),
                  D = c(),
                  D_sd = c(),
                  E = c(),
                  E_sd = c())

n2oiso456 <- data.frame(datetime = c(),
                        bkgd1ave = c(),
                        bkgd1sd = c(),
                        bkgd2ave = c(),
                        bkgd2sd = c(),
                        bkgd3ave = c(),
                        bkgd3sd = c(),
                        B = c(),
                        B_sd = c(),
                        C = c(),
                        C_sd = c(),
                        D = c(),
                        D_sd = c(),
                        E = c(),
                        E_sd = c()
                     )

n2oiso546 <- data.frame(datetime = c(),
                        bkgd1ave = c(),
                        bkgd1sd = c(),
                        bkgd2ave = c(),
                        bkgd2sd = c(),
                        bkgd3ave = c(),
                        bkgd3sd = c(),
                        B = c(),
                        B_sd = c(),
                        C = c(),
                        C_sd = c(),
                        D = c(),
                        D_sd = c(),
                        E = c(),
                        E_sd = c()
                        )

n2oiso448 <- data.frame(datetime = c(),
                        bkgd1ave = c(),
                        bkgd1sd = c(),
                        bkgd2ave = c(),
                        bkgd2sd = c(),
                        bkgd3ave = c(),
                        bkgd3sd = c(),
                        B = c(),
                        B_sd = c(),
                        C = c(),
                        C_sd = c(),
                        D = c(),
                        D_sd = c(),
                        E = c(),
                        E_sd = c()
  
)

## Loop for processing each N2O file

if (Continuous == TRUE) {
for (i in 1:length(filelist)) {

## Read and process file by position in filelist  
  
file <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Raw N2O/',filelist[i])  

data <- read.table(file, sep = ' ', skip = 1, header = FALSE)
data <- data[,1:8]

colnames(data) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')

#### File error control ####
## Sometimes files do not start or end at midnight which causes difficulties when trying to automate the flux calculation process

if(i == 16) {
  data <- data[-1:-1069,]
}

if(i == 15) {
  data <- data[-46800:-nrow(data),]
}

## Change the string to a relevant time format

data$datetime <- as.POSIXct(data$datetime, origin = "1904-01-01 00:00:00", tz = 'UTC')
print(head(data))

data$datetime <- as.POSIXlt(data$datetime)

data$datetime$min

## Subset and average based on the protocol designed for the continuous flow experiment

datasubset <- data[data$datetime$min %in% c(6,7,8,17,18,19,21,22,23,32,33,34,36,37,38,47,48,49,51,52,53),]

datasubset$datetime <- floor_date(datasubset$datetime, unit = '5 minutes')
datasubset$datetime <- as.POSIXct(datasubset$datetime)

average <- aggregate(. ~ datetime, datasubset, mean)
standarddeviation <- aggregate(. ~ datetime, datasubset, sd)

average$datetime <- as.POSIXlt(average$datetime)
standarddeviation$datetime <- as.POSIXlt(standarddeviation$datetime)

combine <- data.frame(average$datetime, average$'446', standarddeviation$'446', average$'456', standarddeviation$'456',
                      average$'546', standarddeviation$'546', average$'447', standarddeviation$'447', average$N2O, standarddeviation$N2O,
                      average$CO2, standarddeviation$CO2, average$'448', standarddeviation$'448')
colnames(combine) <- c('datetime', '446', '446 sd', '456', '456 sd', '546', '546 sd', '447', '447 sd', 'N2O', 'N2O sd', 'CO2', 'CO2 sd', '448', '448 sd') 
combine$datetime <- as.POSIXlt(combine$datetime)

#### Flux chamber Calculations ####
## Separate into each respect chamber

chamberB <- subset(combine, combine$datetime$min == 5)
chamberC <- subset(combine, combine$datetime$min == 20)
chamberD <- subset(combine, combine$datetime$min == 35)
chamberE <- subset(combine, combine$datetime$min == 50)

## Separate background measurements

bkgd1 <- subset(combine, combine$datetime$min == 15)
bkgd2 <- subset(combine, combine$datetime$min == 30)
bkgd3 <- subset(combine, combine$datetime$min == 45)

## Match background measurements to thier respective chamber measurements

matchB <- match(chamberB$datetime$hour, bkgd1$datetime$hour)
matchC1 <- match(chamberC$datetime$hour, bkgd1$datetime$hour)
matchC2 <- match(chamberC$datetime$hour, bkgd2$datetime$hour)
matchD1 <- match(chamberD$datetime$hour, bkgd2$datetime$hour)
matchD2 <- match(chamberD$datetime$hour, bkgd3$datetime$hour)
matchE <- match(chamberE$datetime$hour, bkgd3$datetime$hour)


#### Standard Isotope N2O Flux ####
## Take the difference in concentration between the blank and the chamber

difB <- chamberB$'446' - bkgd1[matchB,2]
difC <- chamberC$'446' - rowMeans(data.frame(bkgd1[matchC1,2], bkgd2[matchC2,2]), na.rm = TRUE)
difD <- chamberD$'446' - rowMeans(data.frame(bkgd2[matchD1,2], bkgd3[matchD2,2]), na.rm = TRUE)
difE <- chamberE$'446' - bkgd3[matchE,2]

difBsd <- sqrt(chamberB$'446 sd'^2 + bkgd1[matchB, 3]^2)
difCsd <- sqrt(chamberC$'446 sd'^2 + bkgd2[matchC2, 3]^2)
difDsd <- sqrt(chamberD$'446 sd'^2 + bkgd3[matchD2, 3]^2)
difEsd <- sqrt(chamberE$'446 sd'^2 + bkgd3[matchE, 3]^2)

## Append the blank n2o table

output <- data.frame(floor_date(bkgd1$datetime, unit = '1 hour'), difB, difBsd, difC, difCsd, difD, difDsd, difE, difEsd)
colnames(output) <- c('datetime', 'B', 'B_sd', 'C', 'C_sd', 'D', 'D_sd', 'E', 'E_sd')

n2o <- rbind(n2o, output)
print(tail(n2o))

#### Isotopic del values (per mil) ####

## Hitran Isotopic Ratios (used in the program to find the levels of 456, 546, and 448)

n2o446 <- .9903
n2o456 <- .003641
n2o456 <- .003641
n2o448 <- .001986

## Calculating del values according to Wintel Manual relative to the HITRAN database



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

## Deviations in del values

d456bkgd1sd <- sqrt((bkgd1$'456 sd'/bkgd1$'456')^2 + (bkgd1$'446 sd'/bkgd1$'446')^2) * d456bkgd1 
d546bkgd1sd <- sqrt((bkgd1$'546 sd'/bkgd1$'546')^2 + (bkgd1$'446 sd'/bkgd1$'446')^2) * d546bkgd1 
d448bkgd1sd <- sqrt((bkgd1$'448 sd'/bkgd1$'448')^2 + (bkgd1$'446 sd'/bkgd1$'446')^2) * d448bkgd1 

d456bkgd2sd <- sqrt((bkgd2$'456 sd'/bkgd2$'456')^2 + (bkgd2$'446 sd'/bkgd2$'446')^2) * d456bkgd1 
d546bkgd2sd <- sqrt((bkgd2$'546 sd'/bkgd2$'546')^2 + (bkgd2$'446 sd'/bkgd2$'446')^2) * d546bkgd1 
d448bkgd2sd <- sqrt((bkgd2$'448 sd'/bkgd2$'448')^2 + (bkgd2$'446 sd'/bkgd2$'446')^2) * d448bkgd1 

d456bkgd3sd <- sqrt((bkgd3$'456 sd'/bkgd3$'456')^2 + (bkgd3$'446 sd'/bkgd3$'446')^2) * d456bkgd1 
d546bkgd3sd <- sqrt((bkgd3$'546 sd'/bkgd3$'546')^2 + (bkgd3$'446 sd'/bkgd3$'446')^2) * d546bkgd1 
d448bkgd3sd <- sqrt((bkgd3$'448 sd'/bkgd3$'448')^2 + (bkgd3$'446 sd'/bkgd3$'446')^2) * d448bkgd1 

d456Bsd <- sqrt((chamberB$'456 sd'/chamberB$'456')^2 + (chamberB$'446 sd'/chamberB$'446')^2) * d456B 
d546Bsd <- sqrt((chamberB$'546 sd'/chamberB$'546')^2 + (chamberB$'446 sd'/chamberB$'446')^2) * d546B
d448Bsd <- sqrt((chamberB$'448 sd'/chamberB$'448')^2 + (chamberB$'446 sd'/chamberB$'446')^2) * d448B

d456Csd <- sqrt((chamberC$'456 sd'/chamberC$'456')^2 + (chamberC$'446 sd'/chamberC$'446')^2) * d456C 
d546Csd <- sqrt((chamberC$'546 sd'/chamberC$'546')^2 + (chamberC$'446 sd'/chamberC$'446')^2) * d546C
d448Csd <- sqrt((chamberC$'448 sd'/chamberC$'448')^2 + (chamberC$'446 sd'/chamberC$'446')^2) * d448C

d456Dsd <- sqrt((chamberD$'456 sd'/chamberD$'456')^2 + (chamberD$'446 sd'/chamberD$'446')^2) * d456D 
d546Dsd <- sqrt((chamberD$'546 sd'/chamberD$'546')^2 + (chamberD$'446 sd'/chamberD$'446')^2) * d546D
d448Dsd <- sqrt((chamberD$'448 sd'/chamberD$'448')^2 + (chamberD$'446 sd'/chamberD$'446')^2) * d448D

d456Esd <- sqrt((chamberE$'456 sd'/chamberE$'456')^2 + (chamberE$'446 sd'/chamberE$'446')^2) * d456E 
d546Esd <- sqrt((chamberE$'546 sd'/chamberE$'546')^2 + (chamberE$'446 sd'/chamberE$'446')^2) * d546E
d448Esd <- sqrt((chamberE$'448 sd'/chamberE$'448')^2 + (chamberE$'446 sd'/chamberE$'446')^2) * d448E

## Update table for each isotopologue

#456

output <- data.frame(floor_date(bkgd1$datetime, unit = '1 hour'), d456bkgd1, d456bkgd1sd, d456bkgd2, d456bkgd2sd,
                     d456bkgd3, d456bkgd3sd, d456B, d456Bsd, d456C, d456Csd, d456D, d456Dsd, d456E, d456Esd)
colnames(output) <- c('datetime', 'bkgd1ave', 'bkgd1sd', 'bkgd2ave', 'bkgd2sd', 'bkgd3ave', 'bkgd3sd', 'B', 'B_sd',
                      'C', 'C_sd', 'D', 'D_sd', 'E', 'E_sd')
n2oiso456 <- rbind(n2oiso456, output)

print(tail(n2oiso456))

#546

output <- data.frame(floor_date(bkgd1$datetime, unit = '1 hour'), d546bkgd1, d546bkgd1sd, d546bkgd2, d546bkgd2sd,
                     d546bkgd3, d546bkgd3sd, d546B, d546Bsd, d546C, d546Csd, d546D, d546Dsd, d546E, d546Esd)
colnames(output) <- c('datetime', 'bkgd1ave', 'bkgd1sd', 'bkgd2ave', 'bkgd2sd', 'bkgd3ave', 'bkgd3sd', 'B', 'B_sd',
                      'C', 'C_sd', 'D', 'D_sd', 'E', 'E_sd')
n2oiso546 <- rbind(n2oiso546, output)

print(tail(n2oiso546))

#448

output <- data.frame(floor_date(bkgd1$datetime, unit = '1 hour'), d448bkgd1, d448bkgd1sd, d448bkgd2, d448bkgd2sd,
                     d448bkgd3, d448bkgd3sd, d448B, d448Bsd, d448C, d448Csd, d448D, d448Dsd, d448E, d448Esd)
colnames(output) <- c('datetime', 'bkgd1ave', 'bkgd1sd', 'bkgd2ave', 'bkgd2sd', 'bkgd3ave', 'bkgd3sd', 'B', 'B_sd',
                      'C', 'C_sd', 'D', 'D_sd', 'E', 'E_sd')
n2oiso448 <- rbind(n2oiso448, output)

print(tail(n2oiso448))

}

#### Flux calculation subscript ####

starttimefile <- 'F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Start and End Times.txt'

starttimetable <- read.csv(starttimefile, stringsAsFactors = FALSE)

for (i in 1:nrow(starttimetable)) {
  
  startdate <- starttimetable$start.date[i]
  starthour <- starttimetable$starttime[i]

  starttime <- as.POSIXct(paste0(startdate,' ',starthour, ':00:00'), format = '%m/%d/%Y %H:%M:%S', tz = 'UTC')
  
  enddate <- starttimetable$enddate[i]
  endhour <- starttimetable$endtime[i]
  
  endtime <- as.POSIXct(paste0(enddate,' ',endhour, ':00:00'), format = '%m/%d/%Y %H:%M:%S', tz = 'UTC')
  
  startmatch <- match(starttime, n2o$datetime)
  endmatch <- match(endtime, n2o$datetime)

  data <- n2o[startmatch:endmatch,]
  
  datetime <- data$datetime
  
  #### Mass Info ####
  ## Upload the mass for determination of flux
  
  massfile <- 'F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Mass of Dried Soil.txt'
  masstable <- read.csv(massfile, stringsAsFactors = FALSE, header = TRUE)
  
  ## Contents of each chamber ##
  
  chamberfile <- read.csv(file = "F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Chamber Log.txt", header = TRUE, stringsAsFactors = FALSE)
  chamberfile$Date <- as.POSIXct(chamberfile$Date, format = '%m/%d/%Y')
  
  chamberdate <- match(as.Date(datetime[1]), as.Date(chamberfile$Date))
  
  contentsb <- chamberfile[chamberdate, 2]
  contentsc <- chamberfile[chamberdate, 4]
  contentsd <- chamberfile[chamberdate, 6]
  contentse <- chamberfile[chamberdate, 8]
  
  plotb <- chamberfile[chamberdate, 3]
  plotc <- chamberfile[chamberdate, 5]
  plotd <- chamberfile[chamberdate, 7]
  plote <- chamberfile[chamberdate, 9]
  
  ## Match the date of the current loop iteration with the date in the mass file
  
  massmatch <- match(startdate, masstable$Date)
  
  massb <- masstable$chamberBMass[i]
  massc <- masstable$chamberCMass[i]
  massd <- masstable$chamberDMass[i]
  masse <- masstable$chamberEMass[i]
  
  #### Flow Info ####
  ## Upload the flow for determination of flux
  
  flowfile <- 'F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Flow Rates.txt'
  flowtable <- read.csv(flowfile, stringsAsFactors = FALSE, header = TRUE)
  
  ## Match the date of the current loop iteration with the date in the flow file
  
  flowmatch <- match(startdate, flowtable$Date)
  
  flowb <- flowtable$FlowRateB[i]
  flowc <- flowtable$FlowRateC[i]
  flowd <- flowtable$FlowRateD[i]
  flowe <- flowtable$FlowRateE[i]
  
  #### Flux calculation ####
  # Flux in ng N (g soil hr)-1 = dC(ppb) * 1 ppb N2O/10^9 ppb air * 1 mol air/ 24.1 L air * flow (L / min) * 60 min/1 hr * 28 g N/ mol N2O * 10^9 ng N/g N / mass soil (g)
  
  fluxb <- data$B * 1/10^9 * 1/24.1 * flowb * 60/1 * 28/1 * 10^9/1 / massb
  fluxc <- data$C * 1/10^9 * 1/24.1 * flowc * 60/1 * 28/1 * 10^9/1 / massc
  fluxd <- data$D * 1/10^9 * 1/24.1 * flowd * 60/1 * 28/1 * 10^9/1 / massd
  fluxe <- data$E * 1/10^9 * 1/24.1 * flowe * 60/1 * 28/1 * 10^9/1 / masse
  
  fluxbsd <- data$B_sd * 1/10^9 * 1/24.1 * flowb * 60/1 * 28/1 * 10^9/1 / massb
  fluxcsd <- data$C_sd * 1/10^9 * 1/24.1 * flowc * 60/1 * 28/1 * 10^9/1 / massc
  fluxdsd <- data$D_sd * 1/10^9 * 1/24.1 * flowd * 60/1 * 28/1 * 10^9/1 / massd
  fluxesd <- data$E_sd * 1/10^9 * 1/24.1 * flowe * 60/1 * 28/1 * 10^9/1 / masse
  
  
  
  ## Flux output information
  
  date <- as.Date(data$datetime[1])
  output <- data.frame(datetime = data$datetime, hour = seq(0,length(data$datetime)-1,1), 
                       Flux_B = fluxb, Flux_B_sd = fluxbsd,
                       Flux_C = fluxc, Flux_C_sd = fluxcsd,
                       Flux_D = fluxd, Flux_D_sd = fluxdsd,
                       Flux_E = fluxe, Flux_E_sd = fluxesd)
  head(output)
  filename <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Fluxes/',date,'.csv')
  
  write.csv(output, file = filename, row.names = FALSE)
  
  ## Plot the flux
  
  fluxbforplot <- data.frame(fluxb, fluxbsd, seq(0,length(fluxb)-1, 1), paste0(contentsb,':',plotb))
  fluxcforplot <- data.frame(fluxb, fluxcsd, seq(0,length(fluxc)-1, 1), paste0(contentsc,':',plotc))
  fluxdforplot <- data.frame(fluxd, fluxdsd, seq(0,length(fluxd)-1, 1), paste0(contentsd,':',plotd))
  fluxeforplot <- data.frame(fluxe, fluxesd, seq(0,length(fluxe)-1, 1), paste0(contentse,':',plote))
  
  names <- c('n2o_flux', 'n2o_flux_sd', 'hour', 'chamber')
  
  colnames(fluxbforplot) <- names
  colnames(fluxcforplot) <- names
  colnames(fluxdforplot) <- names
  colnames(fluxeforplot) <- names
  
  flux <- rbind(fluxbforplot, fluxcforplot, fluxdforplot, fluxeforplot)
  
  filedate <- gsub('/', '_', startdate)
  
  n2oplot <- ggplot(data = flux, aes(x = hour, y = n2o_flux, color = chamber)) + geom_line() + 
    labs(title = paste('N2O flux for', format(as.Date(datetime[1]), "%m/%d/%Y")),
         x = 'Time Since Start (hr.)',
         y = expression(paste('N'[2],'O flux (ug N kg' ^-1,'dray soil hr' ^-1, ')')),
         color = "Treatment - Plot")
  
  n2oplot <- n2oplot + theme_bw() + theme(plot.title = element_text(size = 32, hjust = 0.5))
  
  png(paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/N2O Flux Plots/',filedate,'.png'))
  
  print(n2oplot)
  
  dev.off()
  
  ## Isotope Output ##
  
  #456
  
  startmatch <- match(starttime, n2oiso456$datetime)
  endmatch <- match(endtime, n2oiso456$datetime)
  
  data <- n2oiso456[startmatch:endmatch,]
  
  filename <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/456/', filedate,'.csv')
  write.csv(data, file = filename, row.names = FALSE)
  
  #546 
  
  startmatch <- match(starttime, n2oiso546$datetime)
  endmatch <- match(endtime, n2oiso546$datetime)
  
  data <- n2oiso546[startmatch:endmatch,]
  
  filename <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/546/', filedate,'.csv')
  write.csv(data, file = filename, row.names = FALSE)
  
  #448
  
  startmatch <- match(starttime, n2oiso448$datetime)
  endmatch <- match(endtime, n2oiso448$datetime)
  
  data <- n2oiso448[startmatch:endmatch,]
  
  filename <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/448/', filedate,'.csv')
  write.csv(data, file = filename, row.names = FALSE)
  
}}

#### Trapped Flow Experiment ####

if (Trapped == TRUE) {
  
  n2o <- data.frame(datetime = c(),
                    A = c(),
                    A_sd = c(),
                    C = c(),
                    C_sd = c(),
                    D = c(),
                    D_sd = c())
  
  n2oiso456 <- data.frame(datetime = c(),
                          bkgd1ave = c(),
                          bkgd1sd = c(),
                          bkgd2ave = c(),
                          bkgd2sd = c(),
                          bkgd3ave = c(),
                          bkgd3sd = c(),
                          A = c(),
                          A_sd = c(),
                          C = c(),
                          C_sd = c(),
                          D = c(),
                          D_sd = c()
  )
  
  n2oiso546 <- data.frame(datetime = c(),
                          bkgd1ave = c(),
                          bkgd1sd = c(),
                          bkgd2ave = c(),
                          bkgd2sd = c(),
                          bkgd3ave = c(),
                          bkgd3sd = c(),
                          A = c(),
                          A_sd = c(),
                          C = c(),
                          C_sd = c(),
                          D = c(),
                          D_sd = c()

  )
  
  n2oiso448 <- data.frame(datetime = c(),
                          bkgd1ave = c(),
                          bkgd1sd = c(),
                          bkgd2ave = c(),
                          bkgd2sd = c(),
                          bkgd3ave = c(),
                          bkgd3sd = c(),
                          A = c(),
                          A_sd = c(),
                          C = c(),
                          C_sd = c(),
                          D = c(),
                          D_sd = c()
  )                        
  
                          
  
  for (i in 1:length(filelist)) {
    
    ## Read and process file by position in filelist  
    
    file <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Raw N2O/',filelist[i])  
    
    data <- read.table(file, sep = ' ', skip = 1, header = FALSE)
    data <- data[,1:8]
    
    colnames(data) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')
    
    #### File error control ####
    ## Sometimes files do not start or end at midnight which causes difficulties when trying to automate the flux calculation process
    
    if(i == 16) {
      data <- data[-1:-1069,]
    }
    
    if(i == 15) {
      data <- data[-46800:-nrow(data),]
    }
    
    ## Change the string to a relevant time format
    
    data$datetime <- as.POSIXct(data$datetime, origin = "1904-01-01 00:00:00", tz = 'UTC')
    print(head(data))
    
    data$datetime <- as.POSIXlt(data$datetime)
    
    data$datetime$min
    
    ## Subset and average based on the protocol designed for the continuous flow experiment
    
    #bkgd1 = 17,18,19; bkgd2 = 32,33,34; bkgd3 = 47,48,49
    #chambera = 13, 14; chamberc = 28, 29; chamberd = 43, 44
    
    datasubset <- data[data$datetime$min %in% c(13, 14, 17, 18, 19, 28, 29, 32, 33, 34, 43, 44, 47, 48, 49),]
  
    datasubset$datetime <- floor_date(datasubset$datetime, unit = '5 minutes')
    datasubset$datetime <- as.POSIXct(datasubset$datetime)
    
    average <- aggregate(. ~ datetime, datasubset, mean)
    standarddeviation <- aggregate(. ~ datetime, datasubset, sd)
    
    average$datetime <- as.POSIXlt(average$datetime)
    standarddeviation$datetime <- as.POSIXlt(standarddeviation$datetime)
    
    #bkgd 1 = 15, bkgd 2 = 30, bkgd 3 = 45
    #chambera = 10, chamberc = 25, chamberd = 40  
    
    combine <- data.frame(average$datetime, average$'446', standarddeviation$'446', average$'456', standarddeviation$'456',
                          average$'546', standarddeviation$'546', average$'447', standarddeviation$'447', average$N2O, standarddeviation$N2O,
                          average$CO2, standarddeviation$CO2, average$'448', standarddeviation$'448')
    colnames(combine) <- c('datetime', '446', '446 sd', '456', '456 sd', '546', '546 sd', '447', '447 sd', 'N2O', 'N2O sd', 'CO2', 'CO2 sd', '448', '448 sd') 
    combine$datetime <- as.POSIXlt(combine$datetime)
    
    #### Flux chamber Calculations ####
    ## Separate into each respect chamber
    
    chamberA <- subset(combine, combine$datetime$min == 10)
    chamberC <- subset(combine, combine$datetime$min == 25)
    chamberD <- subset(combine, combine$datetime$min == 40)
    
    ## Separate background measurements
    
    bkgd1 <- subset(combine, combine$datetime$min == 15)
    bkgd2 <- subset(combine, combine$datetime$min == 30)
    bkgd3 <- subset(combine, combine$datetime$min == 45)
    
    ## Match background measurements to thier respective chamber measurements
    
    matchA <- match(chamberA$datetime$hour, bkgd1$datetime$hour)
    matchC1 <- match(chamberC$datetime$hour, bkgd1$datetime$hour)
    matchC2 <- match(chamberC$datetime$hour, bkgd2$datetime$hour)
    matchD1 <- match(chamberD$datetime$hour, bkgd2$datetime$hour)
    matchD2 <- match(chamberD$datetime$hour, bkgd3$datetime$hour)
    
    
    #### Standard Isotope N2O Flux ####
    ## Take the difference in concentration between the blank and the chamber
    
    difA <- chamberA$'446' - bkgd1[matchA,2]
    difC <- chamberC$'446' - rowMeans(data.frame(bkgd1[matchC1,2], bkgd2[matchC2,2]), na.rm = TRUE)
    difD <- chamberD$'446' - rowMeans(data.frame(bkgd2[matchD1,2], bkgd3[matchD2,2]), na.rm = TRUE)
    
    difAsd <- sqrt(chamberA$'446 sd'^2 + bkgd1[matchA, 3]^2)
    difCsd <- sqrt(chamberC$'446 sd'^2 + bkgd2[matchC2, 3]^2)
    difDsd <- sqrt(chamberD$'446 sd'^2 + bkgd3[matchD2, 3]^2)
    
    ## Append the blank n2o table
    
    output <- data.frame(floor_date(bkgd1$datetime, unit = '1 hour'), difA, difAsd, difC, difCsd, difD, difDsd)
    colnames(output) <- c('datetime', 'A', 'A_sd', 'C', 'C_sd', 'D', 'D_sd')
    
    n2o <- rbind(n2o, output)
    print(tail(n2o))
    
    #### Isotopic del values (per mil) ####
    
    ## Hitran Isotopic Ratios (used in the program to find the levels of 456, 546, and 448)
    
    n2o446 <- .9903
    n2o456 <- .003641
    n2o456 <- .003641
    n2o448 <- .001986
    
    ## Calculating del values according to Wintel Manual relative to the HITRAN database
    
    
    
    d456bkgd1 <- (bkgd1$`456`/bkgd1$`446` - 1) * 1000
    d546bkgd1 <- (bkgd1$`546`/bkgd1$`446` - 1) * 1000
    d448bkgd1 <- (bkgd1$`448`/bkgd1$`446` - 1) * 1000
    
    d456bkgd2 <- (bkgd2$`456`/bkgd2$`446` - 1) * 1000
    d546bkgd2 <- (bkgd2$`546`/bkgd2$`446` - 1) * 1000
    d448bkgd2 <- (bkgd2$`448`/bkgd2$`446` - 1) * 1000
    
    d456bkgd3 <- (bkgd3$`456`/bkgd3$`446` - 1) * 1000
    d546bkgd3 <- (bkgd3$`546`/bkgd3$`446` - 1) * 1000
    d448bkgd3 <- (bkgd3$`448`/bkgd3$`446` - 1) * 1000
    
    d456A <- (chamberA$`456`/chamberA$`446` - 1) * 1000
    d546A <- (chamberA$`546`/chamberA$`446` - 1) * 1000
    d448A <- (chamberA$`448`/chamberA$`446` - 1) * 1000
    
    d456C <- (chamberC$`456`/chamberC$`446` - 1) * 1000
    d546C <- (chamberC$`546`/chamberC$`446` - 1) * 1000
    d448C <- (chamberC$`448`/chamberC$`446` - 1) * 1000
    
    d456D <- (chamberD$`456`/chamberD$`446` - 1) * 1000
    d546D <- (chamberD$`546`/chamberD$`446` - 1) * 1000
    d448D <- (chamberD$`448`/chamberD$`446` - 1) * 1000
    
    ## Deviations in del values
    
    d456bkgd1sd <- sqrt((bkgd1$'456 sd'/bkgd1$'456')^2 + (bkgd1$'446 sd'/bkgd1$'446')^2) * d456bkgd1 
    d546bkgd1sd <- sqrt((bkgd1$'546 sd'/bkgd1$'546')^2 + (bkgd1$'446 sd'/bkgd1$'446')^2) * d546bkgd1 
    d448bkgd1sd <- sqrt((bkgd1$'448 sd'/bkgd1$'448')^2 + (bkgd1$'446 sd'/bkgd1$'446')^2) * d448bkgd1 
    
    d456bkgd2sd <- sqrt((bkgd2$'456 sd'/bkgd2$'456')^2 + (bkgd2$'446 sd'/bkgd2$'446')^2) * d456bkgd1 
    d546bkgd2sd <- sqrt((bkgd2$'546 sd'/bkgd2$'546')^2 + (bkgd2$'446 sd'/bkgd2$'446')^2) * d546bkgd1 
    d448bkgd2sd <- sqrt((bkgd2$'448 sd'/bkgd2$'448')^2 + (bkgd2$'446 sd'/bkgd2$'446')^2) * d448bkgd1 
    
    d456bkgd3sd <- sqrt((bkgd3$'456 sd'/bkgd3$'456')^2 + (bkgd3$'446 sd'/bkgd3$'446')^2) * d456bkgd1 
    d546bkgd3sd <- sqrt((bkgd3$'546 sd'/bkgd3$'546')^2 + (bkgd3$'446 sd'/bkgd3$'446')^2) * d546bkgd1 
    d448bkgd3sd <- sqrt((bkgd3$'448 sd'/bkgd3$'448')^2 + (bkgd3$'446 sd'/bkgd3$'446')^2) * d448bkgd1 
    
    d456Asd <- sqrt((chamberA$'456 sd'/chamberA$'456')^2 + (chamberA$'446 sd'/chamberA$'446')^2) * d456A
    d546Asd <- sqrt((chamberA$'546 sd'/chamberA$'546')^2 + (chamberA$'446 sd'/chamberA$'446')^2) * d546A
    d448Asd <- sqrt((chamberA$'448 sd'/chamberA$'448')^2 + (chamberA$'446 sd'/chamberA$'446')^2) * d448A
    
    d456Csd <- sqrt((chamberC$'456 sd'/chamberC$'456')^2 + (chamberC$'446 sd'/chamberC$'446')^2) * d456C 
    d546Csd <- sqrt((chamberC$'546 sd'/chamberC$'546')^2 + (chamberC$'446 sd'/chamberC$'446')^2) * d546C
    d448Csd <- sqrt((chamberC$'448 sd'/chamberC$'448')^2 + (chamberC$'446 sd'/chamberC$'446')^2) * d448C
    
    d456Dsd <- sqrt((chamberD$'456 sd'/chamberD$'456')^2 + (chamberD$'446 sd'/chamberD$'446')^2) * d456D 
    d546Dsd <- sqrt((chamberD$'546 sd'/chamberD$'546')^2 + (chamberD$'446 sd'/chamberD$'446')^2) * d546D
    d448Dsd <- sqrt((chamberD$'448 sd'/chamberD$'448')^2 + (chamberD$'446 sd'/chamberD$'446')^2) * d448D
    
    
    ## Update table for each isotopologue
    
    #456
    
    output <- data.frame(floor_date(bkgd1$datetime, unit = '1 hour'), d456bkgd1, d456bkgd1sd, d456bkgd2, d456bkgd2sd,
                         d456bkgd3, d456bkgd3sd, d456A, d456Asd, d456C, d456Csd, d456D, d456Dsd)
    colnames(output) <- c('datetime', 'bkgd1ave', 'bkgd1sd', 'bkgd2ave', 'bkgd2sd', 'bkgd3ave', 'bkgd3sd', 'A', 'A_sd',
                          'C', 'C_sd', 'D', 'D_sd')
    n2oiso456 <- rbind(n2oiso456, output)
    
    print(tail(n2oiso456))
    
    #546
    
    output <- data.frame(floor_date(bkgd1$datetime, unit = '1 hour'), d546bkgd1, d546bkgd1sd, d546bkgd2, d546bkgd2sd,
                         d546bkgd3, d546bkgd3sd, d546A, d546Asd, d546C, d546Csd, d546D, d546Dsd)
    colnames(output) <- c('datetime', 'bkgd1ave', 'bkgd1sd', 'bkgd2ave', 'bkgd2sd', 'bkgd3ave', 'bkgd3sd', 'A', 'A_sd',
                          'C', 'C_sd', 'D', 'D_sd')
    n2oiso546 <- rbind(n2oiso546, output)
    
    print(tail(n2oiso546))
    
    #448
    
    output <- data.frame(floor_date(bkgd1$datetime, unit = '1 hour'), d448bkgd1, d448bkgd1sd, d448bkgd2, d448bkgd2sd,
                         d448bkgd3, d448bkgd3sd, d448A, d448Asd, d448C, d448Csd, d448D, d448Dsd)
    colnames(output) <- c('datetime', 'bkgd1ave', 'bkgd1sd', 'bkgd2ave', 'bkgd2sd', 'bkgd3ave', 'bkgd3sd', 'A', 'A_sd',
                          'C', 'C_sd', 'D', 'D_sd')
    n2oiso448 <- rbind(n2oiso448, output)
    
    print(tail(n2oiso448))
    
  }

  ## Times Files ##
  
  starttimefile <- 'F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Trapped - Start and End Times.txt'
  
  starttimetable <- read.csv(starttimefile, stringsAsFactors = FALSE)
  
  for (i in 1:nrow(starttimetable)) {
    
    startdate <- starttimetable$start.date[i]
    starthour <- starttimetable$starttime[i]
    
    starttime <- as.POSIXct(paste0(startdate,' ',starthour, ':00:00'), format = '%m/%d/%Y %H:%M:%S', tz = 'UTC')
    
    enddate <- starttimetable$enddate[i]
    endhour <- starttimetable$endtime[i]
    
    endtime <- as.POSIXct(paste0(enddate,' ',endhour, ':00:00'), format = '%m/%d/%Y %H:%M:%S', tz = 'UTC')
    
    startmatch <- match(starttime, n2o$datetime)
    endmatch <- match(endtime, n2o$datetime)
    
    data <- n2o[startmatch:endmatch,]
    
    datetime <- data$datetime
    
    filedate <- gsub('/', '_', startdate)
      
    # #### Mass Info ####
    # ## Upload the mass for determination of flux
    # 
    # massfile <- 'F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Mass of Dried Soil.txt'
    # masstable <- read.csv(massfile, stringsAsFactors = FALSE, header = TRUE)
    # 
    # ## Contents of each chamber ##
    # 
    # chamberfile <- read.csv(file = "F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Chamber Log.txt", header = TRUE, stringsAsFactors = FALSE)
    # chamberfile$Date <- as.POSIXct(chamberfile$Date, format = '%m/%d/%Y')
    # 
    # chamberdate <- match(as.Date(datetime[1]), as.Date(chamberfile$Date))
    # 
    # contentsb <- chamberfile[chamberdate, 2]
    # contentsc <- chamberfile[chamberdate, 4]
    # contentsd <- chamberfile[chamberdate, 6]
    # contentse <- chamberfile[chamberdate, 8]
    # 
    # plotb <- chamberfile[chamberdate, 3]
    # plotc <- chamberfile[chamberdate, 5]
    # plotd <- chamberfile[chamberdate, 7]
    # plote <- chamberfile[chamberdate, 9]
    # 
    # ## Match the date of the current loop iteration with the date in the mass file
    # 
    # massmatch <- match(startdate, masstable$Date)
    # 
    # massb <- masstable$chamberBMass[i]
    # massc <- masstable$chamberCMass[i]
    # massd <- masstable$chamberDMass[i]
    # masse <- masstable$chamberEMass[i]
    # 
    # #### Flow Info ####
    # ## Upload the flow for determination of flux
    # 
    # flowfile <- 'F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/Flow Rates.txt'
    # flowtable <- read.csv(flowfile, stringsAsFactors = FALSE, header = TRUE)
    # 
    # ## Match the date of the current loop iteration with the date in the flow file
    # 
    # flowmatch <- match(startdate, flowtable$Date)
    # 
    # flowb <- flowtable$FlowRateB[i]
    # flowc <- flowtable$FlowRateC[i]
    # flowd <- flowtable$FlowRateD[i]
    # flowe <- flowtable$FlowRateE[i]
    # 
    # #### Flux calculation ####
    # # Flux in ng N (g soil hr)-1 = dC(ppb) * 1 ppb N2O/10^9 ppb air * 1 mol air/ 24.1 L air * flow (L / min) * 60 min/1 hr * 28 g N/ mol N2O * 10^9 ng N/g N / mass soil (g)
    # 
    # fluxb <- data$B * 1/10^9 * 1/24.1 * flowb * 60/1 * 28/1 * 10^9/1 / massb
    # fluxc <- data$C * 1/10^9 * 1/24.1 * flowc * 60/1 * 28/1 * 10^9/1 / massc
    # fluxd <- data$D * 1/10^9 * 1/24.1 * flowd * 60/1 * 28/1 * 10^9/1 / massd
    # fluxe <- data$E * 1/10^9 * 1/24.1 * flowe * 60/1 * 28/1 * 10^9/1 / masse
    # 
    # fluxbsd <- data$B_sd * 1/10^9 * 1/24.1 * flowb * 60/1 * 28/1 * 10^9/1 / massb
    # fluxcsd <- data$C_sd * 1/10^9 * 1/24.1 * flowc * 60/1 * 28/1 * 10^9/1 / massc
    # fluxdsd <- data$D_sd * 1/10^9 * 1/24.1 * flowd * 60/1 * 28/1 * 10^9/1 / massd
    # fluxesd <- data$E_sd * 1/10^9 * 1/24.1 * flowe * 60/1 * 28/1 * 10^9/1 / masse
    # 
    # 
    # 
    # ## Flux output information
    # 
    # date <- as.Date(data$datetime[1])
    # output <- data.frame(datetime = data$datetime, hour = seq(0,length(data$datetime)-1,1), 
    #                      Flux_B = fluxb, Flux_B_sd = fluxbsd,
    #                      Flux_C = fluxc, Flux_C_sd = fluxcsd,
    #                      Flux_D = fluxd, Flux_D_sd = fluxdsd,
    #                      Flux_E = fluxe, Flux_E_sd = fluxesd)
    # head(output)
    # filename <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Fluxes/',date,'.csv')
    # 
    # write.csv(output, file = filename, row.names = FALSE)
    # 
    # ## Plot the flux
    # 
    # fluxbforplot <- data.frame(fluxb, fluxbsd, seq(0,length(fluxb)-1, 1), paste0(contentsb,':',plotb))
    # fluxcforplot <- data.frame(fluxb, fluxcsd, seq(0,length(fluxc)-1, 1), paste0(contentsc,':',plotc))
    # fluxdforplot <- data.frame(fluxd, fluxdsd, seq(0,length(fluxd)-1, 1), paste0(contentsd,':',plotd))
    # fluxeforplot <- data.frame(fluxe, fluxesd, seq(0,length(fluxe)-1, 1), paste0(contentse,':',plote))
    # 
    # names <- c('n2o_flux', 'n2o_flux_sd', 'hour', 'chamber')
    # 
    # colnames(fluxbforplot) <- names
    # colnames(fluxcforplot) <- names
    # colnames(fluxdforplot) <- names
    # colnames(fluxeforplot) <- names
    # 
    # flux <- rbind(fluxbforplot, fluxcforplot, fluxdforplot, fluxeforplot)
    # 
    # filedate <- gsub('/', '_', startdate)
    # 
    # n2oplot <- ggplot(data = flux, aes(x = hour, y = n2o_flux, color = chamber)) + geom_line() + 
    #   labs(title = paste('N2O flux for', format(as.Date(datetime[1]), "%m/%d/%Y")),
    #        x = 'Time Since Start (hr.)',
    #        y = expression(paste('N'[2],'O flux (ug N kg' ^-1,'dray soil hr' ^-1, ')')),
    #        color = "Treatment - Plot")
    # 
    # n2oplot <- n2oplot + theme_bw() + theme(plot.title = element_text(size = 32, hjust = 0.5))
    # 
    # png(paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/N2O Flux Plots/',filedate,'.png'))
    # 
    # print(n2oplot)
    # 
    # dev.off()
    
    ## Isotope Output ##
    
    #456
    
    startmatch <- match(starttime, n2oiso456$datetime)
    endmatch <- match(endtime, n2oiso456$datetime)
    
    data <- n2oiso456[startmatch:endmatch,]
    
    filename <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Trapped/456/', filedate,'.csv')
    write.csv(data, file = filename, row.names = FALSE)
    
    #546 
    
    startmatch <- match(starttime, n2oiso546$datetime)
    endmatch <- match(endtime, n2oiso546$datetime)
    
    data <- n2oiso546[startmatch:endmatch,]
    
    filename <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Trapped/546/', filedate,'.csv')
    write.csv(data, file = filename, row.names = FALSE)
    
    #448
    
    startmatch <- match(starttime, n2oiso448$datetime)
    endmatch <- match(endtime, n2oiso448$datetime)
    
    data <- n2oiso448[startmatch:endmatch,]
    
    filename <- paste0('F:/Surface 3/Main Folder/Documents/Lab Data/2019_11 Soil Incubation Study/N2O/Trapped/448/', filedate,'.csv')
    write.csv(data, file = filename, row.names = FALSE)
    
  }
  
}
