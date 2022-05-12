library(lubridate)
library(dplyr)

  ## NO Concentration only ####

file <- file.choose() # Select NOx Werx File\
csv <- read.csv(file, stringsAsFactors = FALSE, header = TRUE) # Convert the csv into a table

colnames(csv) # Read out column names of csv

csv <- data.frame(Time = csv$TheTime, Counts = csv$CH1_Hz) # Remove superfluous columns

head(csv) # Show the head of csv

## Time Conversion ####

csv$Time <- as.POSIXlt(csv$Time*(60*60*24), origin = "1899-12-30", tz = 'UTC') # Convert to POSIXlt to access minutes 

## Subset background ####

background <- subset(csv, csv$Time$min %in% c(0,5,10,15,20,25,30,35,40,45,50,55)) #Subset the background based on time
background <- subset(background, background$Time$sec > 15) #Take only the last 45 seconds of each measurement

backgroundave <- data.frame(Time = floor_date(background$Time, unit = 'minute'), Counts = background$Counts) #Floor the background measurement for averaging

backgroundave <- backgroundave %>%    
                 group_by(Time) %>%
                 summarise(avg = mean(Counts))  #Average the background measurement

## Subset NO measurements ####

NOmeas <- subset(csv, !(csv$Time$min %in% c(0,5,10,15,20,25,30,35,40,45,50,55))) #Subset the NO measurement based on time
NOmeas <- subset(NOmeas, (NOmeas$Time$min %in% c(1,6,11,16,21,26,31,36,41,46,51,56) & NOmeas$Time$sec > 15) | !(NOmeas$Time$min %in% c(1,6,11,16,21,26,31,36,41,46,51,56)))

## Subtract the background from the NO measurement ####

NOmeas$floortime <- floor_date(NOmeas$Time, unit = '5 minutes') #Floor the time for matching
NOmatchback <- match(as.POSIXct(NOmeas$floortime), backgroundave$Time) #Matchbackground to appropriate time

NOmeas$NOsubback <- NOmeas$Counts - backgroundave$avg[NOmatchback] #Subtract background
NOmeas <- NOmeas[!is.na(NOmeas$NOsubback),] #Remove NAs

## Calculate the concentration of NO ####

NOmeas$NOconc <- (NOmeas$NOsubback-34)*(584)^-1 #NO concentration calculation from measurement at 03-04-20

## CO2 Concentration ####

co2file <- file.choose() #CO2 file

co2 <- read.table(co2file, skip = 1, header = TRUE) #Skip first row
co2$datetime <- paste(co2$Date.Y.M.D., co2$Time.H.M.S.) #Combine date and time

co2 <- data.frame(Time = as.POSIXct(co2$datetime, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC'), 
                 h2o = co2$H2O.ppt.) #CO2 dataframe, delete extraneous columns

head(co2)

co2$h2oah <- co2$h2o/1000/24.05*1000*18 #convert ppth to AH (g/m3)

## Match LICOR to NOxWerx ####

co2matchNO <- match(as.character(NOmeas$Time), as.character(co2$Time)) #match NO measurements with LICOR measurements
NOmeas$H2o <- co2$h2oah[co2matchNO] #Apply the match

NOmeas <- NOmeas[!is.na(NOmeas$H2o),] #Remove all points with missing water data

## NO absolute humidity correction ####

NOmeas$NOcorr <- NOmeas$NOconc/(-9.3E-3*NOmeas$H2o + 1) #Corrected NO measurement

## Write to CSV ####

filename <- 'C:/Users/zacpayne/Desktop/03_05_2020 RH Nafion Experiment.csv'
write.csv(NOmeas, filename, row.names = FALSE)
write.csv()

