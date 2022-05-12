## Dirunal Averaging for CO2

file <- file.choose()
compile <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

## Column Names

colnames(compile) <- c('datetime', 'ave_CO2', 'sd_CO2', 'ave_H2O', 'sd_H2O')

## Makes Sure the time works correctly

compile$datetime <- strptime(compile$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "EST")

datetime <- compile$datetime
compile$hour <- datetime$hour

## Create hours column

start <- compile[0 == compile$hour,]

## Remove nonvalues for CO2

start$na <- is.na(start$ave_CO2)
start <- start[start$na == FALSE,]
start$na <- is.na(start$sd_CO2)
start <- start[start$na == FALSE,]

## Remove nonvalues for H2O

start$na <- is.na(start$ave_H2O)
start <- start[start$na == FALSE,]
start$na <- is.na(start$sd_H2O)
start <- start[start$na == FALSE,]

ave_CO2 <- mean(c(start$ave_CO2))

ES <- na.omit(start$sd_CO2) ^ 2 * (44)
GS <- (na.omit(start$ave_CO2) - (ave_CO2[1])) ^ 2 * 45
sd_CO2 <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$ave_CO2)) - 1))

ave_H2O <- mean(c(start$ave_H2O))

ES <- na.omit(start$sd_H2O) ^ 2 * (44)
GS <- (na.omit(start$ave_H2O) - (ave_H2O[1])) ^ 2 * 45
sd_H2O <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$ave_H2O)) - 1))

## Nummber of observations for determine confidence

observations <- nrow(start) * 4 * 60 * 2 + 60

export <- data.frame(start$hour[1], ave_CO2, sd_CO2, ave_H2O, sd_H2O, observations)
colnames(export) <- c('hour', 'ave_CO2', 'sd_CO2', 'ave_H2O', 'sd_H2O', 'observations')

## Loop for determining 

for(i in 1:23)	{
  start <- compile[i == compile$hour,]
  start$na <- is.na(start$ave_CO2)
  start <- start[start$na == FALSE,]
  start$na <- is.na(start$sd_CO2)
  start <- start[start$na == FALSE,]
  
  start$na <- is.na(start$ave_H2O)
  start <- start[start$na == FALSE,]
  start$na <- is.na(start$sd_H2O)
  start <- start[start$na == FALSE,]
  
  ave_CO2 <- mean(c(start$ave_CO2))
  
  ES <- na.omit(start$sd_CO2) ^ 2 * (44)
  GS <- (na.omit(start$ave_CO2) - (ave_CO2[1])) ^ 2 * 45
  sd_CO2 <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$ave_CO2)) - 1))
  
  ave_H2O <- mean(c(start$ave_H2O))
  
  ES <- na.omit(start$sd_H2O) ^ 2 * (44)
  GS <- (na.omit(start$ave_H2O) - (ave_H2O[1])) ^ 2 * 45
  sd_H2O <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$ave_H2O)) - 1))
  
  observations <- nrow(start) * 4 * 60 * 2 + 60

  add <- data.frame(start$hour[1], ave_CO2, sd_CO2, ave_H2O, sd_H2O, observations)
  colnames(add) <- c('hour', 'ave_CO2', 'sd_CO2', 'ave_H2O', 'sd_H2O', 'observations')
  
  export <- rbind(export, add)
}

## Export to the Clipboard for creating a collected file

write.table(export, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)
