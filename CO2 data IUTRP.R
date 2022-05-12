#### CO2 and H20 Measurements ####

## Open CO2 tab delemited file ##

file <- file.choose()
CO2 <- read.delim(file, sep = "", header = FALSE)
CO2 <- CO2[-1,]
CO2 <- CO2[-1,]
CO2[5:9] <- list(NULL)

colnames(CO2) <- c('date','time','CO2_ppm','H2O_ppt')

## Get time ##
datetime <- paste(CO2$date, CO2$time)
datetime <- strptime((datetime), format='%Y-%m-%d %H:%M:%S', tz = 'EST')
CO2$min <- datetime$min
CO2$day <- datetime$mday
CO2$hour <- datetime$hour
datetime <- format(datetime, "%m/%d/%Y %H:%M:%S")
CO2$datetime <- datetime
CO2$date <- NULL
CO2$time <- NULL

## Seperate Data by Subsets ##

amba <- subset(CO2, subset = min > 0 & min < 5)
ambb <- subset(CO2, subset = min > 15 & min < 20)
ambc <- subset(CO2, subset = min > 30 & min < 35)
ambd <- subset(CO2, subset = min > 45 & min < 50)

chambera <- subset(CO2, subset = min > 5 & min < 15)
chamberb <- subset(CO2, subset = min > 20 & min < 30)
chamberc <- subset(CO2, subset = min > 35 & min < 45)
chamberd <- subset(CO2, subset = min > 50 & min < 60)

## Combined Ambient Data ##

amb <- rbind(amba, ambb, ambc, ambd)
amb <- data.frame(amb$CO2_ppm, amb$H2O_ppt, amb$datetime)

## Shorten Chamber Data Frames ##

chambera <- data.frame(chambera$CO2_ppm, chambera$H2O_ppt, chambera$datetime)
chamberb <- data.frame(chamberb$CO2_ppm, chamberb$H2O_ppt, chamberb$datetime)
chamberc <- data.frame(chamberc$CO2_ppm, chamberc$H2O_ppt, chamberc$datetime)
chamberd <- data.frame(chamberd$CO2_ppm, chamberd$H2O_ppt, chamberd$datetime)

## Combined Chamber Data ##

library(plyr)

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

chambers <- cbind.fill(chambera, chamberb, chamberc, chamberd)
colnames(chambers) <- c('CO2_ppm_a','H2O_ppt_a','datetime_a','CO2_ppm_b','H2O_ppt_b','datetime_b',
				'CO2_ppm_c','H2O_ppt_c','datetime_c','CO2_ppm_d','H2O_ppt_d','datetime_d')

## Write Files ##

filename <- "C:/Users/Zachary/Desktop/CO2_amb.csv"
filename2 <- "C:/Users/Zachary/Desktop/CO2_chambers.csv"
write.table(amb, file = filename, append = FALSE, sep = ",", row.names = FALSE)
write.table(chambers, file = filename2, append = FALSE, sep = ",", row.names = FALSE)
