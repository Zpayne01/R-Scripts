## This program is run after the Indoor Chamber Measurement program to seperate the data for easier visualization

file <- 'C:/Users/Zachary/Desktop/NOx.csv'
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

## Seperate the columns based on the chamber

chamberb <- csv[1:7]
chamberc <- csv[8:14]
chamberd <- csv[15:21] 
chambere <- csv[22:28]
chambera <- csv[29:35]

## Rename the columns correctly

colnames(chamberb) <- c('timeb', 'nob', 'nobsd', 'no2b', 'no2bsd', 'honob', 'honobsd')
colnames(chamberc) <- c('timec', 'noc', 'nocsd', 'no2c', 'no2csd', 'honoc', 'honocsd')
colnames(chamberd) <- c('timed', 'nod', 'nodsd', 'no2d', 'no2dsd', 'honod', 'honodsd')
colnames(chambere) <- c('timee', 'noe', 'noesd', 'no2e', 'no2esd', 'honoe', 'honoesd')
colnames(chambera) <- c('timea', 'noa', 'noasd', 'no2a', 'no2asd', 'honoa', 'honoasd')

## Change the class of the time to as.POSIXlt (string as time)

chambera$timea <- as.POSIXlt(chambera$timea, format = '%Y-%m-%d %H:%M:%S')
chamberb$timeb <- as.POSIXlt(chamberb$timeb, format = '%Y-%m-%d %H:%M:%S')
chamberc$timec <- as.POSIXlt(chamberc$timec, format = '%Y-%m-%d %H:%M:%S')
chamberd$timed <- as.POSIXlt(chamberd$timed, format = '%Y-%m-%d %H:%M:%S')
chambere$timee <- as.POSIXlt(chambere$timee, format = '%Y-%m-%d %H:%M:%S')

## Seperate first measurement and second measurement

chamberbeven <- chamberb[seq(2,length(chamberb$timeb),2),]
chamberbodd <- chamberb[seq(1,length(chamberb$timeb),2),]

chamberceven <- chamberc[seq(2,length(chamberc$timec),2),]
chambercodd <- chamberc[seq(1,length(chamberc$timec),2),]

chamberdeven <- chamberd[seq(2,length(chamberd$timed),2),]
chamberdodd <- chamberd[seq(1,length(chamberd$timed),2),]

chambereeven <- chambere[seq(2,length(chambere$timee),2),]
chambereodd <- chambere[seq(1,length(chambere$timee),2),]

## Combine even and odds

chambereven <- cbind(chamberbeven, chamberceven, chamberdeven, chambereeven)
chamberodd <- cbind(chamberbodd, chambercodd, chamberdodd, chambereodd)

chambereven <- na.omit(chambereven)
chamberodd <- na.omit(chamberodd)

## Subset chammber a measurements by minutes of the hour

datetime <- chambera$timea
chambera$min <- datetime$min

chambera0 <- subset(chambera, chambera$min == 0)
chambera15 <- subset(chambera, chambera$min == 15)
chambera30 <- subset(chambera, chambera$min == 30)
chambera45 <- subset(chambera, chambera$min == 45)

## Recombine chambera for export

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

chamberacomb <-cbind(chambera0, chambera15, chambera30, chambera45)

## Get date for export

date <- c(as.character(chambera$time[1]))
date <- substring(date, 1, 10)

## Export the data 

write.csv(chambereven, file = paste0('C:/Users/Zachary/Desktop/Working Files/chambereven_', date,'.csv'), row.names = FALSE)
write.csv(chamberodd, file = paste0('C:/Users/Zachary/Desktop/Working Files/chamberodd_', date,'.csv'), row.names = FALSE)
write.csv(chamberacomb, file = paste0('C:/Users/Zachary/Desktop/Working Files/chamberacomb_', date,'.csv'), row.names = FALSE)
