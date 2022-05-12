file <- file.choose()
compile <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
colnames(compile) <- c("datetime", "ave","sd")

compile$datetime <- as.POSIXlt(compile$datetime, format = "%m/%d/%Y %H:%M", tz = "EST")

datetime <- compile$datetime
compile$hour <- datetime$hour

start <- compile[0 == compile$hour,]
start$na <- is.na(start$ave)
start <- start[start$na == FALSE,]
start$na <- is.na(start$sd)
start <- start[start$na == FALSE,]

ave <- mean(c(start$ave))

ES <- na.omit(start$sd) ^ 2 * (95)
GS <- (na.omit(start$ave) - (ave[1])) ^ 2 * 96
sd_ <- sqrt((sum(ES)+sum(GS))/(96 * length(na.omit(start$ave)) - 1))

## Number of observations (for ozone we have 96 measurements for ambient and 48 measurements for chambers)
observations <- nrow(start) * 96
  
export <- data.frame(start$hour[1], ave, sd_, observations)
colnames(export) <- c('hour', 'average', 'sd', 'observations')



for(i in 1:23)	{
  start <- compile[i == compile$hour,]
  start$na <- is.na(start$ave)
  start <- start[start$na == FALSE,]
  start$na <- is.na(start$sd)
  start <- start[start$na == FALSE,]
  
  ave <- mean(c(start$ave))

  ES <- na.omit(start$sd) ^ 2 * (95)
  GS <- (na.omit(start$ave) - (ave[1])) ^ 2 * 96
  sd_ <- sqrt((sum(ES)+sum(GS))/(96 * length(na.omit(start$ave)) - 1))
  
## Number of observations   
  observations <- nrow(start) * 96

  add <- data.frame(start$hour[1], ave, sd_, observations)
  colnames(add) <- c('hour', 'average', 'sd', 'observations')
  
  export <- rbind(export, add)
}

sum(start$variance)

export$CI95 <- 1.96 * export$sd/sqrt(export$observations)
export$SE <- export$sd/sqrt(export$observation)

write.table(export, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)
