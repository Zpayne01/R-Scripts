file1 <- file.choose()
csvmet <- read.csv(file1, stringsAsFactors = FALSE)

file2 <- file.choose()
csvvoc <- read.csv(file2, stringsAsFactors = FALSE)

head(csvmet)
head(csvvoc)

csvmet$datetime <- as.POSIXct(csvmet$datetime, format = "%m/%d/%Y %H:%M")
csvvoc$date.middle.sampling <- as.POSIXct(csvvoc$middle.sampling.time, format = "%m/%d/%Y %H:%M")

matchvec <- c()

for (i in 1:length(csvvoc$date.middle.sampling)) {
  
add <- which.min(abs(csvvoc$date.middle.sampling[i] - csvmet$datetime))
matchvec <- c(matchvec, add)
  
}

csvvoc$airtemp <- csvmet$Tair..deg..C.[matchvec]
csvvoc$PAR <- csvmet$Rn..W.m2.[matchvec]
csvvoc$RH <- csvmet$RH....[matchvec]
csvvoc$windspeed <- csvmet$WS..m.s.[matchvec]
csvvoc$winddirection <- csvmet$WD..degrees.[matchvec]

plotsum <- function (x,y) {
  dftemp <- data.frame(x,y)
  xname <- colnames(dftemp[1])
  yname <- colnames(dftemp[2])
  plot(dftemp[,1]~dftemp[,2], type = 'p', xlab = xname, ylab = yname)
  abline(lm(dftemp[,1]~dftemp[,2]), col="red")
  dat <- summary(lm(dftemp[,1]~dftemp[,2]))
  sumdat <- data.frame(xname, yname, dat$r.squared,dat$coefficients[1,1], dat$coefficients[2,1])
  colnames(sumdat) <- c('x','y','rsquared','intercept','slope')
  return(sumdat)
}

addsumdat <- data.frame(x=character(),y=character(),rquared=numeric(),intercept=numeric(),slope=numeric())

for (i in 2:40) {
    x <- csvvoc[i]
  for (j in 2:45) {
    y <- csvvoc[j]
    if (i != j) {
    sumdat <- plotsum(x,y)
    addsumdat <- rbind(addsumdat, sumdat)
    } else {
    }
  }
}

filename <-  filename <- paste0('C:/Users/zacpayne/Desktop/Working Files/6-14 C VOC relationships.csv')
write.csv(addsumdat, file = filename, row.names = FALSE)


fit <- lm(isoprène~airtemp, data = csvvoc)
dftemp <- data.frame(csvvoc[4], csvvoc[7])
dftemp

plot()

plot(dftemp[,1]~dftemp[,2], type = 'p', xlim = c(0,2500), ylim = c(0,2500), xlab = colnames(dftemp)[1])
abline(lm(dftemp[,1]~dftemp[,2]), col="red")

x <- seq(1,5,1)
y <- c(1,2,4,7,10)

plot(x~y, type='p')
abline(lm(x~y),col='red')
summary(lm(x~y))
