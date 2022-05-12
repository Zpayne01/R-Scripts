## Install Packages

library(lubridate)
library(plyr)

## File list ##

dirf <- 'D:/Surface 3/Main Folder/Documents/Lab Data/2020 Growth Chamber Experiments/N2O/Raw/Nov 11 - Nov 27'
f <- list.files(dirf, pattern = '.str')

fulldata <- data.frame(
  datetime = c(),
  '446' = c(),
  '456' = c(),
  '546' = c(),
  '447' = c(),
  'N2O' = c(),
  'CO2' = c(),
  '448' = c()
)

for (i in 1:length(f)) {
  file <- paste(dirf, f[i], sep = '/')
  
  data <- read.table(file, sep = ' ', skip = 1, header = FALSE)
  data <- data[,1:8]
  
  colnames(data) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')
  data$datetime <- as.POSIXct(data$datetime+(3600*4), origin = "1904-01-01 00:00:00", tz = Sys.timezone())
  
  fulldata <- rbind(fulldata, data)
}

## Start time file

starttimesfile <- read.csv('D:/Surface 3/Main Folder/Documents/Lab Data/2020 Growth Chamber Experiments/start and end times.txt')

for (i in 1:nrow(starttimesfile)) {
  
  start <- as.POSIXct(paste(starttimesfile$StartDate[i], starttimesfile$StartTime[i]), format = '%m-%d-%Y %H:%M', tz = Sys.timezone())
  end <- as.POSIXct(paste(starttimesfile$EndDate[i], starttimesfile$Endtime[i]), format = '%m-%d-%Y %H:%M', tz = Sys.timezone())
  
  if (is.na(match(as.integer(start), as.integer(fulldata$datetime)))) {
    next
  } 
 
  startmatch <- match(as.integer(start), as.integer(fulldata$datetime))
  endmatch <- match(as.integer(end), as.integer(fulldata$datetime))
  
  ## Data for run
  
  data <- fulldata[startmatch:endmatch,]
  row.names(data) <- NULL
  
  ## Plot full data ##
  
  filename <- paste0("C:/Users/zacpayne/Desktop/", starttimesfile$Experiment[i], " Raw N2O.jpeg")
  
  jpeg(filename)
  
  plot(data$datetime,
       data$`446`,
       type = 'l',
       xlab = 'Time (EST)',
       ylab = '[N2O] (ppb)')
  
  dev.off()
 
  ## Despike ##
  ## To despike data we want to find points with large decreases followed by large increases ##
  
  
  despike1 <- c(0, diff(data$`446`))
  despike2 <- c(diff(-data$`446`), 0) 
  sign1 <- sign(despike1)
  sign2 <- sign(despike2)
  
  despikedf <- data.frame(sign1, sign2)
  despike2neg <- which(despikedf$sign1 < 0 & despikedf$sign2 < 0)
  despikedf <- data.frame(despike1, despike2)
  
  despikemean <- rowMeans(despikedf)
  
  w <- which(despikemean <= -0.2)
  w2 <- w[w %in% despike2neg]
  
  despikedata <- data[-w2,]
  
  filename <- paste0("C:/Users/zacpayne/Desktop/", starttimesfile$Experiment[i], "Despiked N2O.jpeg")
  
  ## Plot the despiked
  
 jpeg(filename)
  
  plot(despikedata$datetime,
       despikedata$`446`,
       type = 'l',
       xlab = 'Time (EST)',
       ylab = '[N2O] (ppb)')
  
  dev.off()
  
  ## Despike 2
  ## Second iteration of despiking ##
  
  
  despike1 <- c(0, diff(despikedata$`446`))
  despike2 <- c(diff(-despikedata$`446`), 0) 
  sign1 <- sign(despike1)
  sign2 <- sign(despike2)
  
  despikedf <- data.frame(sign1, sign2)
  despike2neg <- which(despikedf$sign1 < 0 & despikedf$sign2 < 0)
  despikedf <- data.frame(despike1, despike2)
  
  despikemean <- rowMeans(despikedf)
  
  w <- which(despikemean <= -0.2)
  w2 <- w[w %in% despike2neg]
  
  despikedata2 <- despikedata[-w2,]
  
  filename <- paste0("C:/Users/zacpayne/Desktop/", starttimesfile$Experiment[i], "Despiked 2 N2O.jpeg")

  jpeg(filename)
  
  plot(despikedata2$datetime,
       despikedata2$`446`,
       type = 'l',
       xlab = 'Time (EST)',
       ylab = '[N2O] (ppb)')
  

  
  ## Seperate measurements ##
  
  ## Make a new data file 
  
  data <- despikedata2
  
  ## Convert to time list for minute extraction
  
  data$datetime <- as.POSIXlt(data$datetime)
  
  ## Seperate background measurements
  
  background <- subset(data, data$datetime$min %in% c(2:4, 17:19, 32:34, 47:49))
  
  ## Find the start point of background measurements ##
  
  background$datetime <- as.POSIXct(background$datetime)
  startpoint <- as.integer(background$datetime[1])
  
  ## Subract to make start of background time point 0
  
  background$datetime <- as.integer(background$datetime) - startpoint
  
  ## Plot the background
  
  filename <- paste0("C:/Users/zacpayne/Desktop/", starttimesfile$Experiment[i], "background N2O.jpeg")
  
  jpeg(filename)
  
  plot(as.integer(background$datetime),
       background$`446`,
       type = 'l',
       xlab = 'Time (EST)',
       ylab = '[N2O] (ppb)')
 
  lines(smooth.spline(background$datetime, background$`446`), col = 'red')
  
  dev.off()
  
  ## Find the fitting parameters for smooth.spline ##
  
  lo <- smooth.spline(background$datetime, background$`446`)
  predict(lo)
  
  ## Plot the subtracted smoothspline fit
  
  filename <- paste0("C:/Users/zacpayne/Desktop/", starttimesfile$Experiment[i], "background smooth subtracted N2O.jpeg")
  
  jpeg(filename)
  
  plot(as.integer(background$datetime),
       background$`446` - predict(lo)$y,
       type = 'l',
       xlab = 'Time (EST)',
       ylab = '[N2O] (ppb)')
  
  dev.off()
  
  ## Convert data datetime back to POSIXct for integer extraction
  
  data$datetime <- as.POSIXct(data$datetime)
  
  ## Create a new DF for experimenting with
  
  dataexp <- data
  
  ## Convert POSIXct to intger and subtract to 0 seconds
  
  dataexp$datetime <- as.integer(data$datetime)
  start <- dataexp$datetime[1]
  dataexp$datetime <- dataexp$datetime - start
  
  pred <- predict(lo, dataexp$datetime)
  
  filename <- paste0("C:/Users/zacpayne/Desktop/", starttimesfile$Experiment[i], "subtracted N2O.jpeg")
  
  jpeg(filename)
  
  plot(dataexp$datetime,
       dataexp$`446` - pred$y,
       type = 'l',
       xlab = 'Time (EST)',
       ylab = 'delta[N2O] (ppb)')
  
  dev.off()
  
  ## Flux Calculation ##
  
  Area = 0.067 #m2
  FlowA = 2 #LPM
  FlowB = 2 #LPM
  FlowC = 2 #LPM
  FlowD = 2 #LPM

  databkgdsub <- data.frame(
    datetime = data$datetime,
    '446' = dataexp$`446` - pred$y
  )
  
  databkgdsub$datetime <- as.POSIXlt(databkgdsub$datetime)
  
  DataA <- subset(databkgdsub, databkgdsub$date$min %in% c(10:14))
  DataB <- subset(databkgdsub, databkgdsub$date$min %in% c(25:29))
  DataC <- subset(databkgdsub, databkgdsub$date$min %in% c(40:44))
  DataD <- subset(databkgdsub, databkgdsub$date$min %in% c(55:59))

  flux <- function(Data, Time, Area, Flow){
    #Data in PPB
    #Time as a POSIXlt
    #Area in m2
    #Flow in LPM
    
    Time2 <- as.POSIXct(Time)
    Data2 <- 4.02E-11 * Data * 28 * 1E9
    
    fluxout <- Data2 * Flow / Area / 60 
    df <- data.frame(Time2, fluxout)
    return(df)
  }
  
  FluxA <- flux(DataA$X446, DataA$datetime, Area, FlowA)
  FluxB <- flux(DataB$X446, DataB$datetime, Area, FlowB)
  FluxC <- flux(DataC$X446, DataC$datetime, Area, FlowC)
  FluxD <- flux(DataD$X446, DataD$datetime, Area, FlowD)
  
  filename <- paste0("C:/Users/zacpayne/Desktop/", starttimesfile$Experiment[i], "Flux out.jpeg")
  
  jpeg(filename)
  
  xlim <- range(c(FluxA$Time2, FluxB$Time2, FluxC$Time2, FluxD$Time2))
  ylim <- range(c(FluxA$fluxout, FluxB$fluxout, FluxC$fluxout, FluxD$fluxout))
  plot(FluxA$fluxout ~ FluxA$Time2, type = 'l', xlim = xlim, ylim = ylim, ylab = bquote(paste('N '[2],'O Flux (ng N m'^ -2,' s'^ -1,')')), xlab = 'Date (EST)', mgp = c(2,1,0))
  lines(FluxB$fluxout ~ FluxB$Time2, col = 'green')
  lines(FluxC$fluxout ~ FluxC$Time2, col = 'red')
  lines(FluxD$fluxout ~ FluxD$Time2, col = 'blue')
  
  dev.off()
}


start <- as.POSIXct('2020-11-21 05:24:00')
end <- as.POSIXct('2020-11-21 5:25:00')

startmatch <- match(as.integer(start), as.integer(data$datetime))
startmatch

endmatch <-match(as.integer(end), as.integer(data$datetime))
endmatch

plot(despikedata$datetime[startmatch:endmatch], 
     despikedata$`446`[startmatch:endmatch], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[N2O] (ppb)')

despikedata[startmatch:endmatch,]
