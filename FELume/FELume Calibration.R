## Inputs

inputdelay <- 3.5 #seconds
dir <- 'C:/Users/zacpayne/Desktop/' #Directory to save plots and table (use forwards slash)
endpoint_x <- 1.02 #Change this value if the program isn't finding endpoints correctly

## FElume Processing

file <- file.choose() #Raw Data
file2 <- file.choose() ##Concentration File

data <- read.csv(file, header = FALSE, stringsAsFactors = TRUE) #Convert file into R
data <- data[1:nrow(data),] 

data2 <- read.csv(file2, header = FALSE, stringsAsFactors = FALSE) #Convert 3nd file into R

## Find the Time Flag (Evan set this as 0 in the FElume data log)

timeflagstart <- which(data == 0)[1]

## Delete up to time flag 1 second before the first time flag

data <- data[(timeflagstart-2):length(data)]

## Delete last 5 rows

data <- data[1:(length(data) - 5)]

## Create data frame

data <- data.frame(
  rawdata = data
)

## Insert Time

data$time <- seq(0,(nrow(data)-1), 1)/2

## Find Background

background <- mean(data$rawdata[(nrow(data)-30):nrow(data)])
backgroundsd <- sd(data$rawdata[(nrow(data)-30):nrow(data)])
data$bkgd_subtracted <- data$rawdata - background

## Plot

plot(data$time, data$bkgd_subtracted, type = 'l')

## Find all local maxima

w <- which(diff(sign(c(diff(data$bkgd_subtracted), 0))) < 0)
w <- w + 1

## Find the true peaks in the data

truepeaks <- c()
store <- 0

for (i in 1:length(w)) {
  if (w[i] < 31) {
    next
  }
  if (data$bkgd_subtracted[w[i]] < 500) {
    next
  } 
  if ((abs(data$bkgd_subtracted[w[i]-30])/data$bkgd_subtracted[w[i]]) > .5 | (abs(data$bkgd_subtracted[w[i]-30])/data$bkgd_subtracted[w[i]]) < 0 ) {
    next
  }
  if ((w[i] < store + 60)) {
    next 
  }
  store <- w[i]
  truepeaks <- append(truepeaks, w[i])
}

print(truepeaks)

points(data$time[w], data$bkgd_subtracted[w], col = 'blue')
points(data$time[truepeaks], data$bkgd_subtracted[truepeaks], col = 'red')

## Find the point the end point of each linear function

endpoints <- c()

for (i in 1:length(truepeaks)) {
  j = 0
  while ((data$bkgd_subtracted[truepeaks[i]+j]/(data$bkgd_subtracted[truepeaks[i]+j+1]) < endpoint_x) & 
         (data$bkgd_subtracted[truepeaks[i]+j+1]/(data$bkgd_subtracted[truepeaks[i]+j+2]) < endpoint_x)) {
    print((data$bkgd_subtracted[truepeaks[i]+j]/(data$bkgd_subtracted[truepeaks[i]+j+1])))
    j = j+1
  }
  endpoints <- append(endpoints, truepeaks[i]+j)
}

plot(data$time, data$bkgd_subtracted, type = 'l')

print(endpoints)
points(data$time[endpoints], data$bkgd_subtracted[endpoints], col = 'blue')

## Find the true start point of each function

startpoint <- c()

for (i in 1:length(truepeaks)) {
  j = 15
  while ((data$bkgd_subtracted[truepeaks[i]-j]/(data$bkgd_subtracted[truepeaks[i]-j-1])) > 1.05) {
    print((data$bkgd_subtracted[truepeaks[i]-j]/(data$bkgd_subtracted[truepeaks[i]-j-1])))
    j = j+1
  }
  startpoint <- append(startpoint, truepeaks[i]-j)
}

print(startpoint)
points(data$time[startpoint], data$bkgd_subtracted[startpoint], col = 'green')


## Plot to see where every peak starts and stops

filename <- paste(dir, 'peak finder.jpg')

jpeg(filename)

plot(data$time, data$bkgd_subtracted, type = 'l')
points(data$time[startpoint], data$bkgd_subtracted[startpoint], col = 'green')
points(data$time[endpoints], data$bkgd_subtracted[endpoints], col = 'blue')
points(data$time[truepeaks], data$bkgd_subtracted[truepeaks], col = 'red')

dev.off()

## Truestart

truestart <- startpoint - inputdelay * 2

## Plots and fits

counts <- c()

for (i in 1:length(truepeaks)) {
  start <- truestart[i]
  end <- endpoints[i]
  fitstart <- (truepeaks[i]- truestart[i])+1
  trim <- data[start:end,]
  trim$time <- trim$time - trim$time[1]
  trim$bkgd_subtracted[1:fitstart] <- NA
  trim$inverse <- 1/trim$bkgd_subtracted
  
  intercept <- format(lm(trim$inverse ~ trim$time)$coefficients[1], digits = 3)
  slope <- format(lm(trim$inverse ~ trim$time)$coefficients[2], digits = 3)
  
  r2 <- format(summary(lm(trim$inverse ~ trim$time))[8], digits = 4)
  conc <- 1/(lm(trim$inverse ~ trim$time)$coefficients[1])
  
  filename <- paste0(dir, 'plot_', i, '.jpg')
  
  jpeg(filename)
    
  plot(trim$time, trim$inverse, type = 'p', ylab = expression(paste(over(1, 'Counts'))), xlab = 'Time (s)', cex.lab = 0.75, mgp = c(2,1,0), cex.axis = 0.75)
  abline(lm(trim$inverse ~ trim$time), col = 'red')
  text(10, par()$yaxp[2], expression(over(1,'[C]') == over(1,paste('[C'[0],']')) + 'kt'))
  text(10, par()$yaxp[2] - (par()$yaxp[2]-par()$yaxp[1])/5, bquote('y' == .(slope) * ' t' + .(intercept)))
  text(10, par()$yaxp[2] - (par()$yaxp[2]-par()$yaxp[1])/5 * 2, bquote('R'^2 == .(r2)))
  text(10, par()$yaxp[2] - (par()$yaxp[2]-par()$yaxp[1])/5 * 3, bquote(paste('Counts'[0],'') == .(conc)))
  
  dev.off()
  
  counts <- append(counts, conc)
  
}

## Log Concentration vs Log Counts plot

comb <- data.frame(counts, data2, log(counts, 10), log(data2, 10))
colnames(comb) <- c('counts', 'concentration', 'log_counts', 'log_concentration')

intercept <- format(lm(comb$log_concentration ~ comb$log_counts)$coefficients[1], digits = 4)
slope <- format(lm(comb$log_concentration ~ comb$log_counts)$coefficients[2], digits = 4)
slopecalc <- lm(comb$log_concentration ~ comb$log_counts)$coefficients[2]
interceptcalc <- lm(comb$log_concentration ~ comb$log_counts)$coefficients[1]
LOD <- 10^(slopecalc * log10(backgroundsd * 3) + interceptcalc)

r2 <- format(summary(lm(comb$log_concentration ~ comb$log_counts))[8], digits = 4)

filename <- paste0(dir, 'log(Concentration) vs log(Counts).jpg')
 
jpeg(filename)

plot(comb$log_counts, comb$log_concentration, ylab = bquote(paste('log([O'[2]^'-',']) (nM)')), xlab = 'log(Counts)', mgp=c(2.5,1,0))
abline(lm(comb$log_concentration ~ comb$log_counts), col = 'red')

text(par()$xaxp[1]*1.05, par()$yaxp[2], bquote('y' == .(slope) * ' counts' + .(intercept)))
text(par()$xaxp[1]*1.05, par()$yaxp[2]*.92, bquote('R'^2 == .(r2)))
text(par()$xaxp[1]*1.05, par()$yaxp[2]*.83, bquote(paste('[O'[2]^'-',']' == 10^paste(.(slope),' log(Counts) + ', .(intercept) ))))
text(par()$xaxp[1]*1.05, par()$yaxp[2]*.75, bquote(paste('LOD' == '3', sigma['bkgd']) == paste(.(LOD), ' nM')))

dev.off()
