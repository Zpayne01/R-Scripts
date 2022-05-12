## N2O extraction (.str file)

file <- file.choose()

data <- read.table(file, sep = ' ', skip = 1, header = FALSE)
data <- data[,1:8]

colnames(data) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')

## Change the string to a relevant time format

data$datetime <- as.POSIXct(data$datetime+(3600*4), origin = "1904-01-01 00:00:00", tz = Sys.timezone())
print(head(data))
print(tail(data))

## Experiment

start <- as.POSIXct('2021-06-01 00:00:01')
end <- as.POSIXct('2021-06-01 10:00:00')

startmatch <- match(as.integer(start), as.integer(data$datetime))
startmatch

endmatch <-match(as.integer(end), as.integer(data$datetime))
endmatch

plot(data$datetime[startmatch:endmatch], 
     data$`446`[startmatch:endmatch], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[N2O] (ppb)')
lines(data$datetime[startmatch:endmatch], data$`546`[startmatch:endmatch], col = 'blue', type = 'l')
lines(data$datetime[startmatch:endmatch], data$`456`[startmatch:endmatch], col = 'red', type = 'l')

## Write CSV with raw ##

filenamelist <- strsplit(file, "\\\\")
filenamelistlength <- length(filenamelist[[1]])
filenameout <- strsplit(filenamelist[[1]][filenamelistlength], '\\.')[[1]][1]

filename <- paste0('C:/Users/zacpayne/Desktop/', filenameout, '_str.csv')
write.csv(data, filename, row.names = FALSE)

filename <- 'C:/Users/zacpayne/Desktop/06_01_21 Zero Comparison.csv'
write.csv(data[startmatch:endmatch,], file = filename, row.names = FALSE)

## Take out points I want ##

data2 <- data[startmatch:endmatch,]
datatimesremoved <- data2[c(4100:4524,4600:4915,4945:5970,6010:6410,6440:6750,6780:7130,7160:7480,7515:7810,7830:8250,8270:8900,8965:9631),]

datazero <- data2[c(10107:nrow(data2)),]

bkgd446 <- mean(datazero$`446`)
bkgd456 <- mean(datazero$`456`)
bkgd546 <- mean(datazero$`546`)

datatimesremoved$`446` <- datatimesremoved$`446` - bkgd446
datatimesremoved$`456` <- datatimesremoved$`456` - bkgd456
datatimesremoved$`546` <- datatimesremoved$`546` - bkgd546

filename <- 'C:/Users/zacpayne/Desktop/05_20_21 Isotope Study times removed.csv'
write.csv(datatimesremoved, file = filename, row.names = FALSE)


## Zero ##

startzero <-  as.POSIXct('2021-05-04 12:42:00')
endzero <- as.POSIXct('2021-05-04 12:44:00')

startzeromatch <- match(as.integer(startzero), as.integer(data$datetime))
startzeromatch

endzeromatch <-match(as.integer(endzero), as.integer(data$datetime))
endzeromatch

zerodata <- data$`446`[startzeromatch:endzeromatch]
zeroavg <- mean(zerodata)
zerosd <- sd(zerodata)

## 546 ##

zero546 <- data$`546`[startzeromatch:endzeromatch]
zero546ave <- mean(zero546)
zero546sd <- sd(zero546)

## 456 ##

zero456 <- data$`456`[startzeromatch:endzeromatch]
zero456ave <- mean(zero456)
zero456sd <- sd(zero456)
  
## N2O Calibration ##

start1 <- '2021-01-25 15:44:00'
end1 <- '2021-01-25 15:52:00'

start2 <- '2021-01-25 16:25:00'
end2 <- '2021-01-25 16:28:00'

start3 <- '2021-01-25 16:32:00'
end3 <- '2021-01-25 16:35:00'

start4 <- '2021-01-25 16:42:00'
end4 <- '2021-01-25 16:45:00'

start5 <- '2021-01-25 16:58:00'
end5 <- '2021-01-25 17:02:00'

calc <- c(196, 89.8, 113.4, 144.8, 162.7)

matchstart1 <- match(as.integer(as.POSIXct(start1)), as.integer(data$datetime))
matchend1 <- match(as.integer(as.POSIXct(end1)), as.integer(data$datetime))

matchstart2 <- match(as.integer(as.POSIXct(start2)), as.integer(data$datetime))
matchend2 <- match(as.integer(as.POSIXct(end2)), as.integer(data$datetime))

matchstart3 <- match(as.integer(as.POSIXct(start3)), as.integer(data$datetime))
matchend3 <- match(as.integer(as.POSIXct(end3)), as.integer(data$datetime))

matchstart4 <- match(as.integer(as.POSIXct(start4)), as.integer(data$datetime))
matchend4 <- match(as.integer(as.POSIXct(end4)), as.integer(data$datetime))

matchstart5 <- match(as.integer(as.POSIXct(start5)), as.integer(data$datetime))
matchend5 <- match(as.integer(as.POSIXct(end5)), as.integer(data$datetime))

measured1 <- data$`446`[matchstart1:matchend1]
measured2 <- data$`446`[matchstart2:matchend2]
measured3 <- data$`446`[matchstart3:matchend3]
measured4 <- data$`446`[matchstart4:matchend4]
measured5 <- data$`446`[matchstart5:matchend5]

average1 <- mean(measured1)
average2 <- mean(measured2)
average3 <- mean(measured3)
average4 <- mean(measured4)
average5 <- mean(measured5)

sd1 <- sd(measured1)
sd2 <- sd(measured2)
sd3 <- sd(measured3)
sd4 <- sd(measured4)
sd5 <- sd(measured5)

df1 <- data.frame(
  calculate = calc,
  calc_error = calc*.10,
  mean = c(average1, average2, average3, average4, average5),
  sd = c(sd1, sd2, sd3, sd4, sd5)
)

plot(mean ~ calculate, df1, type = 'p')
lm(mean ~ calculate, df1)
