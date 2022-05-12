## N2O extraction 

file <- file.choose()

data <- read.table(file, sep = ' ', skip = 1, header = FALSE)
data <- data[,1:8]

colnames(data) <- c('datetime', '446', '456', '546', '447', 'N2O', 'CO2', '448')

## Change the string to a relevant time format

data$datetime <- as.POSIXct(data$datetime+(3600*5),  origin = "1904-01-01 00:00:00", tz = Sys.timezone())
print(head(data))
print(tail(data))

## Experiment

start <- as.POSIXct('2021-01-26 00:00:32')
end <- as.POSIXct('2021-01-26 23:59:59')

startmatch <- match(as.integer(start), as.integer(data$datetime))
startmatch

endmatch <-match(as.integer(end), as.integer(data$datetime))
endmatch

plot(data$datetime[startmatch:endmatch], 
     data$`446`[startmatch:endmatch], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[N2O] (ppb)')

## Water Points ##

start1 <- '2021-01-26 11:56:00'
end1 <- '2021-01-26 12:00:00'

start2 <- '2021-01-26 12:50:00'
end2 <- '2021-01-26 12:54:00'

start3 <- '2021-01-26 13:32:00'
end3 <- '2021-01-26 13:38:00'

start4 <- '2021-01-26 14:08:00'
end4 <- '2021-01-26 14:12:00'

start5 <- '2021-01-26 11:24'
end5 <- '2021-01-26 11:26'

calc <- c(195.3, 193.0, 190.0, 188, 192.8)

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

RH <- c(80, 60, 40, 25, 0)

df1 <- data.frame(
  calculate = calc,
  RH = RH,
  mean = c(average1, average2, average3, average4, average5),
  sd = c(sd1, sd2, sd3, sd4, sd5)
)

plot((mean/calc) ~ RH, df1, type = 'p')
points()
lm(mean ~ calculate, df1)

write.table(df1, 'clipboard', row.names = FALSE)

## Check ##

## Anova ##

plot(data$datetime[matchstart1:matchend4], 
     data$`446`[matchstart1:matchend4], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[N2O] (ppb)')

## N2O Bubbler Extraction ##

start <- as.POSIXct('2021-01-26 14:30:00')
end <- as.POSIXct('2021-01-26 14:45:00')

startmatch <- match(as.integer(start), as.integer(data$datetime))
startmatch

endmatch <-match(as.integer(end), as.integer(data$datetime))
endmatch

plot(data$datetime[startmatch:endmatch], 
     data$`446`[startmatch:endmatch], 
     type = 'l', 
     xlab = 'Time (EST)',
     ylab = '[N2O] (ppb)')

start1 <- '2021-01-26 16:42:00'
end1 <- '2021-01-26 16:44:00'

start2 <- '2021-01-26 16:23:00'
end2 <- '2021-01-26 16:27:00'

start3 <- '2021-01-26 14:38:00'
end3 <- '2021-01-26 14:40:00'

start4 <- '2021-01-26 17:00:00'
end4 <- '2021-01-26 17:03:00'

matchstart1 <- match(as.integer(as.POSIXct(start1)), as.integer(data$datetime))
matchend1 <- match(as.integer(as.POSIXct(end1)), as.integer(data$datetime))

matchstart2 <- match(as.integer(as.POSIXct(start2)), as.integer(data$datetime))
matchend2 <- match(as.integer(as.POSIXct(end2)), as.integer(data$datetime))

matchstart3 <- match(as.integer(as.POSIXct(start3)), as.integer(data$datetime))
matchend3 <- match(as.integer(as.POSIXct(end3)), as.integer(data$datetime))

matchstart4 <- match(as.integer(as.POSIXct(start4)), as.integer(data$datetime))
matchend4 <- match(as.integer(as.POSIXct(end4)), as.integer(data$datetime))

measured1 <- data$`446`[matchstart1:matchend1]
measured2 <- data$`446`[matchstart2:matchend2]
measured3 <- data$`446`[matchstart3:matchend3]
measured4 <- data$`446`[matchstart4:matchend4]

average1 <- mean(measured1)
average2 <- mean(measured2)
average3 <- mean(measured3)
average4 <- mean(measured4)

sd1 <- sd(measured1)
sd2 <- sd(measured2)
sd3 <- sd(measured3)
sd4 <- sd(measured4)

df2 <- data.frame(
  group = c('Bubble Background', 'Bubble Measurement', 'Zero Background', 'No Bubble Measurement'),
  average = c(average1, average2, average3, average4),
  sd = c(sd1, sd2, sd3, sd4)
)

write.table(df2, 'clipboard', row.names = FALSE)
