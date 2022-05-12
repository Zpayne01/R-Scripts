## Trying to use OH reactivity to estimate Isoprene

install.packages('readxl')
library('readxl')

directory <- 'E:/Lab computer/2017 IURTP/IURTP 2017 Reactivity/Relevant Days'
f <- list.files('E:/Lab computer/2017 IURTP/IURTP 2017 Reactivity/Relevant Days')

comb <- data.frame(
  Time = c(),
  Reactivity = c()
)

for(i in 1:length(f)) {
  
  date <- strsplit(f[i], ' ')[[1]][1]
  data <- read_excel(paste0(directory,'/', f[i]), sheet = 'Stats', skip = 2)
  
  data <- data.frame(Time = data$`Average Time`,
                     Reactivity = data$`Adjusted Reactivity`)
  
  data$Time <- format(data$Time, '%H:%M:%S')
  data$Time <- paste(date, data$Time)

  comb <- rbind(comb, data)
  
  }

comb$Time <- as.POSIXct(comb$Time, format = '%m-%d-%y %H:%M:%S')

comb$Time <- floor_date(comb$Time, unit = '1 hour')

export <- ddply(comb, .(Time), summarize, reactivity = mean(Reactivity), sd = sd(Reactivity), median = summary(Reactivity)[3])

write.csv(export, 'C:/Users/zacpayne/Desktop/Hourly Reactivity.csv', row.names = FALSE)

## Matching OH with isoprene

file2 <- file.choose()
data2 <- read_excel(file2)

data2$Time <- format(data2$Time, '%Y-%m-%d %H:%M:%S')
data2$Time <- as.POSIXct(data2$Time, tz = Sys.timezone())

m <- match(floor_date(force_tz(data2$Time, tz = Sys.timezone()), '1 hour'), output$Time)

export2 <- data.frame(
  Time = data2$Time,
  Iso_can = data2$Isoprene,
  Reactivity = output$mean[m]
)

write.csv(export2, 'C:/Users/zacpayne/Desktop/Matched to Full Isoprene.csv', row.names = FALSE)

file3 <- file.choose()

data3 <- read_excel(file3)
data3$Time <- seq.POSIXt(as.POSIXct('2017-08-02 00:00:00'), as.POSIXct('2017-08-07 23:00:00'), '1 hour')

m <- match(data3$Time, export$Time)

export3 <- data.frame(
  data3,
  export[m,]
)

write.csv(export3, 'C:/Users/zacpayne/Desktop/Comparing Measured OH Reactivity to V3 Modelled.csv', row.names = FALSE)

## Using Pam's averaged data

file4 <- file.choose()
data4 <- data.frame(read_excel(file4))

colnames(data4) <- c('Time', 'OH_reactivity', 'SD', 'n_points')

data4$total <- data4$OH_reactivity * data4$n_points
data4$var <- (data4$n_points - 1) * data4$SD^2  
  
data4$Time <- floor_date(force_tz(data4$Time, tz = Sys.timezone()), unit = '1 hour')

output <- ddply(data4, .(Time), summarize, mean = sum(total)/sum(n_points), mult = n_points)

write.csv(output, 'C:/Users/zacpayne/Desktop/pam averaged to 1 hour.csv', row.names = FALSE)
