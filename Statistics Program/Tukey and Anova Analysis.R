
file1 <- file.choose()
file2 <- file.choose()
file3 <- file.choose()

csv1 <- read.csv(file1, stringsAsFactors = FALSE)
csv2 <- read.csv(file2, stringsAsFactors = FALSE)
csv3 <- read.csv(file3, stringsAsFactors = FALSE)

csv1$Time <- as.POSIXlt(csv1$Time, format = '%m/%d/%Y %H:%M')
csv2$Time <- as.POSIXlt(csv2$Time, format = '%m/%d/%Y %H:%M')
csv3$Time <- as.POSIXlt(csv3$Time, format = '%m/%d/%Y %H:%M')

no <- data.frame()

for(i in 0:23) {

combined <- data.frame(cbind.fill(csv1$no_ave[csv1$Time$hour == i], csv2$no_ave[csv2$Time$hour == i], csv3$no_ave[csv3$Time$hour == i]))
colnames(combined) <- c('B','C','D')
stacked <- stack(na.omit(combined))

stacked_aov <- aov(values ~ ind, data = stacked)
summary(stacked_aov)

tk1 <- TukeyHSD(stacked_aov, 'ind')

letterpair <- as.data.frame.matrix(HSD.test(stacked_aov, 'ind', group = TRUE)$group)
letterpair_hour <- cbind(letterpair, c(row.names(letterpair_hour)),c(i, i, i)) 

colnames(letterpair_hour) <- c('value', 'group', 'chamber', 'hour')

no <- rbind(no, letterpair_hour)

}

no2 <- data.frame()

for(i in 0:23) {
  
  combined <- data.frame(cbind.fill(csv1$no2_ave[csv1$Time$hour == i], csv2$no2_ave[csv2$Time$hour == i], csv3$no2_ave[csv3$Time$hour == i]))
  colnames(combined) <- c('B','C','D')
  stacked <- stack(na.omit(combined))
  
  stacked_aov <- aov(values ~ ind, data = stacked)
  summary(stacked_aov)
  
  tk1 <- TukeyHSD(stacked_aov, 'ind')
  
  letterpair <- as.data.frame.matrix(HSD.test(stacked_aov, 'ind', group = TRUE)$group)
  letterpair_hour <- cbind(letterpair, c(row.names(letterpair_hour)),c(i, i, i)) 
  
  colnames(letterpair_hour) <- c('value', 'group', 'chamber', 'hour')
  
  no2 <- rbind(no2, letterpair_hour)
  
}

hono <- data.frame()

for(i in 0:23) {
  
  combined <- data.frame(cbind.fill(csv1$hono_ave[csv1$Time$hour == i], csv2$hono_ave[csv2$Time$hour == i], csv3$hono_ave[csv3$Time$hour == i]))
  colnames(combined) <- c('B','C','D')
  stacked <- stack(na.omit(combined))
  
  stacked_aov <- aov(values ~ ind, data = stacked)
  summary(stacked_aov)
  
  tk1 <- TukeyHSD(stacked_aov, 'ind')
  
  letterpair <- as.data.frame.matrix(HSD.test(stacked_aov, 'ind', group = TRUE)$group)
  letterpair_hour <- cbind(letterpair, c(row.names(letterpair_hour)),c(i, i, i)) 
  
  colnames(letterpair_hour) <- c('value', 'group', 'chamber', 'hour')
  
  hono <- rbind(hono, letterpair_hour)
  
}


