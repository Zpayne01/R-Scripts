file <- file.choose()
tab <- read.csv(file, header = T)

head(tab)
colnames(tab) <- c('time','measured', 'measured_sd','a' ,'b', 'modeled', 'HO2_OH_ratio') #, 'HO2_OH_ratio'

LOD <- 1.5E5
LOD <- 5E7
tab$time <- as.POSIXlt(tab$time, format = '%m/%d/%Y %H:%M')

tab2 <- subset(tab, tab$measured > LOD)
tab2 <- subset(tab, tab$measured > LOD & (tab$time$hour < 8 | tab$time$hour > 20)) #Night
tab2 <- subset(tab, tab$measured > LOD & !(tab$time$hour < 8 | tab$time$hour > 20)) #Day
tab2 <- subset(tab, tab$measured > LOD & tab$time$mday == 2)

head(tab2)

over <- nrow(tab2[tab2$measured < tab2$modeled,])
under <- nrow(tab2[tab2$measured > tab2$modeled,])


meanobs <- mean(tab2$measured)
meanmod <- mean(tab2$modeled)
meanbias <- meanmod - meanobs
normalizedmeanbias <- sum(tab2$modeled - tab2$measured)/sum(tab2$measured) * 100
normalizedmeanerror <- sum(abs(tab2$modeled - tab2$measured))/sum(tab2$modeled) * 100
fit <- lm(modeled ~ measured, tab2)
out <- summary(fit)
intercept <- out$coefficients[1]
slope <- out$coefficients[2]
r2 <- out$r.squared
comb <- c(meanobs, meanmod, meanbias, normalizedmeanbias, normalizedmeanerror, intercept, slope, r2)
print(comb)

meancoefvar <- mean(tab2$measured_sd / tab2$measured)



plot(modeled ~ measured, tab2, type = 'p')
points(a ~ measured, tab2, col = 'red' )
points(HO2_OH_ratio ~ measured, tab2, col = 'blue')
abline(fit, col = 'red')

write.table(comb, file = 'clipboard', row.names = F)
