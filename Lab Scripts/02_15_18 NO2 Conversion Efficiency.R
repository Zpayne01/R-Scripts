## NO2 Conversion Efficiency 

file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

zeroair <- 1207 #sccm

csv$NO2concentration <- (csv$Flow * 2.732)/(csv$Flow + zeroair) * 1000 #ppb

nocountsave <- aggregate(NO.Counts ~ NO2concentration, csv, mean)
blc1countsave <- aggregate(BLC.1 ~ NO2concentration, csv, mean)
blc2countsave <- aggregate(BLC.2 ~ NO2concentration, csv, mean)

csv$nocountsESSG <- csv$NO.sd^2 * (45 - 1)
csv$blc1ESSG <- csv$BLC1.sd^2 * (45 - 1)
csv$blc2ESSG <- csv$BLC2.sd^2 * (45 - 1)

nocountsESSG <- aggregate(nocountsESSG ~ NO2concentration, csv, sum)
blc1countsESSG <- aggregate(blc1ESSG ~ NO2concentration, csv, sum)
blc2countsESSG <- aggregate(blc2ESSG ~ NO2concentration, csv, sum)

csv$nocountsGV <- (csv$NO.Counts - nocountsave$NO.Counts[match(csv$NO2concentration, nocountsave$NO2concentration)])^2 * 45
csv$blc1countsGV <- (csv$BLC.1 - blc1countsave$BLC.1[match(csv$NO2concentration, blc1countsave$NO2concentration)])^2 * 45
csv$blc2countsGV <- (csv$BLC.2 - blc2countsave$BLC.2[match(csv$NO2concentration, blc2countsave$NO2concentration)])^2 * 45

nocountsGV <- aggregate(nocountsGV ~ NO2concentration, csv, sum)
blc1countsGV <- aggregate(blc1countsGV ~ NO2concentration, csv, sum)
blc2countsGV <- aggregate(blc2countsGV ~ NO2concentration, csv, sum)

nocountssd <- sqrt((nocountsESSG$nocountsESSG + nocountsGV$nocountsGV)/(3*45 - 1))
blc1countssd <- sqrt((blc1countsESSG$blc1ESSG + blc1countsGV$blc1countsGV)/(3*45 - 1))
blc2countssd <- sqrt((blc2countsESSG$blc2ESSG + blc2countsGV$blc2countsGV)/(3*45 - 1))

complete <- data.frame(nocountsave, nocountssd, blc1countsave$BLC.1, blc1countssd, blc2countsave$BLC.2, blc2countssd)
complete$NO <- complete$NO.Counts/2694
complete$NOsd <- complete$nocountssd/2694

complete$blc1CE <- complete$blc1countsave.BLC.1/(complete$NO2concentration * 2694)
complete$blc2CE <- complete$blc2countsave.BLC.2/(complete$NO2concentration * 2694)

complete$blc1CEsd <- complete$blc1countssd/(complete$NO2concentration * 2694)
complete$blc2CEsd <- complete$blc2countssd/(complete$NO2concentration * 2694)

filename <- 'C:/Users/Zachary/Desktop/NO2_Conversion_Efficiency_Complete.csv'
write.csv(complete, file = filename, row.names = FALSE)

