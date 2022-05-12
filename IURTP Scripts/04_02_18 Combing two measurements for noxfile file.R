
file <- file.choose()

noxfile <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
head(noxfile)

noxfile$time <- as.POSIXct(noxfile$time, format = '%m/%d/%Y %H:%M')

## NO

no <- aggregate(no ~ time, noxfile, mean)
noobs <- aggregate(no ~ time, noxfile, na.omit(length))

nomatch <- match(noxfile$time, no$time)

noxfile$noESSG <- noxfile$nosd ^ 2 * (44)
noxfile$noGSS <- (noxfile$no - no$no[nomatch]) ^ 2 * 45

noESSG <- aggregate(noESSG ~ time, noxfile, sum)
noGSS <- aggregate(noGSS ~ time, noxfile, sum)
nosd <- sqrt((noESSG$noESSG + noGSS$noGSS)/(noobs$no * 45 - 1))

## NO2

no2 <- aggregate(no2 ~ time, noxfile, mean)
no2obs <- aggregate(no2 ~ time, noxfile, na.omit(length))

no2match <- match(noxfile$time, no2$time)

noxfile$no2ESSG <- noxfile$no2sd ^ 2 * (44)
noxfile$no2GSS <- (noxfile$no2 - no2$no2[no2match]) ^ 2 * 45

no2ESSG <- aggregate(no2ESSG ~ time, noxfile, sum)
no2GSS <- aggregate(no2GSS ~ time, noxfile, sum)
no2sd <- sqrt((no2ESSG$no2ESSG + no2GSS$no2GSS)/(no2obs$no2 * 45 - 1))

## HONO

hono <- aggregate(hono ~ time, noxfile, mean)
honoobs <- aggregate(hono ~ time, noxfile, na.omit(length))

honomatch <- match(noxfile$time, hono$time)

noxfile$honoESSG <- noxfile$honosd ^ 2 * (44)
noxfile$honoGSS <- (noxfile$hono - hono$hono[honomatch]) ^ 2 * 45

honoESSG <- aggregate(honoESSG ~ time, noxfile, sum)
honoGSS <- aggregate(honoGSS ~ time, noxfile, sum)
honosd <- sqrt((honoESSG$honoESSG + honoGSS$honoGSS)/(honoobs$hono * 45 - 1))

## Export ##

time <- unique(noxfile$time)

notimematch <- match(time, no$time)
no2timematch <- match(time, no2$time)
honotimematch <- match(time, hono$time)

export <- data.frame(time, no$no[notimematch], nosd[notimematch], no2$no2[no2timematch], no2sd[no2timematch], hono$hono[honotimematch], honosd[honotimematch])
colnames(export) <- c('time', 'no', 'nosd', 'no2', 'no2sd', 'hono', 'honosd')

filename <- 'C:/Users/Zachary/Desktop/Final Concencentrations.csv'
write.csv(export, file = filename, row.names = FALSE)

head(export)
head(noxfile)



