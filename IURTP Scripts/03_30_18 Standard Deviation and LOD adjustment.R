## Removal of IURTP points based on the standard deviations of the respective species ##
## This process is done after adjusting for relative humidity effects to account for the effect on the standard deviation ##

file <- file.choose()

## Choose a file named humdity_corrected_x

noxfile <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
head(noxfile)
tail(noxfile)

## Remove relavant points based on the standard deviations ##

nostdlimit <- 1 ## ppb ##
no2stdlimit <- 1.5 ## ppb ##
honostdlimit <- 3.5 ## ppb ##

noxfile$no[noxfile$nosd > nostdlimit] <- ''
noxfile$nosd[noxfile$nosd > nostdlimit] <- ''

noxfile$no2[noxfile$no2sd > no2stdlimit] <- ''
noxfile$no2sd[noxfile$no2sd > no2stdlimit] <- ''

noxfile$hono[noxfile$honosd > honostdlimit] <- ''
noxfile$honosd[noxfile$honosd > honostdlimit] <- ''

## Set negative numbers to the LOD ##

noLOD <- .005
no2LOD <- .013
honoLOD <- .300

noxfile$no[noxfile$no < 0 & noxfile$no != ''] <- 0

noxfile$no2[noxfile$no2 < 0 & noxfile$no2 != ''] <- 0

noxfile$hono[noxfile$hono < 0 & noxfile$hono != ''] <- 0

## Set NAs as blank cells ##

noxfile$no[is.na(noxfile$no)] <- ''
noxfile$nosd[is.na(noxfile$nosd)] <- ''

noxfile$no2[is.na(noxfile$no2)] <- ''
noxfile$no2sd[is.na(noxfile$no2sd)] <- ''

noxfile$hono[is.na(noxfile$hono)] <- ''
noxfile$honosd[is.na(noxfile$honosd)] <- ''

## Export as Deviation Correct NOxfile ##

write.csv(noxfile, file = 'C:/Users/Zachary/Desktop/Deviation_Corrected.csv', row.names = FALSE)
