## Mass file sent from Ryan made before starting the incubation

file1 <- file.choose()
massfile <- read.table(file1, sep = '\t', header = TRUE)

colnames(massfile) <- c('date','chamber', 'plot', 'mass')
massfile$date <- as.POSIXlt(massfile$date, format = '%m/%d/%Y')
massfile$date <- format(massfile$date, '%m/%d/%Y')

## Use the chamber even file to determine the correct concentrations

file2 <- file.choose()
noxfile <- read.csv(file2, header = TRUE)
head(noxfile)

noxfile$timeb <- as.POSIXlt(noxfile$timeb, format = '%m/%d/%Y %H:%M')
noxfile$date <- format(noxfile$timeb, '%m/%d/%Y')
masssub <- subset(massfile, massfile$date == noxfile$date[2])


## Average blank value obtained from blank chamber measurements

blankb <- 0.065654
blankc <- 0.047277
blankd <- 0.055168
blanke <- 0.06493

## Flows obtained from measurements made before starting the experiment  
  
flowb <- 2.183
flowc <- 2.180
flowd <- 2.274
flowe <- 2.127

## Flux is in units of ng/(g soil hr) or ug/(kg soil hr)

nob <- (noxfile$nob-blankb)/10^9/22.4*flowb*14*10^9*60/masssub$mass[1]
nobsd <- noxfile$nobsd/10^9/22.4*flowb*14*10^9*60/masssub$mass[1]

noc <- (noxfile$noc-blankc)/10^9/22.4*flowc*14*10^9*60/masssub$mass[2]
nocsd <- noxfile$nocsd/10^9/22.4*flowc*14*10^9*60/masssub$mass[2]

nod <- (noxfile$nod-blankd)/10^9/22.4*flowd*14*10^9*60/masssub$mass[3]
nodsd <- noxfile$nodsd/10^9/22.4*flowd*14*10^9*60/masssub$mass[3]

noe <- (noxfile$noe-blanke)/10^9/22.4*flowe*14*10^9*60/masssub$mass[4]
noesd <- noxfile$noesd/10^9/22.4*flowe*14*10^9*60/masssub$mass[4]

library(lubridate)

export <- data.frame(floor_date(noxfile$timeb, unit = '1 hour'), noxfile$hour, nob, nobsd, noc, nocsd, nod, nodsd, noe, noesd)
colnames(export) <- c('date', 'hour', 'nob', 'nobsd', 'noc', 'nocsd', 'nod', 'nodsd', 'noe', 'noesd')

write.csv(export, file = 'C:/Users/Zachary/Desktop/NO.csv', row.names = FALSE)
