## NOx Werx Residence Time ##

library(deSolve)

#File Input - raw counts from instrument #

file <- file.choose() ## Used data from 07/22
tab <- read.csv(file, header = T)

# Inputs #

Flowrate <- 1582
o3 <- c(209.14, 212.0, 211.9, 212.1)
date <- '07/22/2021'
pressure <- 1 # atm
temp <- 22.2 # C

nocalslope <- 1616
nocalint <- -8

## Pre-photolysis cell ##

noStartPreCell <- '14:45'
noEndPreCell <- '14:55'

no2StartPreCell <- '15:05'
no2EndPreCell <- '15:15'

## Post Photolysis Cell ##

noStartPostCell <- '15:50'
noEndPostCell <- '16:00'

no2StartPostCell <- '16:05'
no2EndPostCell <- '16:15'

## Mean Ozone ##

meano3 <- mean(o3)

## Convert Time ##

tab$Time <- as.POSIXct(tab$Time, format = '%Y-%m-%d %H:%M:%S')

## Find the Pre-Cell NO average ##

noStartPreCell <- as.POSIXct(paste(date, noStartPreCell), format = '%m/%d/%Y %H:%M')
noEndPreCell <- as.POSIXct(paste(date, noEndPreCell), format = '%m/%d/%Y %H:%M')

matchPreNoStart <- match(as.integer(noStartPreCell), as.integer(tab$Time))
matchPreNoEnd <- match(as.integer(noEndPreCell), as.integer(tab$Time))

nocountsPreCell <- tab$no_counts[matchPreNoStart:matchPreNoEnd]
noconc <- (nocountsPreCell - nocalint)/nocalslope

noPreCellAve <- mean(noconc)
noPreCellSD <- sd(noconc)

## Find NO Pre-Cell after adding O3 ##

no2StartPreCell <- as.POSIXct(paste(date, no2StartPreCell), format = '%m/%d/%Y %H:%M')
no2EndPreCell <- as.POSIXct(paste(date, no2EndPreCell), format = '%m/%d/%Y %H:%M')

matchPreNo2Start <- match(as.integer(no2StartPreCell), as.integer(tab$Time))
matchPreNo2End <- match(as.integer(no2EndPreCell), as.integer(tab$Time))

nocountsPreCellWithO3 <- tab$no_counts[matchPreNo2Start:matchPreNo2End]
noconcPreCellWithO3 <- (nocountsPreCellWithO3 - nocalint)/nocalslope

noPreCellAvewithO3 <- mean(noconcPreCellWithO3)
noPreCellSDwithO3 <- sd(noconcPreCellWithO3)

## Find NO Post-Cell before adding O3 ##

noStartPostCell <- as.POSIXct(paste(date, noStartPostCell), format = '%m/%d/%Y %H:%M')
noEndPostCell <- as.POSIXct(paste(date, noEndPostCell), format = '%m/%d/%Y %H:%M')

matchPostNoStart <- match(as.integer(noStartPostCell), as.integer(tab$Time))
matchPostNoEnd <- match(as.integer(noEndPostCell), as.integer(tab$Time))

nocountsPostCell <- tab$no_counts[matchPostNoStart:matchPostNoEnd]
noconc <- (nocountsPostCell - nocalint)/nocalslope

noPostCellAve <- mean(noconc)
noPostCellSD <- sd(noconc)

## Find NO Post-Cell after adding O3 ##

no2StartPostCell <- as.POSIXct(paste(date, no2StartPostCell), format = '%m/%d/%Y %H:%M')
no2EndPostCell <- as.POSIXct(paste(date, no2EndPostCell), format = '%m/%d/%Y %H:%M')

matchPostNo2Start <- match(as.integer(no2StartPostCell), as.integer(tab$Time))
matchPostNo2End <- match(as.integer(no2EndPostCell), as.integer(tab$Time))

nocountsPostCellWithO3 <- tab$no_counts[matchPostNo2Start:matchPostNo2End]
noconcPostCellWithO3 <- (nocountsPostCellWithO3 - nocalint)/nocalslope

noPostCellAvewithO3 <- mean(noconcPostCellWithO3)
noPostCellSDwithO3 <- sd(noconcPostCellWithO3)

## NO precell difference ##

noPreCellDif <- noPreCellAve - noPreCellAvewithO3

# Convert mixing ratio to molec cm3

R <- 0.08216 #atm L mol-1 K-1
avagadro <- 6.022E23
Lmol <- R*(temp + 273.15)/pressure

noPreCellAve_molec <- noPreCellAve/1E9/Lmol/1000*avagadro
meano3_molec <- meano3/1E9/Lmol/1000*avagadro
noPreCellAvewithO3_molec <- noPreCellAvewithO3/1E9/Lmol/1000*avagadro

noPostCellAve_molec <- noPostCellAve/1E9/Lmol/1000*avagadro
meano3_molec <- meano3/1E9/Lmol/1000*avagadro
noPostCellAvewithO3_molec <- noPostCellAvewithO3/1E9/Lmol/1000*avagadro

## O3 and NO differential equation ##

parameters = c(k1 = 1.7E-14)

OzoneCycle <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         #Rate of change
         
         dNO <- -k1*NO*O3 
         dO3 <- -k1*NO*O3
         dNO2 <- k1*NO*O3 
         
         #return the rate of change
         list(c(dNO, dO3, dNO2))
       })
}

times <- seq(0, 100, by = .01)

state_precell = c(NO = noPreCellAve_molec, O3 = meano3_molec, NO2 = 0)
out_precell <- data.frame(ode(y = state_precell, times = times, func = OzoneCycle, parms = parameters, method = 'euler'))

timemin_pos <- which.min(abs(noPreCellAvewithO3_molec - out_precell$NO))
RT_PreCell <- out_precell$time[timemin_pos] #Residence Time in Seconds

## Post Cell Residenece Time

state_postcell <-  c(NO = noPostCellAve_molec, O3 = meano3_molec, NO2 = 0)
out_Postcell <- data.frame(ode(y = state_postcell, times = times, func = OzoneCycle, parms = parameters, method = 'euler'))

timemin_pos <- which.min(abs(noPostCellAvewithO3_molec - out_Postcell$NO))
RT_PostCell <- out_Postcell$time[timemin_pos] #Residence Time in Seconds

## RT Cell ##

RT_Cell = RT_PreCell - RT_PostCell

## RT_cell was 2.01
## RT_PostCell was 1.05
## RT_PreCell was 3.06

NO_start <- 6.16
NO_start_molec <- NO_start/1E9/Lmol/1000*avagadro

O3_start <- 191.32
O3_start_molec <- O3_start/1E9/Lmol/1000*avagadro

NO_end <- 5.64
NO_end_molec <- NO_end/1E9/Lmol/1000*avagadro

state_test = c(NO = NO_start_molec, O3 = O3_start_molec, NO2 = 0)
out_test <- data.frame(ode(y = state_test, times = times, func = OzoneCycle, parms = parameters, method = 'euler'))

timemin_pos <- which.min(abs(NO_end_molec - out_test$NO))
RT_test <- out_test$time[timemin_pos]

Psuedok <- O3_start_molec * 1.7E-14
print(RT_test)
(log(NO_start)-log(NO_end))/Psuedok

