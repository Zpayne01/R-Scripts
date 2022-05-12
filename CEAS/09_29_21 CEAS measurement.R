#### Libraries ####

library(zoo)
library(ggplot2)

#### Function ####

## Background subtraction ##

bkgdsub <- function(data, zeroout) { m <- mean(c(data[1:230],data[650:length(data)]))
dataout <- data - m
return(dataout)
}

## Alpha determination ##

aHONO <- function (honodata, bkgddata) { ((d*100 - 12.7)/(d*100))*((1 - reflectivity)/(d*100) + n2ext) * ((bkgddata - honodata)/honodata)}

#### Reflectivity from N2 and He ####

n2file <- file.choose()
hefile <- file.choose()

n2 <- read.table(n2file, header = F, sep = '\t')
he <- read.table(hefile, header = F, sep = '\t')

names <- c('wavelength', 'intensity')

colnames(n2) <- names
colnames(he) <- names

windows(width=10, height=8)
plot(intensity ~ wavelength, he, type = 'l', xlab = 'Wavelength (nm)', ylab = 'Intensity (counts)')
lines(intensity ~ wavelength, n2, col = 'red')
legend(330, 4250, legend = c('helium', 'nitrogen'), col = c('black', 'red'), lty = 1:1)

n2$n2bkgdsub <- bkgdsub(n2$intensity, 0)
he$hebkgdsub <- bkgdsub(he$intensity, 0)

windows(width=10, height=8)
plot(hebkgdsub ~ wavelength, he, type = 'l', xlab = 'Wavelength (nm)', ylab = 'Dark Subtracted Intensity (Counts)')
lines(n2bkgdsub ~ wavelength, n2, col = 'red')
legend("topleft", inset = 0.05, legend = c('helium', 'nitrogen'), col = c('black', 'red'), lty = 1:1)


d <- 1.013 #Caivity length in m

## Rayleigh cross sections from Washenfelder 2013 - AMT

n2raycross <- function(wavelength) {1.2577E-15 * wavelength^(-4.1814)} #Wavelength in nm
heraycross <- function(wavelength) {1.336E-17 * wavelength^(-4.1287)}

## Rayleigh extinction coefficient from Andrews Book - cross section * 2.463E19 (molecules cm-3 at STP)

rayextinction <- function(cross, pressure, temperature) {cross * (pressure/760)/((temperature+273.15) * 8.205E-5)*6.022E23/1E6}

n2cross <- n2raycross(n2$wavelength)
hecross <- heraycross(he$wavelength)

plot(n2$wavelength, n2cross, type = 'l')
plot(he$wavelength, hecross, type = 'l')

n2ext <- rayextinction(n2cross, 769.8, 22)
heext <- rayextinction(hecross, 769.4, 22)

# Plot the Rayleigh optical extinction coefficient

plot(n2$wavelength, n2ext, type = 'l', mgp = c(2.5,1,0), xlab = 'Wavelength (nm)', ylab = expression(paste(alpha['N'['2']], ' (cm'^-1, ')'))) 
plot(he$wavelength, heext, type = 'l', mgp = c(2.5,1,0), xlab = 'wavelength (nm)', ylab = expression(paste(alpha['He'], ' (cm'^-1, ')'))) 

stuff <- ((n2$n2bkgdsub/he$hebkgdsub) * n2ext - heext)/(1 - (n2$n2bkgdsub/he$hebkgdsub))

reflectivity <- -1 * (stuff * d*100 - 1)

dfreflectivity = data.frame(wavelength = n2$wavelength,
                            reflectivity = reflectivity)
dfreflectivity$reflectivity[(dfreflectivity$wavelength < 360 | dfreflectivity$wavelength > 380)] = 0

#Using a rolling mean function to extract less noisy reflectivity data

meanwavelength <- rollmean(dfreflectivity$wavelength[350:621], k = 25)
meanreflectivity <- rollmean(reflectivity[350:621], k = 25)

dfreflectivityfit <- data.frame(wavelength = meanwavelength,
                                reflectivity = meanreflectivity)

windows(width=10, height =8)
par(mar = c(5,5,4,10), xpd = F)
plot(reflectivity ~ wavelength, dfreflectivity[300:650,], type = 'l',
     ylab = expression(paste('R(', lambda, ')')),
     xlab = 'Wavelength (nm)',
     ylim = c(0.9995, 0.9998))
lines(dfreflectivityfit$reflectivity ~ dfreflectivityfit$wavelength, col = 'red')
legend("right", inset = c(-0.25, 0), legend = c('Raw', 'Rolling Mean'), cex = 0.8, col = c('black', 'red'), lty = 1:1, xpd = T)


## Lets try using the fitted reflectivity for calculations instead of the current reflectivity
## data to see if it reduces noise

## Pathlength calculation and plot

length = data.frame(wavelength = n2$wavelength, #Without fitted data
                    pathlength = 1.013/(1-reflectivity))

plot(pathlength ~ wavelength, length[300:650,], type = 'l')

length2 = data.frame(wavelength = dfreflectivityfit$wavelength, #With the fitted reflectivity data
                     pathlength = 1.013/(1-dfreflectivityfit$reflectivity)) 

#Pathlength plot in black with fitted reflectivity in red

par(mar = c(5,5,4,8), xpd = F)
plot(pathlength ~ wavelength, length[350:580,], type = 'l',
     ylab = 'Pathlength (m)',
     xlab = 'Wavelength (nm)')
lines(pathlength ~ wavelength, length2, col = 'red')
legend("right", inset = c(-0.25, 0), legend = c('Raw', 'From Mean'), cex = 0.8, col = c('black', 'red'), lty = 1:1, xpd = T)

max(dfreflectivity$reflectivity[375:550])
dfreflectivity$wavelength[which.max(dfreflectivity$reflectivity[375:575])+375]
which.max(length$pathlength[375:500])
maxpathlengthfit <- max(length2$pathlength)
length$pathlength[375+122]

## Use this section when testing the HONO concentration

#alphaHONO is the optical extinction in the BBCES cavity. The equation is seen in a variety
#of papersincluding Washenfelder 2013 - Broadband measurements of aerosol extinction in the UV spectral region

bkgd1file <- file.choose()
hono1file <- file.choose()

bkgd1 <- read.table(bkgd1file, header = F, sep = '\t')
hono1 <- read.table(hono1file, header = F, sep = '\t')

colnames(bkgd1) <- names
colnames(hono1) <- names

bkgd1$intensitybkgdsub <- bkgdsub(bkgd1$intensity)
hono1$intensitybkgdsub <- bkgdsub(hono1$intensity)

plot(bkgd1$intensitybkgdsub ~ bkgd1$wavelength, type = 'l')
lines(hono1$intensitybkgdsub ~ hono1$wavelength, col = 'red')

alphaHONO = ((1 - reflectivity)/(d*100) + n2ext) * (bkgd1$intensitybkgdsub/hono1$intensitybkgdsub - 1) 
#alphaHONO = ((1 - reflectivity)/(d*100)) * ((bkgd2$intensity - hono2$intensity)/hono2$intensity)

dfalphaHONO = data.frame(wavelength = hono1$wavelength,
                         alphaHONO = alphaHONO)

plot(alphaHONO ~ wavelength, dfalphaHONO, type = 'l')
start <- findclose(362, dfalphaHONO$wavelength)
end <- findclose(372, dfalphaHONO$wavelength)

#### HONO Cross Section ####

# The cross section data was pulled from the JPL document (provided in csv form by Emily)

HONOcrossfile <- file.choose()
HONOcross <- read.csv(HONOcrossfile)

m <- sapply(dfalphaHONO$wavelength, function(x) {which.min(abs(x - HONOcross$nm.avg))})

dfalphaHONO$HONOcross <- HONOcross$cross.section[m]

# Convolute the HONO cross section to overlap the wavelengths from the HONO40 file (for the used LED)

plot(HONOcross ~ wavelength, dfalphaHONO[375:575,], type = 'l', mgp = c(2.5,1,0), ylab = expression(paste(sigma['HONO'], ' molecules cm'^'-3')), xlab = 'Wavelength (nm)')

crosspred <- spline(HONOcross$nm.avg, HONOcross$cross.section, xout = hono40$wavelength)
lines(crosspred, col = 'red')

# Function for match the cross section data with the alpha data by wavelength

dfalphaHONO$HONOcross <- HONOcross$cross.section[m]
dfalphaHONO$HONOcrosspred <- crosspred$y
  
# Solve for the concentration of HONO by the alpha and cross section, 2.46E10 is the 
# molec/cm3 to ppb correction

dfalphaHONO$HONOconc <- dfalphaHONO$alphaHONO/dfalphaHONO$HONOcross/2.46E10

plot(HONOconc ~ wavelength, dfalphaHONO[375:575,], type = 'p')
plot(alphaHONO ~ wavelength, dfalphaHONO[375:575,], type = 'l')
plot(HONOcross ~ wavelength, dfalphaHONO[375:575,], type = 'l')

plot(alphaHONO ~ HONOcross, dfalphaHONO[375:575,], type = 'p')
lm(alphaHONO ~ HONOcross, dfalphaHONO[375:575,])
abline(lm(alphaHONO ~ HONOcross, dfalphaHONO[375:575,]), col = 'red')

## 08/12 tests on the different temperature HONO 

bkgdfile <- file.choose()
hono40file <- file.choose()
hono45file <- file.choose()
hono50file <- file.choose()
hono55file <- file.choose()

bkgd <- read.table(bkgdfile, header = F, sep = '\t')
hono40 <- read.table(hono40file, header = F, sep = '\t')
hono45 <- read.table(hono45file, header = F, sep = '\t')
hono50 <- read.table(hono50file, header = F, sep = '\t')
hono55 <- read.table(hono55file, header = F, sep = '\t')

names <- c('wavelength', 'intensity')

colnames(bkgd) <- names
colnames(hono40) <- names
colnames(hono45) <- names
colnames(hono50) <- names
colnames(hono55) <- names

bkgd$group <- 0
hono40$group <- 40
hono45$group <- 45
hono50$group <- 50
hono55$group <- 55

honocomb <- rbind(bkgd, hono40, hono45, hono50, hono55)

library(ggplot2)

# Plot the varying concentrations

p <- ggplot() + geom_line(data = honocomb, aes(x = wavelength, y = intensity, group = group, color = group)) + 
  xlab("Wavelength (nm)") + ylab("Intensity (counts)") + labs(color = 'Temperature')
p + scale_color_gradient(low = 'black', high = 'red')

## Subtract Background ##



bkgd$intensitybkgdsub <- bkgdsub(bkgd$intensity)
hono40$intensitybkgdsub <- bkgdsub(hono40$intensity)
hono45$intensitybkgdsub <- bkgdsub(hono45$intensity)
hono50$intensitybkgdsub <- bkgdsub(hono50$intensity)
hono55$intensitybkgdsub <- bkgdsub(hono55$intensity)

honocomb <- rbind(bkgd, hono40, hono45, hono50, hono55)

p <- ggplot() + geom_line(data = honocomb, aes(x = wavelength, y = intensitybkgdsub, group = group, color = group)) + 
  xlab("Wavelength (nm)") + ylab("Dark Subtracted Intensity (counts)") + labs(color = 'Temperature')
p + scale_color_gradient(low = 'black', high = 'red')


# Calculate the alpha 
# Reflectivity is from the pathlength on 08/09

aHONO40 <- aHONO(hono40$intensitybkgdsub, bkgd$intensitybkgdsub)
aHONO45 <- aHONO(hono45$intensitybkgdsub, bkgd$intensitybkgdsub)
aHONO50 <- aHONO(hono50$intensitybkgdsub, bkgd$intensitybkgdsub)
aHONO55 <- aHONO(hono55$intensitybkgdsub, bkgd$intensitybkgdsub)

dfalphaHONO40 = data.frame(wavelength = hono40$wavelength,
                         alphaHONO = aHONO40, group = 40)
dfalphaHONO45 = data.frame(wavelength = hono45$wavelength,
                           alphaHONO = aHONO45, group = 45)
dfalphaHONO50 = data.frame(wavelength = hono50$wavelength,
                           alphaHONO = aHONO50, group = 50)
dfalphaHONO55 = data.frame(wavelength = hono55$wavelength,
                           alphaHONO = aHONO55, group = 55)
dfalphacomb = rbind(dfalphaHONO40, dfalphaHONO45, dfalphaHONO50, dfalphaHONO55)

p2 <- ggplot(data = dfalphacomb, aes(x = wavelength, y = alphaHONO, group = group, color = group)) + ylab(expression(paste(alpha['HONO']))) + xlab('Wavelength (nm)') + labs(color = 'Temperature') + geom_line() + coord_cartesian(xlim = c(360, 380), ylim = c(-1E-8, 1E-6))
p2 + scale_color_gradient(low = 'grey50', high = 'red')

p5 <- ggplot(dfalphaHONO55[375:550,], aes(x = wavelength, y = alphaHONO)) + theme_bw() + theme(panel.grid = element_blank(), panel.border = element_rect(color = 'black', fill = NA, linetype  = 1)) +
      geom_line(size = 1.05) + 
      labs(y = expression(paste(alpha['HONO'])), x = 'Wavelength (nm)')
p5

## Get the concentration out ##

m40 <- sapply(dfalphaHONO40$wavelength, function(x) {which.min(abs(x - HONOcross$nm.avg))})
m45 <- sapply(dfalphaHONO45$wavelength, function(x) {which.min(abs(x - HONOcross$nm.avg))})
m50 <- sapply(dfalphaHONO50$wavelength, function(x) {which.min(abs(x - HONOcross$nm.avg))})
m55 <- sapply(dfalphaHONO55$wavelength, function(x) {which.min(abs(x - HONOcross$nm.avg))})

dfalphaHONO40$HONOcross <- HONOcross$cross.section[m40]
dfalphaHONO45$HONOcross <- HONOcross$cross.section[m45]
dfalphaHONO50$HONOcross <- HONOcross$cross.section[m50]
dfalphaHONO55$HONOcross <- HONOcross$cross.section[m55]

## Using convulted cross section data

dfalphaHONO40$Honocrosspred <- crosspred$y
dfalphaHONO45$Honocrosspred <- crosspred$y
dfalphaHONO50$Honocrosspred <- crosspred$y
dfalphaHONO55$Honocrosspred <- crosspred$y

# Solve for the concentration of HONO by the alpha and cross section, 2.46E10 is the 
# molec/cm3 to ppb correction

dfalphaHONO40$HONOconc <- dfalphaHONO40$alphaHONO/dfalphaHONO40$HONOcross/2.46E10
dfalphaHONO45$HONOconc <- dfalphaHONO45$alphaHONO/dfalphaHONO45$HONOcross/2.46E10
dfalphaHONO50$HONOconc <- dfalphaHONO50$alphaHONO/dfalphaHONO50$HONOcross/2.46E10
dfalphaHONO55$HONOconc <- dfalphaHONO55$alphaHONO/dfalphaHONO55$HONOcross/2.46E10

## Solve from spline predicted cross section values

dfalphaHONO40$HONOconcpred <- dfalphaHONO40$alphaHONO/dfalphaHONO40$Honocrosspred/2.46E10
dfalphaHONO45$HONOconcpred <- dfalphaHONO45$alphaHONO/dfalphaHONO45$Honocrosspred/2.46E10
dfalphaHONO50$HONOconcpred <- dfalphaHONO50$alphaHONO/dfalphaHONO50$Honocrosspred/2.46E10
dfalphaHONO55$HONOconcpred <- dfalphaHONO55$alphaHONO/dfalphaHONO55$Honocrosspred/2.46E10


dfalphacomb = rbind(dfalphaHONO40, dfalphaHONO45, dfalphaHONO50, dfalphaHONO55)

p3 <- ggplot(data = dfalphacomb, aes(x = wavelength, y = HONOconcpred, group = group, color = group)) + geom_line() + coord_cartesian(xlim = c(360, 380), ylim = c(0, 200)) + labs(color = 'Temp', x = 'Wavelength (nm)', y = expression(paste('[HONO] (ppb)')))
p3 + scale_color_gradient(low = 'gray50', high = 'red')

#Compare the differences between the two cross section examples

p4 <- ggplot(data = dfalphaHONO40, aes(x = wavelength, y = HONOconc, col = 'Black')) + 
  geom_line(size = 1.05) + 
  geom_line(aes(y = HONOconcpred, color = 'Red'), size = 1.05) + 
  coord_cartesian(xlim = c(367, 371), ylim = c(0, 100)) + 
  scale_color_identity(name = "Cross Section",
                       breaks = c('Black', 'Red'),
                       labels = c('Literature', 'Convoluted'),
                       guide = 'legend')
p4

# Mean HONO concentrations from 367.5 - 368.5#

hono40start <- which.min(abs(hono40$wavelength - 367.5))
hono45start <- which.min(abs(hono45$wavelength - 367.5))
hono50start <- which.min(abs(hono50$wavelength - 367.5))
hono55start <- which.min(abs(hono55$wavelength - 367.5))

hono40end <- which.min(abs(hono40$wavelength - 368.5))
hono45end <- which.min(abs(hono45$wavelength - 368.5))
hono50end <- which.min(abs(hono50$wavelength - 368.5))
hono55end <- which.min(abs(hono55$wavelength - 368.5))

hono40meanconc <- mean(dfalphaHONO40$HONOconcpred[hono40start:hono40end])
hono45meanconc <- mean(dfalphaHONO45$HONOconcpred[hono45start:hono45end])
hono50meanconc <- mean(dfalphaHONO50$HONOconcpred[hono50start:hono50end])
hono55meanconc <- mean(dfalphaHONO55$HONOconcpred[hono55start:hono55end])

hono40sdconc <- sd(dfalphaHONO40$HONOconcpred[hono40start:hono40end])
hono45sdconc <- sd(dfalphaHONO45$HONOconcpred[hono45start:hono45end])
hono50sdconc <- sd(dfalphaHONO50$HONOconcpred[hono50start:hono50end])
hono55sdconc <- sd(dfalphaHONO55$HONOconcpred[hono55start:hono55end])

# Using a range from 350 - 500 (channels)

hono40c <- 0.3592
hono45c <- 0.5613
hono50c <- 0.7851

hono40calc <- hono40c * hono55meanconc
hono45calc <- hono45c * hono55meanconc
hono50calc <- hono50c * hono55meanconc

# Plotting the reference and the calculated #

honocomp <- data.frame(honocrosscalc = c(hono40meanconc, hono45meanconc, hono50meanconc, hono55meanconc),
           honodoas = c(hono40calc, hono45calc, hono50calc, hono55meanconc))

plot(honodoas ~ honocrosscalc, honocomp, type = 'p',
     xlab = c('[HONO] (ppb) from Absolute Cross Section'),
     ylab = c('[HONO] (ppb) DOASIS'))

abline(a = 0, b = 1, col = 'red')

#
par(mar = c(5,5,4,4))
plot(HONOcross ~ wavelength, dfalphaHONO[375:575,], type = 'l', ylab = expression(paste(sigma['HONO'], ' (cm'^'-2',')')), xlab = 'Wavelength (nm)')

pred <- spline(HONOcross$nm.avg, HONOcross$cross.section, xout = hono40$wavelength)
lines(pred, col = 'red')
legend("topleft", inset = 0.02, legend = c('JPL', 'Fitted Spline'), col = c('black', 'red'), lty = 1:1, cex = 0.8)

## Build my own reference spectra ##

HONOconcspec <- 100 * 2.46E10

estimated <- (HONOconcspec * pred$y) * length$pathlength*100
dfest <- data.frame(wavelength = length$wavelength, intensity = estimated)
plot(dfest$wavelength, dfest$intensity, type = 'l', mgp = c(2.5, 1, 0), xlab = 'Wavelength (nm)', ylab = expression(paste('-log(I)'/'log(I '[0],')')))
write.table(dfest, row.names = F, col.names = F, sep = '\t', file = 'C:/Users/zacpayne/Desktop/CEAS HONO Ref.txt')

HONOconcest <- c(18.25, 28.42, 42.16, 52.01)

honocomp <- data.frame(honocrosscalc = c(hono40meanconc, hono45meanconc, hono50meanconc, hono55meanconc),
                       honodoas = c(hono40calc, hono45calc, hono50calc, hono55meanconc),
                       honodoasest = HONOconcest)

plot(honodoasest ~ honocrosscalc, honocomp, type = 'p',
     xlab = c('[HONO] (ppb) from Absolute Cross Section'),
     ylab = c('[HONO] (ppb) DOASIS (Estimated from Cross Section)'),
     xlim = c(0,100),
     ylim = c(0,100))
legend("topleft", inset = 0.05, legend = c('Points', '1:1 line'), col = c('Black', 'Red'), pch = c(1,NA), lty = c(0,1))
abline(a = 0, b = 1, col = 'red')

## Reference spectrum plot ##

refspectra <- -(log(hono55$intensitybkgdsub / bkgd$intensitybkgdsub))

plot(dfest$wavelength[350:550], dfest$intensity[350:550], type = 'l', mgp = c(2.5, 1, 0), xlab = 'Wavelength (nm)', ylab = expression(paste('-log(I)'/'log(I '[0],')')))
lines(hono55$wavelength[350:550], refspectra[350:550], col = 'red')
legend('topleft', inset = 0.05, legend = c('100 ppb Reference', 'HONO Generator (55 C)'), col = c('black', 'red'), lty = c(1:1))

## Reference spectra comparison

refcomp <- refspectra/estimated

plot(dfest$wavelength[350:550], refcomp[350:550], type = 'p')

## Build my own reference spectra using the fitted pathlength ##

m <- sapply(length$wavelength, function(x) which.min(abs(x - length2$wavelength)))

length3 <- data.frame(wavelength = length$wavelength,
                      pathlength = length2$pathlength[m])
plot(pathlength ~ wavelength, length3[360:600,], type = 'l')

estimated2 <- (HONOconcspec * pred$y) * length3$pathlength*100
dfest2 <- data.frame(wavelength = length$wavelength, intensity = estimated2)
plot(dfest2$wavelength[320:600], dfest2$intensity[320:600], type = 'l', mgp = c(2.5, 1, 0), xlab = 'Wavelength (nm)', ylab = expression(paste('-log(I)'/'log(I '[0],')')))
lines(intensity ~ wavelength, dfest[320:600,], col = 'red')
lines(refspectra ~ length3$wavelength, col = 'blue')

write.table(dfest, row.names = F, col.names = F, sep = '\t', file = 'C:/Users/zacpayne/Desktop/CEAS HONO Ref.txt')

## Build an NO2 Ref ##
## Cross sections are from HITRAN database

NO2file <- file.choose()
NO2cross <- scan(NO2file, sep = ' ')
NO2cross <- NO2cross[!is.na(NO2cross)]

wavelength1 <- 1e7/(42002.302)
wavelength2 <- 1e7/(15001.993)

wavelengthstep <- (wavelength2 - wavelength1)/27993
wavelengthvec <- seq(wavelength1, wavelength2-wavelengthstep, wavelengthstep)

NO2crossdf <- data.frame(wavelength = wavelengthvec,
                         crosssection = NO2cross)

plot(crosssection ~ wavelength, NO2crossdf, type = 'l')

m <- sapply(length$wavelength, function(x) {which.min(abs(x - NO2crossdf$wavelength))})

NO2crossdf2 <- data.frame(wavelength = length$wavelength,
                          crosssection = NO2crossdf$crosssection[m])
plot(crosssection ~ wavelength, NO2crossdf2, type = 'l')

NO2conc <- 100 * 2.46E10

estimatedNO2 <- (NO2conc * NO2crossdf2) * length$pathlength*100

dfest3 <- data.frame(wavelength = length$wavelength, intensity = estimatedNO2$crosssection)
plot(intensity ~ wavelength, dfest3[350:500,], type = 'l', mgp = c(2.5, 1, 0), xlab = 'Wavelength (nm)', ylab = expression(paste('-log(I)'/'log(I '[0],')')), ylim = c(0.05, 1))
lines(intensity ~ wavelength, dfest[350:500,], col = 'red')

write.table(dfest3, file = 'C:/Users/zacpayne/Desktop/100 ppb NO2 Reference.txt', row.names = F, col.names = F)

## Lets see how the intensity compares in here to what Andrew has ##
## There was a good comparison between Andrew's data and my data when looking
## specifically at the -log(I/Io) plots

plot(intensitybkgdsub ~ wavelength, bkgd, type = 'l')
lines(intensitybkgdsub ~ wavelength, hono40, col = 'red')

logint55 <- -log(hono55$intensitybkgdsub/bkgd$intensitybkgdsub)

plot(logint55[300:500], type = 'l')

output <- data.frame(wavelength = length$wavelength,
                     intensity = logint40)
write.table(output, file = 'C:/Users/zacpayne/Desktop/HONO 40.txt', sep = '\t', col.names = F, row.names = F)

## Intensity plot ##

intensity = bkgd$intensitybkgdsub * exp(- (HONOconcspec * pred$y) * length$pathlength*100)
plot(intensity ~ length3$wavelength, ylim = c(-100, 1500), type = 'l', xlim = c(355, 380))
lines(intensitybkgdsub ~ wavelength, hono55, col = 'red')
lines(intensitybkgdsub ~ wavelength, bkgd, col = 'blue')

# Alpha HONO of prediction plot 

aHONOpred <- aHONO(intensity, bkgd$intensitybkgdsub)
HONOconcpred <- aHONOpred/crosspred$y/2.46E10
plot(length$wavelength, aHONOpred, ylim = c(0, 2e-6), xlim = c(360,375), type = 'l', xlab = 'Wavelegnth (nm)', ylab = expression(paste(alpha['HONO'], ' (cm'^-1,')')))
lines(length$wavelength, aHONO55, col = 'red')
legend("topleft", inset = 0.05, legend = c('Reference Alpha', 'HONO Generator (55 C)'), col = c('black', 'red'), lty = c(1:1), cex = 0.75)

plot(length$wavelength, HONOconcpred, type = 'l', ylim = c(0,200), xlim = c(360, 375), xlab = 'Wavelength (nm)', ylab = expression(paste('[HONO] (ppb)')))
lines(dfalphaHONO55$wavelength, dfalphaHONO55$HONOconcpred, col = 'red')
legend("bottomleft", inset = 0.05, legend = c('Reference Alpha', 'HONO Generator (55 C)'), col = c('black', 'red'), lty = c(1:1), cex = 0.75)

colnames(dfalphaHONO)
mean(HONOconcpred[360:380])

plot(length$wavelength, aHONOpred, type = 'l', xlim = c(360, 375), ylim = c(1E-8, 1.5E-6))
lines(length$wavelength, dfalphaHONO55$alphaHONO, col = 'red')

#### Stutz Cross Section ####

HONOfile2 <- file.choose()
HONOcross2 <- read.table(file = HONOfile2, sep = '')

colnames(HONOcross2) <- c('Wavelength', 'CrossSection')

plot(CrossSection ~ Wavelength, HONOcross2, type = 'l')

length$wavelength[1]
length$wavelength[nrow(length)]

subHONOcross2 <- subset(HONOcross2, HONOcross2$Wavelength >= 330 & HONOcross2$Wavelength <= 410)

plot(CrossSection ~ Wavelength, subHONOcross2, type = 'l')

## Deconvolution ##

findclose <- function(x, y) {
  output <- c()
  for (i in 1:length(x)) {
  output[i] <- which.min(abs(x[i] - y))
  }
  return(output)
}

output <- findclose(subHONOcross2$Wavelength, length$wavelength)
subHONOcross2$WavelengthDeconv <- length$wavelength[output]

HONOcrossDeconv <- aggregate(CrossSection ~ WavelengthDeconv, subHONOcross2, mean)
lines(CrossSection ~ WavelengthDeconv, HONOcrossDeconv, col = 'red')

## Strutz Alpha ##

HONOconcspec <- 100 * 2.46E10 #at STP

HONOcrossDeconv$alpha <- HONOconcspec * HONOcrossDeconv$CrossSection
plot(alpha ~ WavelengthDeconv, HONOcrossDeconv, type = 'l')
lines(alphaHONO ~ wavelength, dfalphaHONO35, col = 'red')

HONOcrossDeconv$Conc <- HONOcrossDeconv$alpha/HONOcrossDeconv$CrossSection/2.46E10

plot(Conc ~ WavelengthDeconv, HONOcrossDeconv, type = 'l', ylim = c(80, 120))

referencealpha <- data.frame(wavelength = HONOcrossDeconv$WavelengthDeconv, 
                             alpha = HONOcrossDeconv$alpha)
names(referencealpha) <- NULL

write.table(referencealpha, file = 'C:/Users/zacpayne/Desktop/HONO Reference alpha.txt', row.names = F, sep = '\t')
write.table(dfalphaHONO55[,1:2], file = 'C:/Users/zacpayne/Desktop/HONO 55 alpha.txt', row.names = F, sep = '\t')

#### Secondary Strutz Fit (run after the first stuff) ####

HONOconcspec2 <- 80 * 2.46E10
HONOcrossDeconv$alpha2 <- HONOconcspec2 * HONOcrossDeconv$CrossSection
plot(alpha ~ WavelengthDeconv, HONOcrossDeconv, type = 'l')
lines(alpha2 ~ WavelengthDeconv, HONOcrossDeconv, col = 'red')

#### Plotting dfalphaHONO and strutz stuff ####

plot(alphaHONO ~ wavelength, dfalphaHONO[start:end,], type = 'l')
HONOcrossDeconvSub <- HONOcrossDeconv[start:end,]
plot(alpha ~ WavelengthDeconv, HONOcrossDeconvSub, type = 'l', ylim = c(2E-8, 1.5E-6))
lines(alphaHONO ~ wavelength, dfalphaHONO[start:end,], col = 'red')

#### Plotfit ####

c <- dfalphaHONO$alphaHONO[start:end]/HONOcrossDeconvSub$alpha

plot(alpha*.20 ~ WavelengthDeconv, HONOcrossDeconvSub, type = 'l')
lines(alphaHONO ~ wavelength, dfalphaHONO[start:end,], col = 'red')


#### NO2 Fit ####

NO2conc <- 100 * 2.46E10

NO2file <- file.choose()
NO2cross <- read.table(NO2file)

NO2alpha <- data.frame(
  wavelength = NO2cross$V1,
  alpha = NO2cross$V2 * NO2conc
)

output <- findclose(NO2alpha$wavelength, length$wavelength)

NO2alpha$WavelengthDeconv <- length$wavelength[output]

NO2alphaDeconv <- aggregate(alpha ~ WavelengthDeconv, NO2alpha, mean)

plot(alpha ~ wavelength, NO2alpha, type = 'l')
plot(alpha ~ WavelengthDeconv, NO2alpha, type = 'l')
lines(alpha ~ WavelengthDeconv, NO2alpha, col = 'red')

#### Alphafit take two ####

file <- file.choose()
dfalphaHONO2 <- read.delim(file, col.names = c('wavelength', 'alpha'))

plot(alpha ~ wavelength, dfalphaHONO2[start:end,], type = 'l')
HONOcrossDeconvSub <- HONOcrossDeconv[start:end,]
plot(alpha ~ WavelengthDeconv, HONOcrossDeconvSub, type = 'l', ylim = c(2E-8, 1.5E-6))
lines(alpha ~ wavelength, dfalphaHONO2[start:end,], col = 'red')

NO2alphasub <- NO2alphaDeconv[start:end,]

HONOcrossDeconvSub$x <- HONOcrossDeconvSub$WavelengthDeconv

x <- dfalphaHONO2$wavelength[start:end]
fit = nls(y ~ c * dfalphaHONO2$alpha[start:end] * (s + t*x) + (a1*x^2 + a2*x + a3), data = HONOcrossDeconvSub, start(c = 1, s = 1, t = 1, a1 = 1, a2 = 1, a3 = 1))
fit = nls(y ~ c  * (s + t*x) + (a1*x^2 + a2*x + a3), data = HONOcrossDeconvSub, start = list(c = 1, s = 1, t = 1, a1 = 1, a2 = 1, a3 = 1))

fit = nls(y ~ c * dfalphaHONO2$alpha[start:end] + (a1*x^2 + a2*x + a3), data = HONOcrossDeconvSub, start = list(c = 1, a1 = 0.5, a2 = 0.5, a3 = 0.5))

fit = nls(y ~ (c * dfalphaHONO2$alpha[start:end] + d * dfalphaHONO2$alpha[start:end]*x), data = HONOcrossDeconvSub, start = list(c = 1, d = 1))
fit = nls(y ~ (c * dfalphaHONO2$alpha[start:end] + d * dfalphaHONO2$alpha[start:end]*x) + (a1 * x^2 + a2 * x + a3), data = HONOcrossDeconvSub, start = list(c = 1, d = 1, a1 = 1, a2 = 1, a3 = 1))


fit = nls(y ~ a1 * x^2 + a2 * x + a3, data = HONOcrossDeconvSub, start = list(a1 = 1, a2 = 1, a3 = 1))

fit = nls(y ~ c * dfalphaHONO2$alpha[start:end] * x + poly(x, 3), data = HONOcrossDeconvSub, start = list(c = 1, s = 1, t = 1))

fit = nls(y ~ c * dfalphaHONO2$alpha[start:end] * (1 + x) + a1*x^3 + a2*x^2 + a3*x, data = HONOcrossDeconvSub, start = list(c = .9, a1 = .1, a2 = .9, a3 = .9), trace = T)

fit = nls(y ~ c * dfalphaHONO2$alpha[start:end] * x, data = HONOcrossDeconvSub, start = list(c = .1))
fit2 = nls(y ~ dfalphaHONO2$alpha[start:end] + a1*x^2,  data = HONOcrossDeconvSub, start = list(a1 = .1))
fit3 = nls(y ~ c * dfalphaHONO2$alpha[start:end] * x + a1*x^2 + a2*x + a3,  data = HONOcrossDeconvSub, start = list(c = .1, a1 = .1, a2 = .1, a3 = .1), nls.control(tol = 1e-2))

fit = nls(y ~ a1*x^3 + a2*x^2 + a3*x + a4, data = HONOcrossDeconvSub, start = list( a1 = .1, a2 = .9, a3 = .1, a4 = 0.9), trace = T, nls.control(tol = 1e-2))

fit = nls(y ~ (1/c)*alpha3 + a1*x^2 + a2*x + a3, data = HONOcrossDeconvSub, start = list(c = .1, a1 = 1, a2 = 1, a3 = 1), trace = T)
fit = nls(y ~ (1/c)*alpha3, data = HONOcrossDeconvSub, start = list(c = .1), trace = T)

fit = nls(alpha ~ (1/c)*dfalphaHONO2$alpha[start:end] + a2*x^2 + a3*x + a4, data = HONOcrossDeconvSub, start = list(c = .1, a2 = 1, a3 = 1, a4 = 1), trace = T)

HONOcrossDeconvSub$alphafit <- dfalphaHONO2$alpha[start:end]

fit = nls(alphafit ~ a0 + a1*x + a2*x^2 + a3 * alpha, HONOcrossDeconvSub, start = list(a0 = .1, a1 = .1, a2 = .1, a3 = 1))
fit = nls(alphafit ~ a0 + a3 * alpha, HONOcrossDeconvSub, start = list(a0 = .1, a3 = 1))
fit = nls(alphafit ~ a0 + a1*x + a2*x^2 + a3 * alpha + a4 * NO2, HONOcrossDeconvSub, start = list(a0 = .1, a1 = 0.1, a2 = .01, a3 = 1, a4 = 1), trace = TRUE, nls.control(tol = 1e-2))
fit = nls(alphafit ~ a3 * alpha, HONOcrossDeconvSub, start = list(a3 = 1))

plot(alphafit ~ x, HONOcrossDeconvSub, type = 'l')
pred = predict(fit, HONOcrossDeconvSub$x)
lines(HONOcrossDeconvSub$x,pred, col = 'red')

write.table(NO2alphaDeconv, file = filename, row.names = F)

## Data Time ##

bkgdfile1 <- file.choose()
bkgdfile2 <- file.choose()
bkgdfile3 <- file.choose()
bkgdfile4 <- file.choose()

bkgd1 <- read.table(bkgdfile1, sep = '\t')
bkgd2 <- read.table(bkgdfile2, sep = '\t')
bkgd3 <- read.table(bkgdfile3, sep = '\t')
bkgd4 <- read.table(bkgdfile4, sep = '\t')

colnames(bkgd1) <- names
colnames(bkgd2) <- names
colnames(bkgd3) <- names
colnames(bkgd4) <- names

bkgd1 <- data.frame(wavelength = bkgd1$wavelength, intensity = bkgdsub(bkgd1$intensity, 0))
bkgd2 <- data.frame(wavelength = bkgd2$wavelength, intensity = bkgdsub(bkgd2$intensity, 0))
bkgd3 <- data.frame(wavelength = bkgd3$wavelength, intensity = bkgdsub(bkgd3$intensity, 0))
bkgd4 <- data.frame(wavelength = bkgd4$wavelength, intensity = bkgdsub(bkgd4$intensity, 0))

plot(intensity ~ wavelength, bkgd1, type = 'l')
lines(intensity ~ wavelength, bkgd2, col = 'red')
lines(intensity ~ wavelength, bkgd3, col = 'blue')
lines(intensity ~ wavelength, bkgd4, col = 'pink')

abkgd2 <- aHONO(bkgd2$intensity, bkgd1$intensity)
abkgd4 <- aHONO(bkgd4$intensity, bkgd1$intensity)

plot(bkgd2$wavelength, abkgd2, type = 'l', xlim = c(360,380), ylim = c(-1e-6,1e-6))
lines(bkgd4$wavelength, abkgd4, col = 'red')

HONOhighfile <- file.choose()
HONOlowfile <- file.choose()

HONOhigh <- read.table(HONOhighfile, sep = '\t')
HONOlow <- read.table(HONOlowfile, sep = '\t')

colnames(HONOhigh) <- names
colnames(HONOlow) <- names

HONOhigh <- bkgdsub(HONOhigh$intensity, 0)
HONOlow <- bkgdsub(HONOlow$intensity, 0)

plot(length$wavelength, HONOhigh, type = 'l')
lines(length$wavelength, HONOlow, col = 'red')

alphaHONOhigh <- aHONO(HONOhigh, bkgd1$intensity)
alphaHONOlow <- aHONO(HONOlow, bkgd1$intensity)

plot(length$wavelength, alphaHONOlow, type = 'l', xlim = c(360, 380), ylim = c(-1e-7, 2e-6))
lines(length$wavelength, alphaHONOhigh, col = 'red')
lines(referencealpha$wavelength, referencealpha$alpha, col = 'blue')

## Check the concentrations ##

wavelengthstart <- 340
wavelengthend <- 400

length <- length[length$wavelength >= wavelengthstart & length$wavelength <= wavelengthend,]


alphaHONOhighdf <- data.frame(wavelength = length$wavelength, alpha = alphaHONOhigh, cross = HONOcrossDeconv$CrossSection)

#### Multiple N2 and He ####

n21file <- file.choose()
n22file <- file.choose()
n23file <- file.choose()

he1file <- file.choose()
he2file <- file.choose()
he3file <- file.choose()

n21 <- read.table(n21file, sep = '\t')
n22 <- read.table(n22file, sep = '\t')
n23 <- read.table(n23file, sep = '\t')

he1 <- read.table(he1file, sep = '\t')
he2 <- read.table(he2file, sep = '\t')
he3 <- read.table(he3file, sep = '\t')

n21 <- bkgdsub(n21$V2, 0)
n22 <- bkgdsub(n22$V2, 0)
n23 <- bkgdsub(n23$V2, 0)

he1 <- bkgdsub(he1$V2, 0)
he2 <- bkgdsub(he2$V2, 0)
he3 <- bkgdsub(he3$V2, 0)

n2 <- data.frame(wavelength = length$wavelength, intensity = rowMeans(data.frame(n21, n22, n23)))
he <- data.frame(wavelength = length$wavelength, intensity = rowMeans(data.frame(he1, he2, he3)))

## Note: The noise is reduced by including multiple N2 and He files and may be reduced further by inlcuding
## 10+ or by introducing a longer averaging time into spectra suite 

bkgdfilelist <- list.files('E:/CEAS Test/10_12', pattern = 'bkgd')
HONO35filelist <- list.files('E:/CEAS Test/10_12', pattern = 'HONO_35')
HONO40filelist <- list.files('E:/CEAS Test/10_12', pattern = 'HONO_40')

bkgdlist <- lapply(bkgdfilelist, function(x) {read.table(paste0('E:/CEAS Test/10_12/', x), sep = '\t')})
HONO35filelist <- lapply(HONO35filelist, function(x) {read.table(paste0('E:/CEAS Test/10_12/', x), sep = '\t')})
HONO40filelist <- lapply(HONO40filelist, function(x) {read.table(paste0('E:/CEAS Test/10_12/', x), sep = '\t')})

bkgdcomb <- data.frame(wavelength = bkgdlist[[1]][,1])

for (i in 1:length(bkgdlist)) {
  bkgdcomb <- data.frame(bkgdcomb, bkgdlist[[i]][2])
}

bkgdcombsub <- sapply(bkgdcomb[2:ncol(bkgdcomb)], function (x) bkgdsub(x, 0))

bkgdcomb <- data.frame(wavelength = bkgdcomb$wavelength,
                       intensity = rowMeans(bkgdcombsub))

plot(intensity ~ wavelength, bkgdcomb, type = 'l')

