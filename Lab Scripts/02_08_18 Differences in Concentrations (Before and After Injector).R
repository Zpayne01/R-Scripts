
f <- list.files(path = 'C:/Users/Zachary/Documents/Lab Data/01_2018 Flow Tube Studies/Difference Files', pattern = '', full.names = TRUE)

for(i in 1:length(f))

{

file <- read.csv(f[i], header = TRUE)

before <- subset(file, file$Minutes <= 30)
after <- subset(file, file$Minutes >= 35)
afterNO <- subset(file, file$Minutes >= 55)

## NO Information 

## First

firstNObefore <- mean(na.omit(before$NO))
firstNOafter <- mean(na.omit(afterNO$NO))

firstNOessg <- na.omit(before$NOsd)^2 * (44)
firstNOessgsum <- sum(firstNOessg)
firstNOgss <- (na.omit(before$NO) - firstNObefore)^2*44
firstNOgsssum <- sum(firstNOgss)
firstNOgv <- sqrt((firstNOgsssum + firstNOessgsum)/((45 * length(na.omit(before$NO))-1)))

firstNOafteressg <- na.omit(afterNO$NOsd)^2 * (44)
firstNOafteressgsum <- sum(firstNOafteressg)
firstNOaftergss <- (na.omit(afterNO$NO) - firstNOafter)^2*44
firstNOaftergsssum <- sum(firstNOaftergss)
firstNOaftergv <- sqrt((firstNOaftergsssum + firstNOafteressgsum)/((45 * length(na.omit(afterNO$NO))-1)))

## Second

secondNObefore <- mean(before$NO.1)
secondNOafter <- mean(na.omit(afterNO$NO.1))

secondNOessg <- na.omit(before$NOsd.1)^2 * (44)
secondNOessgsum <- sum(secondNOessg)
secondNOgss <- (na.omit(before$NO.1) - secondNObefore)^2*44
secondNOgsssum <- sum(secondNOgss)
secondNOgv <- sqrt((secondNOgsssum + secondNOessgsum)/((45 * length(na.omit(before$NO.1))-1)))

secondNOafteressg <- na.omit(afterNO$NOsd.1)^2 * (44)
secondNOafteressgsum <- sum(secondNOafteressg)
secondNOaftergss <- (na.omit(afterNO$NO.1) - secondNOafter)^2*44
secondNOaftergsssum <- sum(secondNOaftergss)
secondNOaftergv <- sqrt((secondNOaftergsssum + secondNOafteressgsum)/((45 * length(na.omit(afterNO$NO.1))-1)))

## Third

thirdNObefore <- mean(na.omit(before$NO.2))
thirdNOafter <- mean(na.omit(afterNO$NO.2))

thirdNOessg <- na.omit(before$NOsd.2)^2 * (44)
thirdNOessgsum <- sum(thirdNOessg)
thirdNOgss <- (na.omit(before$NO.2) - thirdNObefore)^2*44
thirdNOgsssum <- sum(thirdNOgss)
thirdNOgv <- sqrt((thirdNOgsssum + thirdNOessgsum)/((45 * length(na.omit(before$NO.2))-1)))

thirdNOafteressg <- na.omit(afterNO$NOsd.2)^2 * (44)
thirdNOafteressgsum <- sum(thirdNOafteressg)
thirdNOaftergss <- (na.omit(afterNO$NO.2) - thirdNOafter)^2*44
thirdNOaftergsssum <- sum(thirdNOaftergss)
thirdNOaftergv <- sqrt((thirdNOaftergsssum + thirdNOafteressgsum)/((45 * length(na.omit(afterNO$NO.2))-1)))

## NO2 

## First

firstNO2before <- mean(na.omit(before$NO2))
firstNO2after <- mean(na.omit(after$NO2))

firstNO2essg <- na.omit(before$NO2.sd)^2 * (44)
firstNO2essgsum <- sum(firstNO2essg)
firstNO2gss <- (na.omit(before$NO2) - firstNO2before)^2*44
firstNO2gsssum <- sum(firstNO2gss)
firstNO2gv <- sqrt((firstNO2gsssum + firstNO2essgsum)/((45 * length(na.omit(before$NO2))-1)))

firstNO2afteressg <- na.omit(after$NO2.sd)^2 * (44)
firstNO2afteressgsum <- sum(firstNO2afteressg)
firstNO2aftergss <- (na.omit(after$NO2) - firstNO2after)^2*44
firstNO2aftergsssum <- sum(firstNO2aftergss)
firstNO2aftergv <- sqrt((firstNO2aftergsssum + firstNO2afteressgsum)/((45 * length(na.omit(after$NO2))-1)))

## Second

secondNO2before <- mean(na.omit(before$NO2.1))
secondNO2after <- mean(na.omit(after$NO2.1))

secondNO2essg <- na.omit(before$NO2.sd.1)^2 * (44)
secondNO2essgsum <- sum(secondNO2essg)
secondNO2gss <- (na.omit(before$NO2.1) - secondNO2before)^2*44
secondNO2gsssum <- sum(secondNO2gss)
secondNO2gv <- sqrt((secondNO2gsssum + secondNO2essgsum)/((45 * length(na.omit(before$NO2.1))-1)))

secondNO2afteressg <- na.omit(after$NO2.sd.1)^2 * (44)
secondNO2afteressgsum <- sum(secondNO2afteressg)
secondNO2aftergss <- (na.omit(after$NO2.1) - secondNO2after)^2*44
secondNO2aftergsssum <- sum(secondNO2aftergss)
secondNO2aftergv <- sqrt((secondNO2aftergsssum + secondNO2afteressgsum)/((45 * length(na.omit(after$NO2.1))-1)))

## Third

thirdNO2before <- mean(na.omit(before$NO2.2))
thirdNO2after <- mean(na.omit(after$NO2.2))

thirdNO2essg <- na.omit(before$NO2.sd.2)^2 * (44)
thirdNO2essgsum <- sum(thirdNO2essg)
thirdNO2gss <- (na.omit(before$NO2.2) - thirdNO2before)^2*44
thirdNO2gsssum <- sum(thirdNO2gss)
thirdNO2gv <- sqrt((thirdNO2gsssum + thirdNO2essgsum)/((45 * length(na.omit(before$NO2.2))-1)))

thirdNO2afteressg <- na.omit(after$NO2.sd.2)^2 * (44)
thirdNO2afteressgsum <- sum(thirdNO2afteressg)
thirdNO2aftergss <- (na.omit(after$NO2.2) - thirdNO2after)^2*44
thirdNO2aftergsssum <- sum(thirdNO2aftergss)
thirdNO2aftergv <- sqrt((thirdNO2aftergsssum + thirdNO2afteressgsum)/((45 * length(na.omit(after$NO2.2))-1)))

## HONO 

## First

firstHONObefore <- mean(na.omit(before$HONO))
firstHONOafter <- mean(na.omit(after$HONO))

firstHONOessg <- na.omit(before$HONO.sd)^2 * (44)
firstHONOessgsum <- sum(firstHONOessg)
firstHONOgss <- (na.omit(before$HONO) - firstHONObefore)^2*44
firstHONOgsssum <- sum(firstHONOgss)
firstHONOgv <- sqrt((firstHONOgsssum + firstHONOessgsum)/((45 * length(na.omit(before$HONO))-1)))

firstHONOafteressg <- na.omit(after$HONO.sd)^2 * (44)
firstHONOafteressgsum <- sum(firstHONOafteressg)
firstHONOaftergss <- (na.omit(after$HONO) - firstHONOafter)^2*44
firstHONOaftergsssum <- sum(firstHONOaftergss)
firstHONOaftergv <- sqrt((firstHONOaftergsssum + firstHONOafteressgsum)/((45 * length(na.omit(after$HONO))-1)))

## Second

secondHONObefore <- mean(na.omit(before$HONO.1))
secondHONOafter <- mean(na.omit(after$HONO.1))

secondHONOessg <- na.omit(before$HONO.sd.1)^2 * (44)
secondHONOessgsum <- sum(secondHONOessg)
secondHONOgss <- (na.omit(before$HONO.1) - secondHONObefore)^2*44
secondHONOgsssum <- sum(secondHONOgss)
secondHONOgv <- sqrt((secondHONOgsssum + secondHONOessgsum)/((45 * length(na.omit(before$HONO.1))-1)))

secondHONOafteressg <- na.omit(after$HONO.sd.1)^2 * (44)
secondHONOafteressgsum <- sum(secondHONOafteressg)
secondHONOaftergss <- (na.omit(after$HONO.1) - secondHONOafter)^2*44
secondHONOaftergsssum <- sum(secondHONOaftergss)
secondHONOaftergv <- sqrt((secondHONOaftergsssum + secondHONOafteressgsum)/((45 * length(na.omit(after$HONO.1))-1)))

## Third

thirdHONObefore <- mean(na.omit(before$HONO.2))
thirdHONOafter <- mean(na.omit(after$HONO.2))

thirdHONOessg <- na.omit(before$HONO.sd.2)^2 * (44)
thirdHONOessgsum <- sum(thirdHONOessg)
thirdHONOgss <- (na.omit(before$HONO.2) - thirdHONObefore)^2*44
thirdHONOgsssum <- sum(thirdHONOgss)
thirdHONOgv <- sqrt((thirdHONOgsssum + thirdHONOessgsum)/((45 * length(na.omit(before$HONO.2))-1)))

thirdHONOafteressg <- na.omit(after$HONO.sd.2)^2 * (44)
thirdHONOafteressgsum <- sum(thirdHONOafteressg)
thirdHONOaftergss <- (na.omit(after$HONO.2) - thirdHONOafter)^2*44
thirdHONOaftergsssum <- sum(thirdHONOaftergss)
thirdHONOaftergv <- sqrt((thirdHONOaftergsssum + thirdHONOafteressgsum)/((45 * length(na.omit(after$HONO.2))-1)))

## Find the differences for each run

## NO difference

nodifferencefirst <- firstNOafter - firstNObefore
nodifferencefirstsd <- sqrt(firstNOgv^2 + firstNOaftergv^2)

nodifferencesecond <- secondNOafter - secondNObefore
nodifferencesecondsd <- sqrt(secondNOgv^2 + secondNOaftergv^2)

nodifferencethird <- thirdNOafter - thirdNObefore
nodifferencethirdsd <- sqrt(thirdNOgv^2 + thirdNOaftergv^2)

## NO2 difference

NO2differencefirst <- firstNO2after - firstNO2before
NO2differencefirstsd <- sqrt(firstNO2gv^2 + firstNO2aftergv^2)

NO2differencesecond <- secondNO2after - secondNO2before
NO2differencesecondsd <- sqrt(secondNO2gv^2 + secondNO2aftergv^2)

NO2differencethird <- thirdNO2after - thirdNO2before
NO2differencethirdsd <- sqrt(thirdNO2gv^2 + thirdNO2aftergv^2)

## HONO difference

honodifferencefirst <- firstHONOafter - firstHONObefore
honodifferencefirstsd <- sqrt(firstHONOgv^2 + firstHONOaftergv^2)

honodifferencesecond <- secondHONOafter - secondHONObefore
honodifferencesecondsd <- sqrt(secondHONOgv^2 + secondHONOaftergv^2)

honodifferencethird <- thirdHONOafter - thirdHONObefore
honodifferencethirdsd <- sqrt(thirdHONOgv^2 + thirdHONOaftergv^2)

## NO ratio

noratiofirst <- firstNOafter/firstNObefore
noratiofirstsd <- sqrt((firstNOgv/firstNObefore)^2 + (firstNOaftergv/firstNOafter)^2)

noratiosecond <- secondNOafter/secondNObefore
noratiosecondsd <- sqrt((secondNOgv/secondNObefore)^2 + (secondNOaftergv/secondNOafter)^2)

noratiothird <- thirdNOafter/thirdNObefore
noratiothirdsd <- sqrt((thirdNOgv/thirdNObefore)^2 + (thirdNOaftergv/thirdNOafter)^2)

## NO2 ratio

NO2ratiofirst <- firstNO2after/firstNO2before
NO2ratiofirstsd <- sqrt((firstNO2gv/firstNO2before)^2 + (firstNO2aftergv/firstNO2after)^2)

NO2ratiosecond <- secondNO2after/secondNO2before
NO2ratiosecondsd <- sqrt((secondNO2gv/secondNO2before)^2 + (secondNO2aftergv/secondNO2after)^2)

NO2ratiothird <- thirdNO2after/thirdNO2before
NO2ratiothirdsd <- sqrt((thirdNO2gv/thirdNO2before)^2 + (thirdNO2aftergv/thirdNO2after)^2)

## HONO ratio

honoratiofirst <- firstHONOafter/firstHONObefore
honoratiofirstsd <- sqrt((firstHONOgv/firstHONObefore)^2 + (firstHONOaftergv/firstHONOafter)^2)

honoratiosecond <- secondHONOafter/secondHONObefore
honoratiosecondsd <- sqrt((secondHONOgv/secondHONObefore)^2 + (secondHONOaftergv/secondHONOafter)^2)

honoratiothird <- thirdHONOafter/thirdHONObefore
honoratiothirdsd <- sqrt((thirdHONOgv/thirdHONObefore)^2 + (thirdHONOaftergv/thirdHONOafter)^2)

## Total

## NO

totalNO <- mean(c(nodifferencefirst, nodifferencesecond, nodifferencethird))

essg <- sum(c(nodifferencefirstsd, nodifferencesecondsd, nodifferencethirdsd)^2*5)
gss <- sum((c(nodifferencefirst, nodifferencesecond, nodifferencethird)-totalNO)^2*5)
GV <- sqrt((essg+gss)/(3*6-1))
  
totalNOsd <- GV
  
totalNOratio <- mean(c(noratiofirst, noratiosecond, noratiothird))

essg <- sum(c(noratiofirstsd, noratiosecondsd, noratiothirdsd)^2*5)
gss <- sum((c(noratiofirst, noratiosecond, noratiothird)-totalNOratio)^2*5)
GV <- sqrt((essg+gss)/(3*6-1))

totalNOratiosd <- GV

## NO2

totalNO2 <- mean(c(NO2differencefirst, NO2differencesecond, NO2differencethird))  

essg <- sum(c(NO2differencefirstsd, NO2differencesecondsd, NO2differencethirdsd)^2*5)
gss <- sum((c(NO2differencefirst, NO2differencesecond, NO2differencethird)-totalNO2)^2*5)
GV <- sqrt((essg+gss)/(3*6-1))

totalNO2sd <- GV

totalNO2ratio <- mean(c(NO2ratiofirst, NO2ratiosecond, NO2ratiothird))

essg <- sum(c(NO2ratiofirstsd, NO2ratiosecondsd, NO2ratiothirdsd)^2*5)
gss <- sum((c(NO2ratiofirst, NO2ratiosecond, NO2ratiothird)-totalNO2ratio)^2*5)
GV <- sqrt((essg+gss)/(3*6-1))

totalNO2ratiosd <- GV

## HONO

totalHONO <- mean(c(honodifferencefirst, honodifferencesecond, honodifferencethird))  

essg <- sum(c(honodifferencefirstsd, honodifferencesecondsd, honodifferencethirdsd)^2*5)
gss <- sum((c(honodifferencefirst, honodifferencesecond, honodifferencethird)-totalHONO)^2*5)
GV <- sqrt((essg+gss)/(3*6-1))

totalHONOsd <- GV

totalHONOratio <- mean(c(honoratiofirst, honoratiosecond, honoratiothird))

essg <- sum(c(honoratiofirstsd, honoratiosecondsd, honoratiothirdsd)^2*5)
gss <- sum((c(honoratiofirst, honoratiosecond, honoratiothird)-totalHONOratio)^2*5)
GV <- sqrt((essg+gss)/(3*6-1))

totalHONOratiosd <- GV

## Export  
  
names <- c("First NO", "Firsts NO2", "First HONO", "Second NO", "Second NO2", "Second HONO", "Third NO", "Third NO2", "Third HONO", "Total NO", "Total NO2", "Total HONO")
average <- c(nodifferencefirst, NO2differencefirst, honodifferencefirst, nodifferencesecond, NO2differencesecond, honodifferencesecond, nodifferencethird, NO2differencethird, honodifferencethird, totalNO, totalNO2, totalHONO)
sdaverage <- c(nodifferencefirstsd, NO2differencefirstsd, honodifferencefirstsd, nodifferencesecondsd, NO2differencesecondsd, honodifferencesecondsd, nodifferencethirdsd, NO2differencethirdsd, honodifferencethirdsd, totalNOsd, totalNO2sd, totalHONOsd)
ratio <- c(noratiofirst, NO2ratiofirst, honoratiofirst, noratiosecond, NO2ratiosecond, honoratiosecond, noratiothird, NO2ratiothird, honoratiothird, totalNOratio, totalNO2ratio, totalHONOratio)
sdratio <- c(noratiofirstsd, NO2ratiofirstsd, honoratiofirstsd, noratiosecondsd, NO2ratiosecondsd, honoratiosecondsd, noratiothirdsd, NO2ratiothirdsd, honoratiothirdsd, totalNOratiosd, totalNO2ratiosd, totalHONOratiosd)

export <- data.frame(names, average, sdaverage, ratio, sdratio)
colnames(export) <- c('','Difference','Difference SD', 'Ratio', 'Ratio SD')

currentfile <- strsplit(f[i], '/')
end <- currentfile[[1]][8]
filename <- paste0('C:/Users/Zachary/Desktop/Finished_',end)
write.csv(export, file = filename, row.names = FALSE)

}