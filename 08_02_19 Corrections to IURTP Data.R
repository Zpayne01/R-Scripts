file1 <- file.choose()
file1
ozone <- read.csv(file1, header = TRUE, stringsAsFactors = FALSE)

file2 <- file.choose()
file2
NOx <- read.csv(file2, header = TRUE, stringsAsFactors = FALSE)

## Set Ozone Time as POSIXct

ozone$datetime <- as.POSIXct(ozone$datetime, format = '%m/%d/%Y %H:%M')
NOx$datetime <- as.POSIXct(NOx$datetime, format = '%m/%d/%T %H:%M')

## Find the first NOx Point for Correction

firstozone <- match(NOx$datetime[1], ozone$datetime)
ozone <- ozone[firstozone:nrow(ozone),]

## Matching ozone to NOx

ozone$floor_time <- floor_date(ozone$datetime, unit = 'hour')
matchingmeas <- match(ozone$floor_time, NOx$datetime)

## Tubing residence time

ttube = 16

## Mixing ratio factor

MixingRatio <- data.frame(time = NOx$datetime, Presure_Pa = NOx$Air.Pressure * 100, Temperate_K = NOx$Temperature +273.15)
Volume = 1E-6
Rconstant = 1.38E-23

MixingRatio$mixingratiofactor <- 1/(MixingRatio$Presure_Pa*Volume/(Rconstant*MixingRatio$Temperate_K))*10^9

## k value calculation
    
Afactor = 3.0E-12 #E/R according to JPL Document 
E_R = 1500 #E/R according to JPL Document 

NOx$k_no_o3 <- Afactor*exp(-E_R/(NOx$Temperature+273.15))

## Converting ambient NOx ppbv to molecules cm-3

NOx$mixingratiofactor <- MixingRatio$mixingratiofactor

NOx$no_ave_molec_cm3 <- NOx$no_ave/NOx$mixingratiofactor
NOx$no_sd_molec_cm3 <- NOx$no_sd/NOx$mixingratiofactor
NOx$no2_ave_molec_cm3 <- NOx$no2_ave/NOx$mixingratiofactor
NOx$no2_sd_molec_cm3 <- NOx$no2_sd/NOx$mixingratiofactor
NOx$hono_ave_molec_cm3 <- NOx$hono_ave/NOx$mixingratiofactor
NOx$hono_sd_molec_cm3 <- NOx$hono_sd/NOx$mixingratiofactor

## Converting Ozone

mNOt <- ((ozone$ave.molec.cm.3..w.o.0. - NOx$no_ave_molec_cm3[matchingmeas])) * NOx$no_ave_molec_cm3[matchingmeas]/(ozone$ave.molec.cm.3..w.o.0. * 10^(-NOx$k_no_o3[matchingmeas]*(ozone$ave.molec.cm.3..w.o.0.- NOx$no_ave_molec_cm3[matchingmeas])*ttube)- NOx$no_ave_molec_cm3[matchingmeas])
mO3t <- ((NOx$no_ave[matchingmeas] - ozone$ave) * ozone$ave)/(NOx$no_ave[matchingmeas] * 10^(-NOx$k_no_o3[matchingmeas]*(NOx$no_ave[matchingmeas] - ozone$ave)*ttube) - ozone$ave)

o1 <- ozone$ave.molec.cm.3..w.o.0.[2]
no1 <- NOx$no_ave_molec_cm3[1]
k1 <- NOx$k_no_o3[1]

mO3t <- (no1 - o1) * o1/(no1 * 10^(-k1*(no1-o1)*ttube)-o1)
mNOt <- (o1 - no1) * no1/(o1 * 10^(-k1*(o1-no1)*ttube)-no1)

mNOt2 <- (no1*exp(k1*(o1-no1)*16)*(-no1/o1+1))/(1-no1/o1*exp(k1*(o1-no1)*16))
mO3t22 <- (no1-o1)/(no1/o1*exp(k1*(o1-no1)*16)-1)

mO3t2 <- mNOt2 - no1 + o1 
mNOt22 <- mO3t22 - o1 + no1

## NO in chamber

mNOmn = k1 * mNOt2 * mO3t2 * 118 + mNOt2

mNOmn/2.46e10

(no1 - o1)/(1 - o1/no1 * exp(k1 * 118 * (no1 - o1)))
(no1-o1)/(no1/o1*exp(k1*(o1-no1)*5)-1)

no2 <- NOx$no2_ave_molec_cm3[1]
jNO2 <- 0.001788

43000 * ((no2 * jNO2) - (k1 * mNOt22 * mO3t22))

o <- 20
no <- 4
no2 <- 5
k <- 4.31E-4
jNO2 <- 0

0.041 * (-k * no * o) 

SA = 0.096 #m2
V = 0.041 #m3
Q = 0.001 #m3 s-1
NOcham = 2.7 #ppb 
NOamb = 5 #ppb
O3 = 50 #ppb
k <- 4.31E-4 #ppb-1 s-1

(Q * (NOcham - NOamb) - V * (-k*O3*NOcham))/SA * 44.64
