#### Used to find flux in mg N m(-2) hr(-1) ####

#### Subtracting by blank chamber instead of ambient

directory <- 'D:/Surface 3/Main Folder/One Drive/IURTP/NOx/AQD NOx New CE/Corrections v4/Tubing Corrected'

files <- list.files(directory)

csv1 <- read.csv(paste(directory, files[1], sep = '/'), stringsAsFactors = FALSE)

for (i in 3:length(files)) {

#file1 <- file.choose()
#file2 <- file.choose()

#csv1 <- read.csv(file1, stringsAsFactors = FALSE)

csv2 <- read.csv(paste(directory, files[i], sep = '/'), stringsAsFactors = FALSE)

head(csv1)
head(csv2)

matched <- match(csv2$datetime, csv1$datetime)

#### Gas Phase Source Constants ####

csv2$k_NO_O3 <- 3.0E-12*exp(-1500/(csv2$air_temp_ave+273.15))
csv2$jNO2[csv$jNO2 < 0] = 0

cf = 0.2368 # based on transmissivity through the walls
V = 43300 # cm3

#### Gas Phase Source Conversions ####

#csv2$Mixing.Ratio.Factor <- 1/1E9/csv2$L.mol1/1000*6.02214E23

NO <- csv2$NO_ave_ppb*csv2$Mixing.Ratio.Factor
NO2 <- csv2$NO2_ave_ppb*csv2$Mixing.Ratio.Factor
O3 <- csv2$O3_ave_ppb*csv2$Mixing.Ratio.Factor

S_NO <- V*(csv2$jNO2*cf*NO2 - csv2$k_NO_O3*NO*O3)/6.022E23 #mol
S_NO2 <- -S_NO
S_O3 <- S_NO

#### Fluxes ####

flow = (2.7 + 20.0)/60 #L s-1
A = 0.106535 #m-2

delta_no <- (csv2$NO_ave_ppb/1E9/csv2$L.mol1 - csv1$NO_ave_ppb[matched]/1E9/csv1$L.mol1[matched])
no_flux <- (delta_no*flow-S_NO)/A # mol NO s-1 m-2
no_flux <- no_flux*14.0*1E9 # ng N s-1 m-2

delta_no2 <- (csv2$NO2_ave_ppb/1E9/csv2$L.mol1 - csv1$NO2_ave_ppb[matched]/1E9/csv1$L.mol1[matched])
no2_flux <- (delta_no2*flow-S_NO2)/A # mol NO2 s-1 m-2
no2_flux <- no2_flux*14.0*1E9 #ng N s-1 m-2

# delta_o3 <- (csv2$O3_ave_ppb/1E9/csv2$L.mol1 - csv1$O3_ave_ppb[matched]/1E9/csv1$L.mol1[matched])
# o3_flux <- delta_o3*flow/A # mol O3 min-1 m-2
# o3_flux <- o3_flux/60*1000 # mmol O3 s-1 m-2

#### Flux Deviations ####

delta_no_sd <- sqrt((csv2$NO_sd_ppb_instrument/1E9/csv2$L.mol1)^2+(csv1$NO_sd_ppb_instrument[matched]/1E9/csv1$L.mol1[matched])^2)
no_flux_sd <- delta_no_sd*flow/A/60*14.0*1E9

delta_no2_sd <- sqrt((csv2$NO2_sd_ppb_instrument/1E9/csv2$L.mol1)^2+(csv1$NO2_sd_ppb_instrument[matched]/1E9/csv1$L.mol1[matched])^2)
no2_flux_sd <- delta_no2_sd*flow/A/60*14.0*1E9

# delta_o3_sd <- sqrt((csv2$O3_sd_ppb_instrument/1E9/csv2$L.mol1)^2+(csv1$O3_sd_ppb_instrument[matched]/1E9/csv1$L.mol1[matched])^2)
# o3_flux_sd <- o3_flux/60*1000

export <- data.frame(
  datetime = csv2$datetime, 
  no_flux_ng_N_m2_s = no_flux, 
  no_flux_sd = no_flux_sd, 
  no2_flux_ng_N_m2_s = no2_flux, 
  no2_flux_sd = no2_flux_sd)

fileend <- strsplit(files[i], ' ')[[1]][[4]]
filename = 'C:/Users/zacpayne/Desktop/NOx Flux'
write.csv(export, paste(filename, fileend), row.names = FALSE)

}
