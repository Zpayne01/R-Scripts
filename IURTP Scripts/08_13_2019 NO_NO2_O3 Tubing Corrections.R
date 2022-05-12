#### File Upload ####

#file <- file.choose()
#csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

files <- list.files('D:/Surface 3/Main Folder/One Drive/IURTP/NOx/AQD NOx New CE/Corrections v3/NOx Ozone and Met')

for (i in 1:length(files)) {

csv = read.csv(paste0('D:/Surface 3/Main Folder/One Drive/IURTP/NOx/AQD NOx New CE/Corrections v3/NOx Ozone and Met/', files[i]), stringsAsFactors = FALSE)

#### Absolute humitidy correction - Use if necessary

csv$no_ave = csv$no_ave/(-9.3E-3*csv$absolute_humidity + 1)
csv$no2_ave = csv$no2_ave/(-9.3E-3*csv$absolute_humidity + 1)

#### Residence Time of the Tubing Calculcation ####

T_length = 500 #centimeters
T_id = 0.47625 #centimeters
flow = 1856 #SCCM
t_tube = (T_id/2)^2*pi*T_length/flow * 60
print(t_tube)

t_instrument = 6.2
t_time = t_tube + t_instrument

#### k_NO+O3 equation based on values from the JPL Document ####

csv$k_NO_O3 <- 3.0E-12*exp(-1500/(csv$air_temp_ave+273.15))
csv$O3_ppb[csv$O3_ppb < 0] = .100
csv$O3_ppb[is.na(csv$O3_ppb)] = .100

#### NO chamber concentration based on modifications to bimolecular integrated rate law ####
### Concentration is in ppb and Mixing Ratio Factor is the value that converts from ppb to molecules cm-3 based on T and P ####

NO_m <- csv$no_ave * csv$Mixing.Ratio.Factor
O3_m <- csv$O3_ppb * csv$Mixing.Ratio.Factor
NO2_m <- csv$no2_ave * csv$Mixing.Ratio.Factor

NO_ch <- (NO_m - O3_m)/(1-O3_m/NO_m*exp(csv$k_NO_O3*(t_time)*(NO_m-O3_m)))

##NO_ch <- (O3_m - NO_m)/(O3_m/NO_m*exp(csv$k_NO_O3*t_tube*(NO_m-O3_m))-1)
O3_ch <- O3_m+(NO_ch-NO_m)
NO2_ch <- NO2_m-(NO_ch-NO_m)

#### NO concentration back to ppb ####

export <- data.frame(datetime = csv$datetime,
                     NO_ave_ppb = NO_ch/csv$Mixing.Ratio.Factor,
                     NO_sd_ppb_measurement = csv$no_sd_meas,
                     NO_sd_ppb_instrument = csv$no_sd_inst,
                     NO2_ave_ppb = NO2_ch/csv$Mixing.Ratio.Factor,
                     NO2_sd_ppb_measurement = csv$no2_sd_meas,
                     NO2_sd_ppb_instrumental = csv$no2_sd_inst,
                     O3_ave_ppb = O3_ch/csv$Mixing.Ratio.Factor,
                     O3_sd_measurement = csv$O3_sd,
                     L.mol1 = csv$L.mol..1,
                     Mixing.Ratio.Factor = csv$Mixing.Ratio.Factor,
                     air_temp_ave = csv$air_temp_ave,
                     jNO2 = csv$jNO2
                     )

name <- strsplit(files[i], ' ')[[1]][4]                       
write.csv(export, file = paste0('C:/Users/zacpayne/Desktop/NO_NO2_O3 Tubing Corrected ', name), row.names = FALSE)

}
