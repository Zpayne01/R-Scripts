# Pick the file that you are taking the diurnal average of

file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

# Convert the time class into time

csv$Time <- as.POSIXlt(csv$Time, format = "%Y-%m-%d %H:%M:%S")
csv$hour <- csv$Time$hour

# Parameterize the NO from the equation in Forkel et al. 2006 and my IU parameterizations

csv$Forkel_NO_parameterize <- 18 * exp(0.14 * csv$soil_temp)
csv$IU_NO_parameterize <- 2.95 * exp(0.08 * csv$soil_temp)
# Chamber C csv$IU_NO_parameterize <- 0.92 * exp(0.12 * csv$soil_temp)
# Chamber D csv$IU_NO_parameterize <- 1.94 * exp(0.09 * csv$soil_temp)

# Take the average of each species (NO and NO2) along with the parameterizations

agg_no <- aggregate(no_ave ~ hour, csv, mean)
agg_no2 <- aggregate(no2_ave ~ hour, csv, mean)
agg_soil_temp <- aggregate(soil_temp ~ hour, csv, mean)
agg_Forkel_no_parameterize <- aggregate(Forkel_NO_parameterize ~ hour, csv, mean)
agg_IU_no_parameterize <- aggregate(IU_NO_parameterize ~ hour, csv, mean)

agg_no_sd <- aggregate(no_ave ~ hour, csv, sd)
agg_no2_sd <- aggregate(no2_ave ~ hour, csv, sd)
agg_soil_temp_sd <- aggregate(soil_temp ~ hour, csv, sd)
agg_Forkel_no_parameterize_sd <- aggregate(Forkel_NO_parameterize ~ hour, csv, sd)
agg_IU_no_parameterize_sd <- aggregate(IU_NO_parameterize ~ hour, csv, sd)

combined_agg <- data.frame(agg_no, agg_no_sd[2], agg_no2[2], agg_no2_sd[2], agg_soil_temp[2], agg_soil_temp_sd[2],
                           agg_Forkel_no_parameterize[2], agg_Forkel_no_parameterize_sd[2], agg_IU_no_parameterize[2],
                           agg_IU_no_parameterize_sd[2])
colnames(combined_agg) <- c("hour", "NO_Flux_ave_(ug m-1 h-1)", "NO_Flux_sd", "NO2_Flux_ave_(ug m-1 h-1)", "NO2_Flux_sd", "Soil_Temp_(C)",
                            "Soil_Temp_sd", "NO_Emissions_Forkel_(ug m-1 h-1)", "NO_Forkel_sd", "NO_Emissions_IU_(ug m-1 h-1)", "NO_IU_sd")

# Save the csv and the aggregate

csvfilename <- "D:/Surface 3/Main Folder/One Drive/IURTP/12_06_18 Modeling Work/NO Flux with Parameterizations (No Rain)/Insert Chamber with parameterizations full.csv"
aggfilename <- "D:/Surface 3/Main Folder/One Drive/IURTP/12_06_18 Modeling Work/NO Flux with Parameterizations (No Rain)/Insert Chamber with parameterizations diurnal.csv"

write.csv(csv, csvfilename, row.names = FALSE)
write.csv(combined_agg, aggfilename, row.names = FALSE)