f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/CO2", pattern = "_", full.names = TRUE)

setwd('C:/Users/Zachary/OneDrive/IUFRP/CO2')

for (i in 1:length(f))
{ assign(f[i], read.csv(f[i], header = TRUE))	}

for (i in 1:length(f))
{ csv <- paste0(f[i],"/",list.files(f[i], pattern = "CO2_amb.csv"))
CO2 <- read.csv(csv, header = TRUE)

{
  
  colnames(CO2) <- c('CO2', 'H2O', 'datetime')
  datetime <- strptime((CO2$datetime), format = '%m/%d/%Y %H:%M')
  
  CO2$datetime <- datetime
  
  CO2$datetimefloor <- as.POSIXct(floor(as.numeric(CO2$datetime) / (5 * 60)) * (5 * 60), origin='1970-01-01')
  CO2meanfloor <- aggregate(CO2 ~ datetimefloor, CO2, mean)
  
  CO2$datetimefloor <- as.POSIXct(floor(as.numeric(CO2$datetime) / (5 * 60)) * (5 * 60), origin='1970-01-01')
  CO2sdfloor <- aggregate(CO2 ~ datetimefloor, CO2, sd)
  
  CO2$datetimefloor <- as.POSIXct(floor(as.numeric(CO2$datetime) / (5 * 60)) * (5 * 60), origin='1970-01-01')
  H2Omeanfloor <- aggregate(H2O ~ datetimefloor, CO2, mean)
  
  CO2$datetimefloor <- as.POSIXct(floor(as.numeric(CO2$datetime) / (5 * 60)) * (5 * 60), origin='1970-01-01')
  H2Osdfloor <- aggregate(H2O ~ datetimefloor, CO2, sd)
  
  stats <- cbind(CO2meanfloor, CO2sdfloor$CO2, H2Omeanfloor$H2O, H2Osdfloor$H2O)
  colnames(stats) <- c('datetime', 'ave_CO2', 'sd_CO2', 'ave_H2O', 'sd_H2O')
  
  filename <- "/CO2_Stats_amb.csv"
  write.table(stats, file = paste0(f[i],filename), append = FALSE, sep = ",", row.names = FALSE)
  
}	
}
