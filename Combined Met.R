## Combine all weather station files ##

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/Weather Station", pattern = "Table_Central", full.names = TRUE)

length(f)

for (i in 1:length(f))
	{	assign(f[i], read.csv(f[i], header = FALSE))	}

start <- read.csv(f[1], header = FALSE, stringsAsFactors = FALSE)
firstrow <- start[seq(1, nrow(start), 3),]
firstrow <- firstrow[-9:-ncol(start)]
secondrow <- start[seq(2, nrow(start), 3),]
secondrow <- secondrow[-9:-ncol(start)]
thirdrow <- start[seq(3, nrow(start), 3),]

complete <- cbind(firstrow, secondrow[-1:-2], thirdrow[-1:-2])
colnames(complete) <- c("Date", "Time", "Delete", "Broadband Flux", "Delete",
				"UV", "Delete", "Wind Speed", "Delete", "Wind Direction",
				"Delete", "Rain Accumulation", "Delete", "Rain Duration",
				"Delete", "Rain Intensity", "Delete", "RH", "Delete",
				"Air Pressure", "Delete", "Temperature")
keep <- c("Date", "Time", "Broadband Flux",
	    "UV", "Wind Speed", "Wind Direction",
	    "Rain Accumulation", "Rain Duration",
	    "Rain Intensity", "RH", "Air Pressure", "Temperature")
complete <- complete[keep]

for (i in 2:length(f))
	{ start <- read.csv(f[i], header = FALSE, stringsAsFactors = FALSE)
	  firstrow <- start[seq(1, nrow(start), 3),]
	  firstrow <- firstrow[-9:-ncol(start)]
	  secondrow <- start[seq(2, nrow(start), 3),]
	  secondrow <- secondrow[-9:-ncol(start)]
	  thirdrow <- start[seq(3, nrow(start), 3),]

	
	  nextcomplete <- cbind(firstrow, secondrow[-1:-2], thirdrow[-1:-2])
	  colnames(nextcomplete) <- c("Date", "Time", "Delete", "Broadband Flux", "Delete",
				"UV", "Delete", "Wind Speed", "Delete", "Wind Direction",
				"Delete", "Rain Accumulation", "Delete", "Rain Duration",
				"Delete", "Rain Intensity", "Delete", "RH", "Delete",
				"Air Pressure", "Delete", "Temperature")

	  nextcomplete <- nextcomplete[keep]
	  complete <- rbind(complete, nextcomplete)
	}

datetime <- paste(complete$Date, complete$Time)
complete <- complete[-1:-2]
export <- cbind(datetime, complete)

## Order based on datetime

newexport <- export[order(export$datetime),]

newexport$UV <- as.numeric(newexport$UV)
newexport$`Broadband Flux` <- as.numeric(newexport$`Broadband Flux`)
newexport$`Wind Speed` <- as.numeric(newexport$`Wind Speed`)
newexport$`Wind Direction` <- as.numeric(newexport$`Wind Direction`)
newexport$`Rain Accumulation` <- as.numeric(newexport$`Rain Accumulation`)
newexport$`Rain Duration` <- as.numeric(newexport$`Rain Duration`)
newexport$`Rain Intensity` <- as.numeric(newexport$`Rain Intensity`)
newexport$RH <- as.numeric(newexport$RH)
newexport$`Air Pressure` <- as.numeric(newexport$`Air Pressure`)
newexport$Temperature <- as.numeric(newexport$Temperature)
 
newexport$UV[newexport$UV < -32767] <- 272141
newexport$UV[newexport$UV < 0] <- 0
newexport$`Broadband Flux`[newexport$`Broadband Flux` < 0] <- 0

newexport$percent_UV <- newexport$UV/272141
newexport$percent_Broadband <- newexport$`Broadband Flux`/177450

newexport$absolute_humidity <- (6.112 * exp((17.67 * newexport$Temperature)/(newexport$Temperature + 243.5)) * newexport$RH *2.1674)/(273.15 + newexport$Temperature)
newexport$absolute_humidity[newexport$absolute_humidity < 0] <- 'NaN'

## Remove unnecesary points ##

newexport[newexport < 0] <- 'NaN' 

## Export the file ##


file <- "C:/Users/Zachary/OneDrive/IUFRP/Weather Station/Complete.csv"
write.csv(newexport, file, row.names = FALSE)

library(lubridate)

manipulate <- newexport

manipulate$datetime <- as.POSIXct(manipulate$datetime, format = "%m/%d/%Y %H:%M:%S")

manipulate$datetime <- floor_date(manipulate$datetime, unit = '15 minutes')

UVmeanfloor <- aggregate(UV ~ datetime, manipulate, mean)
Broadbandfloor <- aggregate(`Broadband Flux` ~ datetime, manipulate, mean)
Windspeedfloor <- aggregate(`Wind Speed` ~ datetime, manipulate, mean)

absolutehumidityfloor <- aggregate(as.numeric(absolute_humidity) ~ datetime, manipulate, mean)
write.csv(absolutehumidityfloor, file = "C:/Users/Zachary/OneDrive/IUFRP/Weather Station/AH.csv", row.names = FALSE)

