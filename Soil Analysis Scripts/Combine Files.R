## Combine all files for soil parameters ##

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Combined A", full.names = TRUE)

length(f)

for (i in 1:length(f))
	{	assign(f[i], read.csv(f[i]))
	}

start <- read.csv(f[1], header = FALSE, stringsAsFactors = FALSE)
keep <- c("V1", "V2", "V4", "V6", "V8", "V10")
start <- start[keep]
colnames(start) <- c('date', 'time', 'air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')


for (i in 2:length(f))
	{ add <- read.csv(f[i], header = FALSE, stringsAsFactors = FALSE)
	  add <- add[keep]
	  colnames(add) <- c('date', 'time', 'air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')
	  start <- rbind(start, add)
	}

data <- start
datetime <- paste(data$date, data$time)
keep <- c('air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')

## Remove points ## 
data[data < 0] <- 'NaN'

export <- cbind(datetime, data[keep])                                                                                              
colnames(export) <- c('datetime_a', 'air_temp_a', 'relative_humidity_a', 'soil_temp_a', 'soil_moisture_a')

file <- "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Complete A.csv"
write.csv(export, file, row.names = FALSE)

## Combine all files for soil parameters ##

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Combined B", full.names = TRUE)

length(f)

for (i in 1:length(f))
	{	assign(f[i], read.csv(f[i]))
	}

start <- read.csv(f[1], header = FALSE, stringsAsFactors = FALSE)
keep <- c("V1", "V2", "V4", "V6", "V8", "V10")
start <- start[keep]
colnames(start) <- c('date', 'time', 'air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')


for (i in 2:length(f))
	{ add <- read.csv(f[i], header = FALSE, stringsAsFactors = FALSE)
	  add <- add[keep]
	  colnames(add) <- c('date', 'time', 'air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')
	  start <- rbind(start, add)
	}

data <- start
datetime <- paste(data$date, data$time)
keep <- c('air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')

## Remove points ## 
data[data < 0] <- 'NaN'

export <- cbind(datetime, data[keep])                                                                                              
colnames(export) <- c('datetime_b', 'air_temp_b', 'relative_humidity_b', 'soil_temp_b', 'soil_moisture_b')

file <- "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Complete B.csv"
write.csv(export, file, row.names = FALSE)

## Combine all files for soil parameters ##

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Combined C", full.names = TRUE)

length(f)

for (i in 1:length(f))
	{	assign(f[i], read.csv(f[i]))
	}

start <- read.csv(f[1], header = FALSE, stringsAsFactors = FALSE)
keep <- c("V1", "V2", "V4", "V6", "V8", "V10")
start <- start[keep]
colnames(start) <- c('date', 'time', 'air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')


for (i in 2:length(f))
	{ add <- read.csv(f[i], header = FALSE, stringsAsFactors = FALSE)
	  add <- add[keep]
	  colnames(add) <- c('date', 'time', 'air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')
	  start <- rbind(start, add)
	}

data <- start
datetime <- paste(data$date, data$time)
keep <- c('air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')

## Remove points ## 
data[data < 0] <- 'NaN'

export <- cbind(datetime, data[keep])                                                                                              
colnames(export) <- c('datetime_c', 'air_temp_c', 'relative_humidity_c', 'soil_temp_c', 'soil_moisture_c')

file <- "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Complete C.csv"
write.csv(export, file, row.names = FALSE)

## Combine all files for soil parameters ##

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Combined D", full.names = TRUE)

length(f)

for (i in 1:length(f))
	{	assign(f[i], read.csv(f[i]))
	}

start <- read.csv(f[1], header = FALSE, stringsAsFactors = FALSE)
keep <- c("V1", "V2", "V4", "V6", "V8", "V10")
start <- start[keep]
colnames(start) <- c('date', 'time', 'air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')


for (i in 2:length(f))
	{ add <- read.csv(f[i], header = FALSE, stringsAsFactors = FALSE)
	  add <- add[keep]
	  colnames(add) <- c('date', 'time', 'air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')
	  start <- rbind(start, add)
	}

data <- start
datetime <- paste(data$date, data$time)
keep <- c('air_temp', 'relative_humidity', 'soil_temp', 'soil_moisture')

## Remove points ## 
data[data < 0] <- 'NaN'

export <- cbind(datetime, data[keep])                                                                                              
colnames(export) <- c('datetime_d', 'air_temp_d', 'relative_humidity_d', 'soil_temp_d', 'soil_moisture_d')

file <- "C:/Users/Zachary/OneDrive/IUFRP/Soil Parameters/Complete D.csv"
write.csv(export, file, row.names = FALSE)
