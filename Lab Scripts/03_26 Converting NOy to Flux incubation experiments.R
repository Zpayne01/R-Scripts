## If using this file for the incubation experiment make sure to change the file names and indexes properly ##

## Mass file sent from Ryan made before starting the incubation

file <- 'C:/Users/Zachary/Documents/Lab Data/Incubation Experiment/Incubation Masses.csv'
mass <- read.csv(file, stringsAsFactors = FALSE)
mass$Date <- as.POSIXlt(mass$Date, format = '%m/%d/%Y')
mass$Date <- format(mass$Date, '%m/%d/%Y')

## Use the chamber even file to determine the correct concentrations

## Choose flow File

file <- 'C:/Users/Zachary/Documents/Lab Data/Incubation Experiment/Incubation Flows.csv'
flow <- read.csv(file, stringsAsFactors = FALSE)
flow$Date <- as.POSIXlt(flow$Date, format = '%m/%d/%Y')
flow$Date <- format(flow$Date, '%m/%d/%Y')

## Create file directory

f <- list.files(path = "C:/Users/Zachary/Desktop/Acetylene/Humidity Corrected", full.names = TRUE, pattern = '.csv')

for (i in 1:length(f))  {	
  assign(f[i], read.csv(f[i]))
}

## Open the first flow file

start <- read.csv(f[1], header = TRUE, stringsAsFactors = FALSE)
start$time <- as.POSIXlt(start$time, format = "%m/%d/%Y %H:%M")
start$time <- format(start$time, '%m/%d/%Y')
flowmatch <- match(start$time[1], flow$Date)
massmatch <- match(start$time[1], mass$Date)

chamber <- substring(f[1], nchar(f[1])-4, nchar(f[1])-4 )

reloadtime <- read.csv(f[1], header = TRUE, stringsAsFactors = FALSE)
start$time <- reloadtime$time

## Flux is in units of ng/(g soil hr) or ug/(kg soil hr)

if (chamber == 'B'){

  start$no_ppb <- start$no_ppb /10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$no_ppb_sd <- start$no_ppb_sd/10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$no2_ppb <- start$no2_ppb/10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$no2_ppb_sd <- start$no2_ppb_sd/10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$hono_ppb <- start$hono_ppb/10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$hono_ppb_sd <- start$hono_ppb_sd/10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  
  filename <- paste0("C:/Users/Zachary/Desktop/Acetylene/flux/",substring(f[1], 55, 60),'flux', substring(f[1], 75, 84),'.csv')
  write.csv(start, filename, row.names = FALSE)
  
} else if (chamber == 'C'){
  
  start$no_ppb <- start$no_ppb /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$no_ppb_sd <- start$no_ppb_sd /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$no2_ppb <- start$no2_ppb /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$no2_ppb_sd <- start$no2_ppb_sd /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$hono_ppb <- start$hono_ppb /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$hono_ppb_sd <- start$hono_ppb_sd /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  
  filename <- paste0("C:/Users/Zachary/Desktop/Acetylene/flux/",substring(f[1], 55, 60),'flux', substring(f[1], 75, 84),'.csv')
  write.csv(start, filename, row.names = FALSE)
  
} else if (chamber == 'D'){
  
  start$no_ppb <- start$no_ppb /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$no_ppb_sd <- start$no_ppb_sd /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$no2_ppb <- start$no2_ppb /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$no2_ppb_sd <- start$no2_ppb_sd /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$hono_ppb <- start$hono_ppb /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$hono_ppb_sd <- start$hono_ppb_sd /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  
  filename <- paste0("C:/Users/Zachary/Desktop/Acetylene/flux/",substring(f[1], 55, 60),'flux', substring(f[1], 75, 84),'.csv')
  write.csv(start, filename, row.names = FALSE)
  
} else if (chamber == 'E'){
  
  start$no_ppb <- start$no_ppb /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$no_ppb_sd <- start$no_ppb_sd /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$no2_ppb <- start$no2_ppb /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$no2_ppb_sd <- start$no2_ppb_sd /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$hono_ppb <- start$hono_ppb /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$hono_ppb_sd <- start$hono_ppb_sd /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  
  filename <- paste0("C:/Users/Zachary/Desktop/Acetylene/flux/",substring(f[1], 55, 60),'flux', substring(f[1], 75, 84),'.csv')
  write.csv(start, filename, row.names = FALSE)
}


for (i in 29:32) {
    
  start <- read.csv(f[i], header = TRUE, stringsAsFactors = FALSE)
  start$time <- as.POSIXlt(start$time, format = "%Y-%m-%d %H:%M:%S")
  start$time <- format(start$time, '%m/%d/%Y')
  flowmatch <- match(start$time[1], flow$Date)
  massmatch <- match(start$time[1], mass$Date)
  
  chamber <- substring(f[i], 84, 84)
  
  reloadtime <- read.csv(f[i], header = TRUE, stringsAsFactors = FALSE)
  start$time <- reloadtime$time

if (chamber == 'B'){
  
  start$no_ppb <- start$no_ppb /10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$no_ppb_sd <- start$no_ppb_sd /10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$no2_ppb <- start$no2_ppb /10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$no2_ppb_sd <- start$no2_ppb_sd /10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$hono_ppb <- start$hono_ppb /10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  start$hono_ppb_sd <- start$hono_ppb_sd /10^9/23.96*flow$B[flowmatch]*14*10^9*60/mass$B[massmatch]
  
  filename <- paste0("C:/Users/Zachary/Desktop/Acetylene/flux/",substring(f[i], 55, 60),'flux', substring(f[i], 75, 84),'.csv')
  write.csv(start, filename, row.names = FALSE)
  
} else if (chamber == 'C'){
  
  start$no_ppb <- start$no_ppb /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$no_ppb_sd <- start$no_ppb_sd /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$no2_ppb <- start$no2_ppb /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$no2_ppb_sd <- start$no2_ppb_sd /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$hono_ppb <- start$hono_ppb /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  start$hono_ppb_sd <- start$hono_ppb_sd /10^9/23.96*flow$C[flowmatch]*14*10^9*60/mass$C[massmatch]
  
  filename <- paste0("C:/Users/Zachary/Desktop/Acetylene/flux/",substring(f[i], 55, 60),'flux', substring(f[i], 75, 84),'.csv')
  write.csv(start, filename, row.names = FALSE)
  
} else if (chamber == 'D'){
  
  start$no_ppb <- start$no_ppb /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$no_ppb_sd <- start$no_ppb_sd /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$no2_ppb <- start$no2_ppb /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$no2_ppb_sd <- start$no2_ppb_sd /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$hono_ppb <- start$hono_ppb /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  start$hono_ppb_sd <- start$hono_ppb_sd /10^9/23.96*flow$D[flowmatch]*14*10^9*60/mass$D[massmatch]
  
  filename <- paste0("C:/Users/Zachary/Desktop/Acetylene/flux/",substring(f[i], 55, 60),'flux', substring(f[i], 75, 84),'.csv')
  write.csv(start, filename, row.names = FALSE)
  
} else if (chamber == 'E'){
  
  start$no_ppb <- start$no_ppb /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$no_ppb_sd <- start$no_ppb_sd /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$no2_ppb <- start$no2_ppb /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$no2_ppb_sd <- start$no2_ppb_sd /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$hono_ppb <- start$hono_ppb /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  start$hono_ppb_sd <- start$hono_ppb_sd /10^9/23.96*flow$E[flowmatch]*14*10^9*60/mass$E[massmatch]
  
  filename <- paste0("C:/Users/Zachary/Desktop/Acetylene/flux/",substring(f[i], 55, 60),'flux', substring(f[i], 75, 84),'.csv')
  write.csv(start, filename, row.names = FALSE)
}
}

