
f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/CO2", pattern = "_", full.names = TRUE)

length(f)

for (i in 1:length(f))
{	assign(f[i], read.csv(f[i], header = FALSE))	}

folder <- f[1]
file <- "/CO2_Stats_amb.csv"
names <- c("datetime", "ave_CO2", "sd_CO2", "ave_H2O", "sd_H2O")

start <- read.csv(paste0(folder, file), header = TRUE, stringsAsFactors = FALSE)
colnames(start) <- names

for (i in 2:length(f))
{ add <- read.csv(paste0(f[i],file), header = TRUE, stringsAsFactors = FALSE)
colnames(add) <- names
add <- add[1:5]
start <- rbind(start, add) }

start <- start[start$ave_CO2 > 0,] 
start <- start[start$sd_CO2 > 0,]
start <- start[start$ave_H2O > 0,]
start <- start[start$sd_H2O > 0,]

write.csv(start, file = "C:/Users/Zachary/OneDrive/IUFRP/CO2/CompleteCO2.csv", row.names = FALSE)

folder <- f[1]
file <- "/CO2_Stats_chambers.csv"

start <- read.csv(paste0(folder, file), header = TRUE, stringsAsFactors = FALSE)

for (i in 2:length(f))
{ add <- read.csv(paste0(f[i],file), header = TRUE, stringsAsFactors = FALSE)
colnames(add) <- names
add <- add[1:3]
start <- rbind(start, add) }

start <- start[start$ave > 0,]  

write.csv(start, file = "C:/Users/Zachary/OneDrive/IUFRP/CO2/Extra/Complete_CO2_chambers.csv", row.names = FALSE)

## Chamber Measurements

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/CO2", pattern = "_", full.names = TRUE)

length(f)

for (i in 1:length(f))
{	assign(f[i], read.csv(f[i], header = FALSE))	}

folder <- f[1]
file <- "/CO2_Stats_chambers.csv"
names <- c("datetime_a", "ave_CO2_a", "sd_CO2_a", "ave_H2O_a", "sd_H2O_a", "datetime_b", "ave_CO2_b", "sd_CO2_b", "ave_H2O_b", "sd_H2O_b", "datetime_c", 
          "ave_CO2_c", "sd_CO2_c", "ave_H2O_c", "sd_H2O_c", "datetime_d", "ave_CO2_d", "sd_CO2_d", "ave_H2O_d", "sd_H2O_d")

start <- read.csv(paste0(folder, file), header = TRUE, stringsAsFactors = FALSE)
colnames(start) <- names
starta <- start[1:5]
startb <- start[6:10]
startc <- start[11:15]
startd <- start[16:20]

for (i in 2:length(f))
{ add <- read.csv(paste0(f[i],file), header = TRUE, stringsAsFactors = FALSE)
colnames(add) <- names
adda <- add[1:5]
addb <- add[6:10]
addc <- add[11:15]
addd <- add[16:20]

starta <- rbind(starta, adda)
startb <- rbind(startb, addb)
startc <- rbind(startc, addc)
startd <- rbind(startd, addd) }

starta <- starta[starta$ave_CO2_a > 0,]
startb <- startb[startb$ave_CO2_b > 0,]
startc <- startc[startc$ave_CO2_c > 0,]
startd <- startd[startd$ave_CO2_d > 0,]

write.csv(starta, file = "C:/Users/Zachary/OneDrive/IUFRP/CO2/Extra/Complete_CO2_a.csv", row.names = FALSE)
write.csv(startb, file = "C:/Users/Zachary/OneDrive/IUFRP/CO2/Extra/Complete_CO2_b.csv", row.names = FALSE)
write.csv(startc, file = "C:/Users/Zachary/OneDrive/IUFRP/CO2/Extra/Complete_CO2_c.csv", row.names = FALSE)
write.csv(startd, file = "C:/Users/Zachary/OneDrive/IUFRP/CO2/Extra/Complete_CO2_d.csv", row.names = FALSE)