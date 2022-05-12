
f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/Ozone", pattern = "_", full.names = TRUE)

length(f)

for (i in 1:length(f))
	{	assign(f[i], read.csv(f[i], header = FALSE))	}

folder <- f[1]
file <- "/Ozone_Stats_amb.csv"
names <- c("datetime", "ave", "sd")

start <- read.csv(paste0(folder, file), header = TRUE, stringsAsFactors = FALSE)
colnames(start) <- names

for (i in 2:length(f))
	{ add <- read.csv(paste0(f[i],file), header = TRUE, stringsAsFactors = FALSE)
	 colnames(add) <- names
	 add <- add[1:3]
	 start <- rbind(start, add) }

start <- start[start$ave > 0,]  

write.csv(start, file = "C:/Users/Zachary/OneDrive/IUFRP/Ozone/Extra/Complete_Ozone.csv", row.names = FALSE)

folder <- f[1]
file <- "/Ozone_Stats_chambers.csv"

start <- read.csv(paste0(folder, file), header = TRUE, stringsAsFactors = FALSE)

for (i in 2:length(f))
	{ add <- read.csv(paste0(f[i],file), header = TRUE, stringsAsFactors = FALSE)
	 colnames(add) <- names
	 add <- add[1:3]
	 start <- rbind(start, add) }

start <- start[start$ave > 0,]  

write.csv(start, file = "C:/Users/Zachary/OneDrive/IUFRP/Ozone/Extra/Complete_Ozone_chambers.csv", row.names = FALSE)

## Chamber Measurements

f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/Ozone", pattern = "_", full.names = TRUE)

length(f)

for (i in 1:length(f))
{	assign(f[i], read.csv(f[i], header = FALSE))	}

folder <- f[1]
file <- "/Ozone_Stats_chambers.csv"
names <- c("datetime_a", "ave_a", "sd_a", "datetime_b", "ave_b", "sd_b", "datetime_c", "ave_c", "sd_c", "datetime_d", "ave_d", "sd_d")

start <- read.csv(paste0(folder, file), header = TRUE, stringsAsFactors = FALSE)
colnames(start) <- names
starta <- start[1:3]
startb <- start[4:6]
startc <- start[7:9]
startd <- start[10:12]

for (i in 2:length(f))
{ add <- read.csv(paste0(f[i],file), header = TRUE, stringsAsFactors = FALSE)
colnames(add) <- names
adda <- add[1:3]
addb <- add[4:6]
addc <- add[7:9]
addd <- add[10:12]

starta <- rbind(starta, adda)
startb <- rbind(startb, addb)
startc <- rbind(startc, addc)
startd <- rbind(startd, addd) }

starta <- starta[starta$ave_a > 0,]
startb <- startb[startb$ave_b > 0,]
startc <- startc[startc$ave_c > 0,]
startd <- startd[startd$ave_d > 0,]

write.csv(starta, file = "C:/Users/Zachary/OneDrive/IUFRP/Ozone/Extra/Complete_Ozone_a.csv", row.names = FALSE)
write.csv(startb, file = "C:/Users/Zachary/OneDrive/IUFRP/Ozone/Extra/Complete_Ozone_b.csv", row.names = FALSE)
write.csv(startc, file = "C:/Users/Zachary/OneDrive/IUFRP/Ozone/Extra/Complete_Ozone_c.csv", row.names = FALSE)
write.csv(startd, file = "C:/Users/Zachary/OneDrive/IUFRP/Ozone/Extra/Complete_Ozone_d.csv", row.names = FALSE)
