f <- list.files(path = "C:/Users/Zachary/OneDrive/IUFRP/NOx/AQD NOx New CE/Concentrations", full.names = TRUE)

start <- read.csv(f[1], header = TRUE, stringsAsFactors = FALSE)

starta <- start[1:7]
startb <- start[8:14]
startc <- start[15:21]
startd <- start[22:28]
startamb <- start[29:35]


for (i in 2:length(f))
{ add <- read.csv(f[i], header = TRUE, stringsAsFactors = FALSE)
adda <- add[1:7]
starta <- rbind(starta, adda) 

addb <- add[8:14]
startb <- rbind(startb, addb)

addc <- add[15:21]
startc <- rbind(startc, addc)

addd <- add[22:28]
startd <- rbind(startd, addd)

addamb <- add[29:35]
startamb <- rbind(startamb, addamb)}

starta <- na.omit(starta)
startb <- na.omit(startb)
startc <- na.omit(startc)
startd <- na.omit(startd)
startamb <- na.omit(startamb)

write.csv(starta, file = "C:/Users/Zachary/OneDrive/IUFRP/NOx/AQD NOx New CE/combined_a.csv", row.names = FALSE)
write.csv(startb, file = "C:/Users/Zachary/OneDrive/IUFRP/NOx/AQD NOx New CE/combined_b.csv", row.names = FALSE)
write.csv(startc, file = "C:/Users/Zachary/OneDrive/IUFRP/NOx/AQD NOx New CE/combined_c.csv", row.names = FALSE)
write.csv(startd, file = "C:/Users/Zachary/OneDrive/IUFRP/NOx/AQD NOx New CE/combined_d.csv", row.names = FALSE)
write.csv(startamb, file = "C:/Users/Zachary/OneDrive/IUFRP/NOx/AQD NOx New CE/combined_amb.csv", row.names = FALSE)

