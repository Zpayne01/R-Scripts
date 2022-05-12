library(lubridate)

csv <- read.table(file = "clipboard", sep = '\t', header = TRUE, stringsAsFactors = FALSE)

csv$Time <- as.POSIXlt(csv$Time, format = "%m/%d/%Y %H:%M")

csv$Time <- floor_date(csv$Time, unit = "5 minutes")

ambientNO <- subset(csv, csv$Time$min == 0 | csv$Time$min == 15 | csv$Time$min == 30 | csv$Time$min == 45)

noa <- subset(csv, csv$Time$min == 05 | csv$Time$min == 10)
nob <- subset(csv, csv$Time$min == 20 | csv$Time$min == 25)
noc <- subset(csv, csv$Time$min == 35 | csv$Time$min == 40)
nod <- subset(csv, csv$Time$min == 50 | csv$Time$min == 55)

noa$Time <- floor_date(noa$Time, unit = "15 minutes") 
nob$Time <- floor_date(nob$Time, unit = "15 minutes") 
noc$Time <- floor_date(noc$Time, unit = "15 minutes") 
nod$Time <- floor_date(nod$Time, unit = "15 minutes") 

plot(ambientNO$Time, ambientNO$NO, type = "l")

ambientNO$Time <- as.POSIXct(ambientNO$Time, format = "%Y-%m-%d %H:%M:S")

noaave <- aggregate(NO ~ as.POSIXct(noa$Time, format = "%Y-%m-%d %H:%M:%S"), noa, mean)
nobave <- aggregate(NO ~ as.POSIXct(nob$Time, format = "%Y-%m-%d %H:%M:%S"), nob, mean)
nocave <- aggregate(NO ~ as.POSIXct(noc$Time, format = "%Y-%m-%d %H:%M:%S"), noc, mean)
nodave <- aggregate(NO ~ as.POSIXct(nod$Time, format = "%Y-%m-%d %H:%M:%S"), nod, mean)

matcha <- match(noaave[,1], ambientNO[,1])
matchb <- match(nobave[,1], ambientNO[,1])
matchc <- match(nocave[,1], ambientNO[,1])
matchd <- match(nodave[,1], ambientNO[,1])

## Units ng N m-2 s-1

fluxa <- ((noaave$NO - ambientNO$NO[matcha])/10^12 * ((1/22.4) * 1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^9)/(60)) 
fluxb <- ((nobave$NO - ambientNO$NO[matchb])/10^12 * ((1/22.4) * 1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^9)/(60)) 
fluxc <- ((nocave$NO - ambientNO$NO[matchc])/10^12 * ((1/22.4) * 1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^9)/(60)) 
fluxd <- ((nodave$NO - ambientNO$NO[matchd])/10^12 * ((1/22.4) * 1000) * (0.02 + 0.0027))/(0.106535) * ((14.0 * 10^9)/(60)) 

write.table(data.frame(ambientNO$Time[matcha], fluxa), file = "clipboard", sep = "\t", row.names = FALSE)
write.table(data.frame(ambientNO$Time[matchb], fluxb), file = "clipboard", sep = "\t", row.names = FALSE)
write.table(data.frame(ambientNO$Time[matchc], fluxc), file = "clipboard", sep = "\t", row.names = FALSE)
write.table(data.frame(ambientNO$Time[matchd], fluxd), file = "clipboard", sep = "\t", row.names = FALSE)

plot(ambientNO$Time[matcha], fluxa, type = "l")