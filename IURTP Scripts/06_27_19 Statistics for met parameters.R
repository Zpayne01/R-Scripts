file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)
head(csv)
csv$datetime <- csv$time
csv$datetime <- as.POSIXlt(csv$datetime, format = "%m/%d/%Y %H:%M")

csv$hour <- csv$datetime$hour

csvday <- subset(csv, csv$datetime$hour >= 8 & csv$datetime$hour < 20)
csvnight <- subset(csv, csv$datetime$hour >= 20 | csv$datetime$hour < 8)

variable <- csv$no2_ave
variableday <- csvday$no2_ave
variablenight <- csvnight$no2_ave

summary(variable, na.rm = TRUE)
sd(variable, na.rm = TRUE)
mean(variableday, na.rm = TRUE)
sd(variableday, na.rm = TRUE)
mean(variablenight, na.rm = TRUE)
sd(variablenight, na.rm = TRUE)

colnames(csv)
