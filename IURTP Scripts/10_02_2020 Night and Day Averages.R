
file <- file.choose()
tab <- read.csv(file , stringsAsFactors = FALSE)

head(tab)

tab$datetime <- as.POSIXlt(tab$datetime, format = '%m/%d/%Y %H:%M')
tab$hour <- tab$datetime$hour

head(tab)

tab <- tab[tab$Chamber == 'D',]

tabday <- subset(tab, tab$datetime$hour >= 8 & tab$datetime$hour < 20)
tabnight <- subset(tab, tab$datetime$hour >= 20 | tab$datetime$hour < 8)

tabday$period <- "day"
tabnight$period <- "night"
comb <- rbind(tabday, tabnight)

head(tabday)
head(tabnight)

colnames(tabday)

variable <- tab$no2_flux_ng_N_m2_s
variableday <- tabday$no2_flux_ng_N_m2_s
variablenight <- tabnight$no2_flux_ng_N_m2_s

summary(variable, na.rm = TRUE)
sd(variable, na.rm = TRUE)
mean(variableday, na.rm = TRUE)
sd(variableday, na.rm = TRUE)
mean(variablenight, na.rm = TRUE)
sd(variablenight, na.rm = TRUE)


summary(aov(no2_flux_ng_N_m2_s ~ period, comb))
TukeyHSD(aov(no2_flux_ng_N_m2_s ~ Chamber, tab))
