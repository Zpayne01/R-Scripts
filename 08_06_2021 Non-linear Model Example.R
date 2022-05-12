file <- file.choose()
data <- read.csv(file, header = T)

model <- lm(no_flux_ng_N_m2_s ~ soil_temp_ave + soil_moisture_ave, data)
summary(model)

modeltemp <- lm(no_flux_ng_N_m2_s ~ soil_temp_ave, data)
summary(modeltemp)

modelh2o <- lm(no_flux_ng_N_m2_s ~ soil_moisture_ave, data)
summary(modelh2o)

modelvsmeas <- lm(noflux_calc ~ no_flux_ng_N_m2_s, data)
summary(modelvsmeas)

data$datetime <- as.POSIXct(data$datetime, format = '%m/%d/%Y %H:%M')
plot(data$datetime, data$no_flux_ng_N_m2_s, type = 'l', col = 'black')
lines(data$datetime, data$noflux_calc, type = 'l', col = 'red')

model <- lm(n2o_ave ~ soil_temp_ave + soil_moisture_ave, data)
summary(model)

data$n2o_calc <- -2.942 + 0.112 * data$soil_temp_ave + 5.395 * data$soil_moisture_ave

data$floor <- as.POSIXct(data$floor, format = '%m/%d/%Y %H:%M')
plot(data$floor, data$n2o_ave, type = 'l', col = 'black')
lines(data$floor, data$n2o_calc, type = 'l', col = 'red')

plot(n2o_ave ~ soil_temp_ave, data=data, type = 'p')
plot(n2o_ave ~ soil_moisture_ave, data=data, type = 'p')

nlsmodel <- nls(n2o_ave ~ soil_temp_ave*a + b*soil_moisture_ave^2, data = data, start = list(a=10, b=10))
summary(nlsmodel)

data$nls_model_data <- data$soil_temp_ave*0.0193272 + 8.421689 * data$soil_moisture_ave^2

plot(data$floor, data$n2o_ave, type = 'l', col = 'black')
lines(data$floor, data$nls_model_data, type = 'l', col = 'red')
 

x <-90000
