##

library(ggplot2)
library(ggpmisc)

file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

csv <- na.omit(csv)

#csv$datetime <- NULL
csv$no2_flux_ng_N.m2.s <- NULL
csv$vd..cm.s. <- NULL
csv$Ave_molecular_speed <- NULL
csv$Uptake.NO2 <- NULL
csv$Wind.Direction <- NULL

csv$datetime <- as.POSIXlt(csv$datetime, format = '%d/%m/%Y %H:%M')

day <- csv[csv$datetime$hour > 8 & csv$datetime$hour < 21,]
night <- csv[!(csv$datetime$hour > 8 & csv$datetime$hour < 21),]

head(csv)

csvnotime <- csv[,2:ncol(csv)]

FitAll <- lm(Uptake.NO2.Smooth ~ ., csvnotime, na.action = na.omit)
FitStart <- lm(Uptake.NO2.Smooth ~ 1, csvnotime, na.action = na.omit)

stepwise <- step(FitAll, direction = "backward", scope = formula(FitAll))
summary(stepwise)


mod1 <- lm(Uptake.NO2.Smooth ~ NO_ave_ppb + O3_ave_ppb + jNO2 + 
             Hours.Since.Rain + relative_humidity_ave + soil_moisture_ave, data = csv)
summary(mod1)
new <- csv
pred <- predict(mod1, newdata = new, na.action = na.omit)

plot(csv$Uptake.NO2.Smooth, pred, type = 'p')

daynotime <- day[,2:length(day)]

FitAll <- lm(Uptake.NO2.Smooth ~ ., daynotime, na.action = na.omit)
FitStart <- lm(Uptake.NO2.Smooth ~ 1, daynotime, na.action = na.omit)

stepwise <- step(FitAll, direction = "backward", scope = formula(FitAll))

summary(stepwise)
predict2 <- predict(eval(stepwise$call), daynotime)
plot(daynotime$Uptake.NO2.Smooth, predict2, type = 'p')

dev.new(height = 1, width = 1)
par(mfrow = c(4,3), mar = c(3.5,3.5,2,1), mgp = c(2,1,0))

csv$datetime <- as.POSIXct(csv$datetime)

for(i in 1:length(csv)) {
  name <- colnames(csv)[i]
  form <- paste('Uptake.NO2.Smooth ~', name)
  model <- lm(eval(parse(text = form)), csv)
  slope <- c(model$coefficients[2])
  intercept <- c(model$coefficients[1])
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(model) [1]), digits = 2),
                        b = format(unname(coef(model)[2]), digits = 2),
                        r2 = format(summary(model)$r.squared, digits = 4)))
  as.character(as.expression(eq));
  plot(eval(parse(text = form)), csv, type = 'p')
  abline(model, col = 'red')
  mtext(text = eq, side = 3, cex = 0.8)
}

FitAll <- lm(Uptake.NO2.Smooth ~ ., csv, na.action = na.omit)
FitStart <- lm(Uptake.NO2.Smooth ~ 1, csv, na.action = na.omit)

stepwise <- step(FitAll, direction = "both", scope = formula(FitAll), trace = 0)
pred <- predict(eval(stepwise$call), csv)
plot(csv$Uptake.NO2.Smooth, pred, type = 'p')

day$datetime <- as.POSIXct(day$datetime)

for(i in 1:length(day)) {
  name <- colnames(day)[i]
  form <- paste('Uptake.NO2.Smooth ~', name)
  model <- lm(eval(parse(text = form)), day)
  slope <- c(model$coefficients[2])
  intercept <- c(model$coefficients[1])
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(model) [1]), digits = 2),
                        b = format(unname(coef(model)[2]), digits = 2),
                        r2 = format(summary(model)$r.squared, digits = 4)))
  as.character(as.expression(eq));
  plot(eval(parse(text = form)), day, type = 'p')
  abline(model, col = 'red')
  mtext(text = eq, side = 3, cex = 0.8)
}

night$datetime <- as.POSIXct(night$datetime)

for(i in 1:length(night)) {
  name <- colnames(night)[i]
  form <- paste('Uptake.NO2.Smooth ~', name)
  model <- lm(eval(parse(text = form)), night)
  slope <- c(model$coefficients[2])
  intercept <- c(model$coefficients[1])
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(model) [1]), digits = 2),
                        b = format(unname(coef(model)[2]), digits = 2),
                        r2 = format(summary(model)$r.squared, digits = 4)))
  as.character(as.expression(eq));
  plot(eval(parse(text = form)), night, type = 'p')
  abline(model, col = 'red')
  mtext(text = eq, side = 3, cex = 0.8)
}


eval(stepwise$call)
