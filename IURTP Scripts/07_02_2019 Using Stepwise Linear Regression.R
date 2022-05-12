file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

csv$Time <- NULL

head(csv)

FitAll <- lm(no_ave ~ ., csv, na.action = na.omit)
FitStart <- lm(no_ave ~ 1, csv, na.action = na.omit)

stepwise <- step(FitAll, direction = "backward", scope = formula(FitAll))
