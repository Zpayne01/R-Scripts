file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

names <- colnames(csv)

head(csv)

addProp <- function (x) {
  sqrt(sum(x^2))
}

n2o_ave <- aggregate(n2o_ppb ~ time, csv, mean)
n2o_sd <- aggregate(n2o_ppb_sd ~ time, csv, addProp)

co_ave <- aggregate(co_ppb ~ time, csv, mean)
co_sd <- aggregate(co_ppb_sd ~ time, csv, addProp)

csv2 <- data.frame(n2o_ave, n2o_sd$n2o_ppb_sd, co_ave$co_ppb, co_sd$co_ppb_sd)
colnames(csv2) <- names

csv2$time <- as.POSIXlt(csv2$time, format = "%m/%d/%Y %H:%M")

uniqueDates <- unique(as.Date(csv2$time))

for (i in 1:length(uniqueDates)) {
  
  exportDate <- csv2[as.Date(csv2$time) == uniqueDates[i],]
  exportDate[order(exportDate$time),]
  dateForName <- gsub('-','_',uniqueDates[i])
  filename <- paste0('C:/Users/zacpayne/Desktop/Working Files/',dateForName,' Co and N2O.csv')
  write.csv(exportDate, filename, row.names = FALSE)
  
  
}