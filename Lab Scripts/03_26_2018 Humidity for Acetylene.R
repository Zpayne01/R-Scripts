filename <- file.choose()
h2ofile <- read.csv(filename, header = TRUE)

dates <- as.character(unique(h2ofile$start_date))

for (i in 1:length(dates)){
  
  start <- subset(h2ofile, h2ofile$start_date == dates[i])
  
  start[start$Blank < 0] <- 0
  start[start$b < 0] <- 0
  start[start$c < 0] <- 0
  start[start$d < 0] <- 0
  start[start$e < 0] <- 0
  
  export <- start
  
  filename <- paste0('C:/Users/Zachary/Documents/Lab Data/Incubation Experiment/Acetylene/H2O/', gsub('/','_',dates[i]),'_H2O.csv')
  write.csv(export, file = filename, row.names = FALSE)
}