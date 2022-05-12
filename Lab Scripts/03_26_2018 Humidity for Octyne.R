
filename <- file.choose()
h2ofile <- read.csv(filename, header = TRUE)

dates <- as.character(unique(h2ofile$start_date))

for (i in 1:length(dates)){
  
  start <- subset(h2ofile, h2ofile$start_date == dates[i])

  startblank <- subset(start, start$Chamber == 'Blank')
  startB <- subset(start, start$Chamber == 'B')
  startC <- subset(start, start$Chamber == 'C')
  startD <- subset(start, start$Chamber == 'D')
  startE <- subset(start, start$Chamber == 'E')
  
  export <- data.frame(startblank$start_date, startblank$Exp_hr, startblank$H2O_ppth, startB$H2O_ppth, startC$H2O_ppth, startD$H2O_ppth, startE$H2O_ppth)
  colnames(export) <- c('time','hour','blank','b','c','d','e')
  
  filename <- paste0('C:/Users/Zachary/Documents/Lab Data/Incubation Experiment/Octyne/H2O/', gsub('/','_',dates[i]),'_H2O.csv')
  write.csv(export, file = filename, row.names = TRUE)
}
  