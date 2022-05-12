file <- file.choose()
workingfile <- read.delim(file, header = TRUE, stringsAsFactors = FALSE)

## Find unique dates

workingfile <- subset(workingfile, workingfile$start_date != '')

workingfile$start_date <- as.POSIXct(workingfile$start_date, format = '%m/%d/%Y')
uniquedates <- unique(workingfile$start_date)

for (i in 1:length(uniquedates)) {
  
  workingdate <- subset(workingfile, workingfile$start_date == uniquedates[i])
  
  chamberblankb <- subset(workingdate, workingdate$chamber == 'Blank-B')
  chamberblankc <- subset(workingdate, workingdate$chamber == 'Blank-C')
  chamberblankd <- subset(workingdate, workingdate$chamber == 'Blank-D')
  chamberblanke <- subset(workingdate, workingdate$chamber == 'Blank-E')
  chamberb <- subset(workingdate, workingdate$chamber == 'B')
  chamberc <- subset(workingdate, workingdate$chamber == 'C')
  chamberd <- subset(workingdate, workingdate$chamber == 'D')
  chambere <- subset(workingdate, workingdate$chamber == 'E')
  
  date <- format(chamberblankb$start_date, '%m/%d/%Y')
  
  export <- data.frame(date, chamberblankb$exp_hr, chamberblankb$co_ppb, chamberblankb$n2o_ppb,
                       chamberblankc$co_ppb, chamberblankc$n2o_ppb, chamberblankd$co_ppb, chamberblankd$n2o_ppb,
                       chamberblanke$co_ppb, chamberblanke$n2o_ppb, chamberb$co_ppb, chamberb$n2o_ppb, chamberc$co_ppb, chamberc$n2o_ppb, chamberd$co_ppb,
                       chamberd$n2o_ppb, chambere$co_ppb, chambere$n2o_ppb)
  
  colnames(export) <- c('date', 'hour', 'blankb_co_ppb', 'blankb_n2o_ppb', 'blankc_co_ppb', 'blankc_n2o_ppb',
                        'blankd_co_ppb', 'blankd_n2o_ppb', 'blanke_co_ppb', 'blanke_n2o_ppb','b_co_ppb', 'b_n2o_ppb',
                        'c_co_ppb', 'c_n2o_ppb','d_co_ppb', 'd_n2o_ppb','e_co_ppb', 'e_n2o_ppb')
  
  filename <- paste0('C:/Users/zacpayne/Desktop/N2O_and_CO_',gsub('/', '_', date[1]),'.csv')
  
  write.csv(export, file = filename, row.names = FALSE)
  
}
