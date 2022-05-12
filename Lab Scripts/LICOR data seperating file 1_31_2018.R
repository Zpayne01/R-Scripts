file <- file.choose()
workingfile <- read.delim(file, header = TRUE, stringsAsFactors = FALSE)

## Find unique dates

workingfile <- subset(workingfile, workingfile$start_date != '')

workingfile$start_date <- as.POSIXct(workingfile$start_date, format = '%m/%d/%Y')
uniquedates <- unique(workingfile$start_date)

for (i in 1:length(uniquedates)) {
  
  workingdate <- subset(workingfile, workingfile$start_date == uniquedates[i])
  
  chamberblank <- subset(workingdate, workingdate$chamber == 'Blank')
  chamberb <- subset(workingdate, workingdate$chamber == 'B')
  chamberc <- subset(workingdate, workingdate$chamber == 'C')
  chamberd <- subset(workingdate, workingdate$chamber == 'D')
  chambere <- subset(workingdate, workingdate$chamber == 'E')
  
  date <- format(chamberblank$start_date, '%m/%d/%Y')
  
  export <- data.frame(date, chamberblank$exp_hr, chamberblank$co2_ppm, chamberblank$h2o_ppth,
                       chamberb$co2_ppm, chamberb$h2o_ppth, chamberc$co2_ppm, chamberc$h2o_ppth, chamberd$co2_ppm,
                       chamberd$h2o_ppth, chambere$co2_ppm, chambere$h2o_ppth)
  
  colnames(export) <- c('date', 'hour', 'blank_co2_ppm', 'blank_h2o_ppth', 'b_co2_ppm', 'b_h2o_ppth',
                        'c_co2_ppm', 'c_h2o_ppth','d_co2_ppm', 'd_h2o_ppth','e_co2_ppm', 'e_h2o_ppth')
  
  filename <- paste0('C:/Users/zacpayne/Desktop/CO2_and_H2O_',gsub('/', '_', date[1]),'.csv')

  write.csv(export, file = filename, row.names = FALSE)
  
  }