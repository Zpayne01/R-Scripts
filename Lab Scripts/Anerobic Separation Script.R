## Open the file containing the Anaerobic Data ##

file <- file.choose()
table <- read.delim(file, stringsAsFactors = FALSE)

colnames(table) <- c('start_date', 'exp_hr', 'chamber', 'co2_ppm', 'co_ppb', 'h2o_ppth', 'n2o_ppb')
dates <- unique(table$start_date)

for(i in 1:length(dates)) {
  workingdate <- dates[i]
  date <- subset(table, table$start_date == workingdate)
  chamberb <- subset(date, date$chamber == 'B')
  chamberc <- subset(date, date$chamber == 'C')
  chamberd <- subset(date, date$chamber == 'D')
  chambere <- subset(date, date$chamber == 'E')
  chamberblank <- subset(date, date$chamber == 'Blank')
  
  chamberb$co_ppb <- chamberb$co_ppb - chamberblank$co_ppb
  chamberc$co_ppb <- chamberc$co_ppb - chamberblank$co_ppb
  chamberd$co_ppb <- chamberd$co_ppb - chamberblank$co_ppb
  chambere$co_ppb <- chambere$co_ppb - chamberblank$co_ppb
  
  chamberb$n2o_ppb <- chamberb$n2o_ppb - chamberblank$n2o_ppb
  chamberc$n2o_ppb <- chamberc$n2o_ppb - chamberblank$n2o_ppb
  chamberd$n2o_ppb <- chamberd$n2o_ppb - chamberblank$n2o_ppb
  chambere$n2o_ppb <- chambere$n2o_ppb - chamberblank$n2o_ppb
  
  chamberb$co2_ppm <- chamberb$co2_ppm - chamberblank$co2_ppm
  chamberc$co2_ppm <- chamberc$co2_ppm - chamberblank$co2_ppm
  chamberd$co2_ppm <- chamberd$co2_ppm - chamberblank$co2_ppm
  chambere$co2_ppm <- chambere$co2_ppm - chamberblank$co2_ppm
  
  chamberb$h2o_ppth <- chamberb$h2o_ppth - chamberblank$h2o_ppth
  chamberc$h2o_ppth <- chamberc$h2o_ppth - chamberblank$h2o_ppth
  chamberd$h2o_ppth <- chamberd$h2o_ppth - chamberblank$h2o_ppth
  chambere$h2o_ppth <- chambere$h2o_ppth - chamberblank$h2o_ppth
  
  export <- data.frame(chamberb$start_date, chamberb$exp_hr, chamberb$co_ppb, chamberb$n2o_ppb, chamberb$co2_ppm, chamberb$h2o_ppth,
                       chamberc$co_ppb, chamberc$n2o_ppb, chamberc$co2_ppm, chamberc$h2o_ppth, chamberd$co_ppb,
                       chamberd$n2o_ppb, chamberd$co2_ppm, chamberd$h2o_ppth, chambere$co_ppb, chambere$n2o_ppb,
                       chambere$co2_ppm, chambere$h2o_ppth)
  colnames(export) <- c('date', 'hour', 'b_co_ppb', 'b_n2o_ppb','b_co2_ppb', 'b_h2o_ppth', 'c_co_ppb', 'c_n2o_ppb','c_co2_ppm','c_h2o_ppth',
                        'd_co_ppb', 'd_n2o_ppb','d_co2_ppm','d_h2o_ppth', 'e_co_ppb', 'e_n2o_ppb', 'e_co2_ppm', 'e_h2o_ppth')
  
  workingdate <- gsub("/", '_', workingdate)
  
  filename <- paste0('C:/Users/Zachary/Desktop/CO_N2O_CO2_H2O_',workingdate,'.csv') 
  write.csv(export, filename, row.names = FALSE)
}