## Open the correct file (N2O and CO) ##

file <- file.choose()
table <- read.table(file, sep = '\t', header = TRUE, stringsAsFactors = FALSE)

## 

dates <- unique(table$start_date)

for(i in 1:length(dates)) {
  
  workingdate <- dates[i]
  date <- subset(table, table$start_date == workingdate)
  chamberb <- subset(date, date$chamber == 'B')
  chamberbblank <- subset(date, date$chamber == 'Blank-B')
  chamberc <- subset(date, date$chamber == 'C')
  chambercblank <- subset(date, date$chamber == 'Blank-C')
  chamberd <- subset(date, date$chamber == 'D')
  chamberdblank <- subset(date, date$chamber == 'Blank-D')
  chambere <- subset(date, date$chamber == 'E')
  chambereblank <- subset(date, date$chamber == 'Blank-E')

## Calculate the change in concentration ##
  
  chamberb$co_ppb <- chamberb$co_ppb - chamberbblank$co_ppb
  chamberc$co_ppb <- chamberc$co_ppb - chambercblank$co_ppb
  chamberd$co_ppb <- chamberd$co_ppb - chamberdblank$co_ppb
  chambere$co_ppb <- chambere$co_ppb - chambereblank$co_ppb
  
  chamberb$n2o_ppb <- chamberb$n2o_ppb - chamberbblank$n2o_ppb
  chamberc$n2o_ppb <- chamberc$n2o_ppb - chambercblank$n2o_ppb
  chamberd$n2o_ppb <- chamberd$n2o_ppb - chamberdblank$n2o_ppb
  chambere$n2o_ppb <- chambere$n2o_ppb - chambereblank$n2o_ppb
  
  export <- data.frame(chamberb$start_date, chamberb$exp_hr, chamberb$co_ppb, chamberb$n2o_ppb, chamberc$co_ppb, chamberc$n2o_ppb,
                       chamberd$co_ppb, chamberd$n2o_ppb, chambere$co_ppb, chambere$n2o_ppb)
  colnames(export) <- c('date', 'hour', 'b_co_ppb', 'b_n2o_ppb', 'c_co_ppb', 'c_n2o_ppb', 'd_co_ppb', 'd_n2o_ppb', 'e_co_ppb', 'e_n2o_ppb')
  
  workingdate <- gsub("/", '_', workingdate)
  
  filename <- paste0('C:/Users/Zachary/Desktop/CO_&_N2O_',workingdate,'.csv') 
  write.csv(export, filename, row.names = FALSE)
}