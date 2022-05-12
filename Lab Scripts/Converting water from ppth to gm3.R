## This file can be used to convert water from ppth to g/m^3
## Choose the file containing the water data

file <- file.choose()
datatable <- read.csv(file, stringsAsFactors = FALSE)

## Remove unnecessary data (this needs to be changed depending on the data set)

newdatatable <- data.frame(datatable$date, datatable$hour, datatable$b_h2o_ppth, datatable$c_h2o_ppth, datatable$d_h2o_ppth, datatable$e_h2o_ppth)

## Update based on the number of columns
## Convert water to g/m^3 from ppth at STP

newdatatable$datatable.b_h2o_ppth <- newdatatable$datatable.b_h2o_ppth/1000*22.4/1000*18
newdatatable$datatable.c_h2o_ppth <- newdatatable$datatable.c_h2o_ppth/1000*22.4/1000*18
newdatatable$datatable.d_h2o_ppth <- newdatatable$datatable.d_h2o_ppth/1000*22.4/1000*18
newdatatable$datatable.e_h2o_ppth <- newdatatable$datatable.e_h2o_ppth/1000*22.4/1000*18

## Name the columns

colnames(newdatatable) <- c('date', 'hour', 'b_h2o_g_m^-3','c_h2o_g_m^-3','d_h2o_g_m^-3','e_h2o_g_m^-3')

## Export the data set

date <- newdatatable$date[1]
date <- gsub('/', '_', date)
filename <- paste0('C:/Users/Zachary/Desktop/Update_Water_',date,'.csv')
write.csv(newdatatable, filename, row.names = FALSE)
