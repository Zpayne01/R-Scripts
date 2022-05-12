## This file can be used to convert water from ppth to g/m^3 using the temperature from the laboratory
## Choose the file containing the water data

file <- file.choose()
datatable <- read.csv(file, stringsAsFactors = FALSE)

## Remove unnecessary data (this needs to be changed depending on the data set)

newdatatable <- data.frame(datatable$hour, datatable$time_b, datatable$H2O_ppt_b, datatable$H2O_ppt_c, datatable$H2O_ppt_d, datatable$H2O_ppt_e)
colnames(newdatatable) <- c('hour', 'datetime', 'B_H2O_ppth',  'C_H2O_ppth',  'D_H2O_ppth',  'E_H2O_ppth')

## Update based on the number of columns
## Convert water to g/m^3 from ppth at STP

newdatatable$B_H2O_ppth <- newdatatable$B_H2O_ppth/1000/24.05*1000*18
newdatatable$C_H2O_ppth <- newdatatable$C_H2O_ppth/1000/24.05*1000*18
newdatatable$D_H2O_ppth <- newdatatable$D_H2O_ppth/1000/24.05*1000*18
newdatatable$E_H2O_ppth <- newdatatable$E_H2O_ppth/1000/24.05*1000*18

## Name the columns

colnames(newdatatable) <- c('date', 'hour', 'b_h2o_g_m^-3','c_h2o_g_m^-3','d_h2o_g_m^-3','e_h2o_g_m^-3')

## Export the data set

date <- as.Date(newdatatable$date[1])
date <- gsub('-', '_', date)
filename <- paste0('C:/Users/Zachary/Desktop/Working H2O/Absolute_humidity_',date,'.csv')
write.csv(newdatatable, filename, row.names = FALSE)
