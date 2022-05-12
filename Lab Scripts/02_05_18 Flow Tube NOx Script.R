## Flow tube analysis script

  start <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
  
## Put the calibration file determined before the experiment consolidated on page 2 of book 2
  
  calibration <- 2666.6

## Nitric oxide concentration    
      
  no_ppb <- start$nocountsave/calibration
  no_ppb_sd <- start$nocountssd/calibration
  
## HONO concentration (updated standard deviation equation on 02_27_2018)
  
  hono_ppb <- ((start$blc1countsave - start$blc2countsave)+72.605)/(100.08)
  hono_ppb_sd <- (sqrt(start$blc1countssd^2+start$blc2countssd^2))/(100.08)
  
## Create exportable file for NOx Calibrations
  
  export <- data.frame(start$time, no_ppb, no_ppb_sd, hono_ppb, hono_ppb_sd)
  colnames(export) <- c('time', 'no_ppb', 'no_sd', 'hono_ppb', 'hono_sd')

## Write as an export
  head(start)
  
  time <- as.POSIXct(start$time, format = '%Y-%m-%d %H:%M:%S')
  date <- format(time[1], '%m_%d_%y')
 
  filename <- paste0('C:/Users/Zachary/DesktNO_and_HONO',date,'.csv')
  write.csv(export, file = filename, row.names = FALSE)
  