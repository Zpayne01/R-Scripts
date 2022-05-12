## File for correcting most rectent incubation experiment with absolute humdity ##

f <- list.files(path = "C:/Users/Zachary/Desktop/No Treatment", full.names = TRUE, pattern = '.csv')
f2 <- list.files(path = "C:/Users/Zachary/Documents/Lab Data/Incubation Experiment/No Treatment/H2O", full.names = TRUE)
setwd <- 'C:/Users/Zachary/Desktop'

for (i in 1:length(f))  {	
  assign(f[i], read.csv(f[i]))
}

start <- read.csv(f[1], header = TRUE, stringsAsFactors = FALSE)
start$time <- as.POSIXlt(start$time, format = "%Y-%m-%d %H:%M:%S")
start$time <- format(start$time, '%m_%d_%Y')
file <- paste0('C:/Users/Zachary/Documents/Lab Data/Incubation Experiment/No Treatment/H2O/',start$time[1],'_H2O.csv')
chamber <- substring(f[1], 68, 68)
h2ofile <- read.csv(file, header = TRUE)
matchedh2o <- match(start$hour, h2ofile$hour-1)

reloadtime <- read.csv(f[1], header = TRUE, stringsAsFactors = FALSE)
start$time <- reloadtime$time

if (chamber == 'B'){
  
  start$no_ppb <- start$no_ppb /(-0.0052*h2ofile$b[matchedh2o]*0.751+1)
  start$no2_ppb <- start$no2_ppb /(-0.0052*h2ofile$b[matchedh2o]*0.751+1)
  start$hono_ppb <- start$hono_ppb /(-0.0052*h2ofile$b[matchedh2o]*0.751+1)

  filename <- paste0("C:/Users/Zachary/Desktop/No Treatment/humidity Corrected/",substring(f[1], 39, 68),'.csv')
  write.csv(start, filename, row.names = FALSE)
} else if (chamber == 'C'){
  
  start$no_ppb <- start$no_ppb /(-0.0052*h2ofile$c[matchedh2o]*0.751+1)
  start$no2_ppb <- start$no2_ppb /(-0.0052*h2ofile$c[matchedh2o]*0.751+1)
  start$hono_ppb <- start$hono_ppb /(-0.0052*h2ofile$c[matchedh2o]*0.751+1)
  
  filename <- paste0("C:/Users/Zachary/Desktop/No Treatment/humidity Corrected/",substring(f[1], 39, 68),'.csv')
  write.csv(start, filename, row.names = FALSE)
} else if (chamber == 'D'){
  
  start$no_ppb <- start$no_ppb /(-0.0052*h2ofile$d[matchedh2o]*0.751+1)
  start$no2_ppb <- start$no2_ppb /(-0.0052*h2ofile$d[matchedh2o]*0.751+1)
  start$hono_ppb <- start$hono_ppb /(-0.0052*h2ofile$d[matchedh2o]*0.751+1)
  
  filename <- paste0("C:/Users/Zachary/Desktop/No Treatment/humidity Corrected/",substring(f[1], 39, 68),'.csv')
  write.csv(start, filename, row.names = FALSE)
} else if (chamber == 'E'){
  
  start$no_ppb <- start$no_ppb /(-0.0052*h2ofile$e[matchedh2o]*0.751+1)
  start$no2_ppb <- start$no2_ppb /(-0.0052*h2ofile$e[matchedh2o]*0.751+1)
  start$hono_ppb <- start$hono_ppb /(-0.0052*h2ofile$e[matchedh2o]*0.751+1)
  
  filename <- paste0("C:/Users/Zachary/Desktop/No Treatment/humidity Corrected/",substring(f[1], 39, 68),'.csv')
  write.csv(start, filename, row.names = FALSE)
}



for (i in 2:length(f)) {

start <- read.csv(f[i], header = TRUE, stringsAsFactors = FALSE)
start$time <- as.POSIXlt(start$time, format = "%Y-%m-%d %H:%M:%S")
start$time <- format(start$time, '%m_%d_%Y')
file <- paste0('C:/Users/Zachary/Documents/Lab Data/Incubation Experiment/No Treatment/H2O/',start$time[1],'_H2O.csv')
chamber <- substring(f[i], 68, 68)
h2ofile <- read.csv(file, header = TRUE)
matchedh2o <- match(start$hour, h2ofile$hour-1)
reloadtime <- read.csv(f[i], header = TRUE, stringsAsFactors = FALSE)
start$time <- reloadtime$time

if (chamber == 'B'){
  
  start$no_ppb <- start$no_ppb /(-0.0052*h2ofile$b[matchedh2o]*0.751+1)
  start$no2_ppb <- start$no2_ppb /(-0.0052*h2ofile$b[matchedh2o]*0.751+1)
  start$hono_ppb <- start$hono_ppb /(-0.0052*h2ofile$b[matchedh2o]*0.751+1)
  
  filename <- paste0("C:/Users/Zachary/Desktop/No Treatment/humidity Corrected/",substring(f[i], 39, 68),'.csv')
  write.csv(start, filename, row.names = FALSE)
} else if (chamber == 'C'){
  
  start$no_ppb <- start$no_ppb /(-0.0052*h2ofile$c[matchedh2o]*0.751+1)
  start$no2_ppb <- start$no2_ppb /(-0.0052*h2ofile$c[matchedh2o]*0.751+1)
  start$hono_ppb <- start$hono_ppb /(-0.0052*h2ofile$c[matchedh2o]*0.751+1)
  
  filename <- paste0("C:/Users/Zachary/Desktop/No Treatment/humidity Corrected/",substring(f[i], 39, 68),'.csv')
  write.csv(start, filename, row.names = FALSE)
} else if (chamber == 'D'){
  
  start$no_ppb <- start$no_ppb /(-0.0052*h2ofile$d[matchedh2o]*0.751+1)
  start$no2_ppb <- start$no2_ppb /(-0.0052*h2ofile$d[matchedh2o]*0.751+1)
  start$hono_ppb <- start$hono_ppb /(-0.0052*h2ofile$d[matchedh2o]*0.751+1)
  
  filename <- paste0("C:/Users/Zachary/Desktop/No Treatment/humidity Corrected/",substring(f[i], 39, 68),'.csv')
  write.csv(start, filename, row.names = FALSE)
} else if (chamber == 'E'){
  
  start$no_ppb <- start$no_ppb /(-0.0052*h2ofile$e[matchedh2o]*0.751+1)
  start$no2_ppb <- start$no2_ppb /(-0.0052*h2ofile$e[matchedh2o]*0.751+1)
  start$hono_ppb <- start$hono_ppb /(-0.0052*h2ofile$e[matchedh2o]*0.751+1)
  
  filename <- paste0("C:/Users/Zachary/Desktop/No Treatment/humidity Corrected/",substring(f[i], 39, 68),'.csv')
  write.csv(start, filename, row.names = FALSE)
}
}