f <- list.dirs('D:/Surface 3/Main Folder/One Drive/IURTP/Ozone')
f <- f[2:26]

ozoneamb <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('datetime', 'O3_ppb', 'O3_sd'))
ozonea <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('datetime', 'O3_ppb', 'O3_sd'))
ozoneb <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('datetime', 'O3_ppb', 'O3_sd'))
ozonec <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('datetime', 'O3_ppb', 'O3_sd'))
ozoned <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('datetime', 'O3_ppb', 'O3_sd'))

for (i in 1:length(f)) {
  file = paste0(f[i],'/Ozone_Stats_chambers.csv')
  csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  ozoneaadd <- setNames(data.frame(csv$datetime_a, csv$ave_Ozone_a, csv$sd_Ozone_a), c('datetime', 'O3_ppb', 'O3_sd'))
  ozonebadd <- setNames(data.frame(csv$datetime_b, csv$ave_Ozone_b, csv$sd_Ozone_b), c('datetime', 'O3_ppb', 'O3_sd'))
  ozonecadd <- setNames(data.frame(csv$datetime_c, csv$ave_Ozone_c, csv$sd_Ozone_c), c('datetime', 'O3_ppb', 'O3_sd'))
  ozonedadd <- setNames(data.frame(csv$datetime_d, csv$ave_Ozone_d, csv$sd_Ozone_d), c('datetime', 'O3_ppb', 'O3_sd'))
  ozonea <- rbind(ozonea, ozoneaadd)
  ozoneb <- rbind(ozoneb, ozonebadd)
  ozonec <- rbind(ozonec, ozonecadd)
  ozoned <- rbind(ozoned, ozonedadd)
}

for (i in 1:length(f)) {
  file = paste0(f[i], '/Ozone_Stats_amb.csv')
  csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  ozoneambadd <- setNames(data.frame(csv$datetime, csv$ave, csv$sd), c('datetime', 'O3_ppb', 'O3_sd'))
  ozoneamb <- rbind(ozoneamb, ozoneambadd)
}

ozonea <- ozonea[!is.na(ozonea$datetime),] 
ozoneb <- ozoneb[!is.na(ozoneb$datetime),]
ozonec <- ozonec[!is.na(ozonec$datetime),]
ozoned <- ozoned[!is.na(ozoned$datetime),]

ozoneamb <- ozoneamb[!is.na(ozoneamb$datetime),]

filenamea <- 'C:/Users/zacpayne/Desktop/Ozone_a.csv'
filenameb <- 'C:/Users/zacpayne/Desktop/Ozone_b.csv'
filenamec <- 'C:/Users/zacpayne/Desktop/Ozone_c.csv'
filenamed <- 'C:/Users/zacpayne/Desktop/Ozone_d.csv'
filenameamb <- 'C:/Users/zacpayne/Desktop/Ozone_amb.csv'

write.csv(ozonea, filenamea, row.names = FALSE)
write.csv(ozoneb, filenameb, row.names = FALSE)
write.csv(ozonec, filenamec, row.names = FALSE)
write.csv(ozoned, filenamed, row.names = FALSE)
write.csv(ozoneamb, filenameamb, row.names = FALSE)