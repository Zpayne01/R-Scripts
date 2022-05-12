file1 <- file.choose()
file2 <- file.choose()

data1 <- read.csv(file1)
data2 <- read.csv(file2)

colnames(data1) <- c('time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')
colnames(data2) <- c('time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')

matched <- match(data1$time, data2$time)

combined_no <- data.frame(data1$no_ave, data2$no_ave[matched]) 
combined_no_sd <- data.frame(data1$no_sd, data2$no_sd[matched])
ES1 <- combined_no_sd$data1.no_sd ^ 2 * (45 - 1)
ES2 <- combined_no_sd$data2.no_sd.matched. ^ 2 * (45 - 1)
no <- rowMeans(combined_no, na.rm = TRUE)
GS1 <- (combined_no$data1.no_ave - no) ^ 2 * 45
GS2 <- (combined_no$data2.no_ave.matched. - no) ^ 2 * 45
ES12 <- cbind(ES1, ES2)
GS12 <- cbind(GS1, GS2)
ES <- rowSums(ES12, na.rm = TRUE)
GS <- rowSums(GS12, na.rm = TRUE)
GV <- sqrt((ES + GS)/(90 - 1))
GV[GV == 0] <- 'NA'
GV <- as.numeric(GV)

no_all <- data.frame(data1$time, no, GV)

combined_no2 <- data.frame(data1$no2_ave, data2$no2_ave[matched])
combined_no2_sd <- data.frame(data1$no2_sd, data2$no2_sd[matched])
no2 <- rowMeans(combined_no2, na.rm = TRUE)
ES1 <- combined_no2_sd$data1.no2_sd ^ 2 * (45 - 1)
ES2 <- combined_no2_sd$data2.no2_sd.matched. ^ 2 * (45 - 1)
no2 <- rowMeans(combined_no2, na.rm = TRUE)
GS1 <- (combined_no2$data1.no2_ave - no2) ^ 2 * 45
GS2 <- (combined_no2$data2.no2_ave.matched. - no2) ^ 2 * 45
ES12 <- cbind(ES1, ES2)
GS12 <- cbind(GS1, GS2)
ES <- rowSums(ES12, na.rm = TRUE)
GS <- rowSums(GS12, na.rm = TRUE)
GV <- sqrt((ES + GS)/(90 - 1))
GV[GV == 0] <- 'NA'
GV <- as.numeric(GV)

no2_all <- data.frame(data1$time, no2, GV)


combined_hono <- data.frame(data1$hono_ave, data2$hono_ave[matched]) 
combined_hono_sd <- data.frame(data1$hono_sd, data2$hono_sd[matched])
hono <- rowMeans(combined_hono, na.rm = TRUE)

ES1 <- combined_hono_sd$data1.hono_sd ^ 2 * (45 - 1)
ES2 <- combined_hono_sd$data2.hono_sd.matched. ^ 2 * (45 - 1)
hono <- rowMeans(combined_hono, na.rm = TRUE)
GS1 <- (combined_hono$data1.hono_ave - hono) ^ 2 * 45
GS2 <- (combined_hono$data2.hono_ave.matched. - hono) ^ 2 * 45
ES12 <- cbind(ES1, ES2)
GS12 <- cbind(GS1, GS2)
ES <- rowSums(ES12, na.rm = TRUE)
GS <- rowSums(GS12, na.rm = TRUE)
GV <- sqrt((ES + GS)/(90 - 1))
GV[GV == 0] <- 'NA'
GV <- as.numeric(GV)

hono_all <- data.frame(data1$time, hono, GV)

export <- cbind(no_all, no2_all[-1],hono_all[-1])
colnames(export) <- c('time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')

write.csv(export, file = 'C:/Users/Zachary/Desktop/Combined.csv', row.names = FALSE)
