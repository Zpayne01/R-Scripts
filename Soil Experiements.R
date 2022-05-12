file <- file.choose()
tab <- read.csv(file, header = T)

head(tab)

#On 2/28/2022 

tab[4,] <- tab[1,] #There was an error with this background so input the most recent
tab[4,1] <- '2022-02-26 12:30:00'

# Convert time

tab$Time <- as.POSIXlt(tab$Time, format = '%Y-%m-%d %H:%M:%S')

tab2 <- tab[tab$Time$min %% 15 != 5, ]
tab2$Time <- as.POSIXct(tab2$Time)

FS <- tab2[seq(2, nrow(tab2), 2), ]
BLANK <- tab2[seq(1, nrow(tab2), 2), ]

FS[,2:7] <- (FS[,2:7] - 25)/(1827)
BLANK[,2:7] <- (BLANK[,2:7] - 25)/(1827)

FS[,4:5] <- FS[4:5]/0.9
BLANK[,4:5] <- BLANK[4:5]/0.9

FS[,6] <- FS[,6] - FS[,2]
BLANK[,6] <- BLANK[,6] - BLANK[,2]

dev.new(width = 15,
        height = 3)

par(mfrow = c(3,1), mar = c(3.5,3.5,2,1), mgp = c(2,1,0))

plot(no_counts ~ Time, FS, type = 'l', ylim = c(0, 2.5), ylab = c('NO (ppb)'), xlab = c('Time (EDT)'))
lines(no_counts ~ Time, BLANK, col = 'red')
legend("topright", legend = c('Forest', 'Blank'), col = c('black', 'red'), lty = 1, cex = 1, pt.cex = 10)

plot(blc2_counts ~ Time, FS, type = 'l', ylim = c(0, 0.3), ylab = c('NO2 (ppb)'), xlab = c('Time (EDT)'))
lines(blc2_counts ~ Time, BLANK, col = 'red')

plot(HONO_counts ~ Time, FS, type = 'l', ylim = c(0, 0.5), ylab = c('HONO (ppb)'), xlab = c('Time (EDT)'))
lines(HONO_counts ~ Time, BLANK, col = 'red')

dev.off()
plot(FS$Time, FS$HONO_counts - BLANK$HONO_counts[1:length(FS$HONO_counts)], type = 'l')
