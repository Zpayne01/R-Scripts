## NOx werx noise analysis on fast sampling (NO + BLC1 + BLC2) cycle

file <- file.choose()

noxfile <- read.table(file, sep = ",", header = T)
noxfile <- data.frame(noxfile$TheTime, noxfile$CH1_Hz)
colnames(noxfile) <- c('TheTime', 'CH1_Hz')

noxfile$TheTime <- as.POSIXct(noxfile$TheTime*(60*60*24), origin = "1899-12-30", tz = 'UTC')

Times <- seq.POSIXt(from = as.POSIXct('2021-07-13 11:50:55', tz = 'UTC'), to = as.POSIXct('2021-07-13 13:05:54', tz = 'UTC'), by = 'sec')

m <- match(as.integer(Times), as.integer(noxfile$TheTime))

noxfileappend <- data.frame(datetime = Times,
                            CH1_Hz = noxfile[m,2])

noxfileappend$CH1_Hz <- na.approx(noxfileappend$CH1_Hz)

head(noxfileappend)

ave <- colMeans(matrix(noxfileappend$CH1_Hz, 30))
sd <- colSds(matrix(noxfileappend$CH1_Hz, 30))

df <- data.frame(Time = seq(0, length(ave)/3-1, 1)*90,
           average_no = ave[seq(1,length(ave), 3)],
           sd_no = sd[seq(1, length(ave), 3)],
           average_BLC2 = ave[seq(2,length(ave), 3)],
           average_BLC1 = ave[seq(3,length(ave), 3)])

df$difference_BLC2 = df$average_BLC2 - df$average_no
df$difference_BLC1 = df$average_BLC1 - df$average_no
df$percent_dif_BLC2 = df$difference_BLC2/df$average_no
df$percent_dif_BLC1 = df$difference_BLC1/df$average_no
df$SNR = df$average_no/df$sd_no

## Mean no_ave ##

ggplot(data = df, aes(x = Time)) + 
  geom_line(aes(y = average_no))

## Deviation over time ##

ggplot(data = df, aes(x = Time)) + 
  geom_line(aes(y = sd_no))

## Signal to noise over time ##

ggplot(data = df, aes(x = Time)) + 
  geom_point(aes(y = SNR))

## Raw count difference of artifact ## 

ggplot(data = df, aes(x = Time)) + 
  geom_point(aes(y = difference_BLC1, color = 'black')) + 
  geom_point(aes(y = difference_BLC2, color = 'red')) + 
  geom_smooth(aes(y = difference_BLC2), method = 'lm', formula = y ~ poly(x, 2), color = 'red') + 
  geom_smooth(aes(y = difference_BLC1), method = 'lm', formula = y ~ poly(x, 2), color = 'black') + 
  scale_color_identity(name = 'LED',
                       breaks = c('black', 'red'),
                       labels = c('385 nm', '395 nm'),            
                       guide = 'legend') + 
  ylab('Difference from NO measurement baseline') + 
  xlab('Time (s)')

## Percentage difference of artifact ## 

ggplot(data = df, aes(x = Time)) + 
  geom_point(aes(y = percent_dif_BLC2, color = 'black')) + 
  geom_point(aes(y = percent_dif_BLC1, color = 'red')) + 
  geom_smooth(aes(y = percent_dif_BLC2), method = 'lm', color = 'black') + 
  geom_smooth(aes(y = percent_dif_BLC1), method = 'lm', color = 'red') + 
  scale_color_identity(name = 'LED',
                       breaks = c('black', 'red'),
                       labels = c('385 nm', '395 nm'),            
                       guide = 'legend') + 
  ylab('Normalized percentage difference') + 
  xlab('Time (s)')

       