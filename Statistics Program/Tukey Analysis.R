
file1 <- file.choose()

csv1 <- read.csv(file1, stringsAsFactor = TRUE)

csv1$Time <- as.POSIXlt(csv1$Time, format = '%Y-%m-%d %H:%M:%S')
csv1$hour <- as.factor(csv1$Time$hour)

aov1 <- aov(csv1$no_ave ~ csv1$hour)
tk1 <- TukeyHSD(aov1, 'csv1$hour')

extracted <- as.data.frame(tk1$'csv1$hour')
head(extracted)

extracted[extracted$`p adj` <= 0.05,]

plot(csv1$hono_ave ~ csv1$hour)
plot(csv1$no2_ave ~ csv1$hour)

HSD.test(aov1, 'csv1$hour', group = TRUE)
install.packages('foreign')
library(foreign)

HSD.test(aov1, 'csv$hour', group = TRUE)
install.packages('agricolae')
library(agricolae)
l <- HSD.test(aov1, 'csv1$hour', group = TRUE)
