## Work for 07_08_2019. Small example of using the ddply function. Mostly just seperating stuff for reporting OH and HO2 

file <- file.choose()
tab <- read.csv(file = file, header = TRUE, stringsAsFactors = FALSE)

tab <- read.table(file = 'clipboard', sep = '\t', header = TRUE, stringsAsFactors = FALSE)

head(tab)
colnames(tab) <- c('datetime', 'o3')

library(plyr)

tab$datetime <- as.POSIXlt(tab$datetime, format = '%m/%d/%Y %H:%M')

tab$day <- tab$datetime$yday
head(tab)

tab$o3 <- as.numeric(tab$o3)

maxday <- max(aggregate(o3 ~ day, tab, max)[,2])
minday <- min(aggregate(o3 ~ day, tab, max)[1:15,2])
as.numeric(aggregate(o3 ~ day, tab, max))

tab <- read.table(file = "clipboard", header = TRUE)

out <- ddply(tab, .(Sample), numcolwise(mean))
outsd <- ddply(tab, .(Sample), numcolwise(sd))

export <- cbind(out, outsd[,2:3])

write.table(export, file = 'clipboard', sep = '\t', row.names = FALSE)
