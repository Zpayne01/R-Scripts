tab <- read.table(file = 'clipboard', sep = '\t', header = TRUE)

head(tab)

install.packages('agricolae')
library(agricolae)

aov1 <- aov(tab$Gravemetric.Water.Content ~ tab$Sample)

aov1

l1 <- HSD.test(aov1, 'tab$Sample', group = TRUE)

aov2 <- aov(tab$Nitrate..ug.g. ~ tab$Sample)

aov2

l2 <- HSD.test(aov2, 'tab$Sample', group = TRUE)

l2
