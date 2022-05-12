
setwd('C:/Users/zacpayne/Desktop/CU HOxROx/CU HOxROx')

files <- list.files(path = getwd())

table <- read.csv(files[1], header = TRUE, stringsAsFactors = FALSE)
names <- colnames(table)

for (i in 2:length(files))
{
  addition <- read.csv(files[i], header = TRUE, stringsAsFactors = FALSE)
  colnames(addition) <- names
  table <- rbind(table, addition)
}

filename <- 'C:/Users/zacpayne/Desktop/Combined.csv'
write.csv(table, file = filename, row.names = FALSE)

