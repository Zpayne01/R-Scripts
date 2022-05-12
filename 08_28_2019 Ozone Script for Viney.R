file <- file.choose() ## Select File  
tab <- read.table(file, header = TRUE) ## Translates the file into a data table 

tabappend <- c() ## Blank tabappend

for (i in 1:nrow(tab)-1) {
  tabappend <- append(tabappend, tab$Ozone.ppb.[i+1]-tab$Ozone.ppb.[i])
} ## Append the rate of change from point to point to each data point
  tabappend <- append(tabappend, 0)

length(tabappend)
nrow(tab)

  tab$change <- tabappend

roc <- which(tab$change >= 3) ## Look for the rate of change (if the difference between one point and the next is > 3)

vector <- c()

for (i in 1:length(roc)) {
  vector <- append(vector, seq(roc[i], roc[i]+600, 1)) ##Delete the next 600 points (change the number based on the length of outdoor measurement)
  vector <- unique(vector)
}

output <- tab[-vector,1:2] ##Output file

write.table(output, file = 'C:/Users/zacpayne/Desktop/Viney.txt', row.names = FALSE) ##Change the export file based on the computer
