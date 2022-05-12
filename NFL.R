file <- file.choose()
csv <- read.csv(file, stringsAsFactors = FALSE)

for (i in 1:nrow(csv))
{assign(csv[i,1], csv[i,2])}

head(csv)

homechart <- data.frame(csv[,1:2])

for (i in 3:ncol(csv)) {
  for(j in 1:nrow(csv)) {
    homechart[j,i] <- grepl('@', csv[j,i])
    csv[j,i] <- gsub('@','',csv[j,i])
  }}
    
 rankchart <- data.frame(csv[,1:2])
 BYE = 0
 
for (i in 3:ncol(csv)) {
  for (j in 1:nrow(csv)) {
    if(get(csv[j, i]) == 0) {
      rankchart[j, i] = 'NA'
      } else {
    if (homechart[j, i] == 'TRUE') {
    rankchart[j, i] <- get(csv[j, i]) - j
    } else {
    rankchart[j, i] <- (get(csv[j, i]) - j) * .80
    }
    }
  }
}
