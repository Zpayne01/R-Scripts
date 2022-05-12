file <- file.choose()
table <- read.table(file, stringsAsFactors = FALSE, header = TRUE)

colnames(table) <- c('date', 'hour', 'chamber', 'h2o')

table$h2o <- as.numeric(table$h2o)
table$h2o <- table$h2o/10^3/22.4*1000*18 
table$h2o[table$h2o < 0] <- 0

uniquedates <- unique(table$date)
uniquechamber <- unique(table$chamber)

save <- data.frame(seq(1,48,1))
i = 1
j = 1

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

for (i in 1:length(uniquedates)) {
start <- subset(table, table$date == uniquedates[i])
 for (j in 1:length(uniquechamber)){
   start2 <- subset(start, start$chamber == uniquechamber[j])
 save <- cbind.fill(save,start2$h2o)}
colnames(save) <- c('hour', 'b', 'blank', 'c', 'd','e')

filename <- paste0('C:/Users/Zachary/Desktop/', gsub("/", "_", start$date[1]), '_H2O.csv')
write.csv(save, filename, row.names = FALSE)

save <- data.frame(seq(1,48,1))
}

