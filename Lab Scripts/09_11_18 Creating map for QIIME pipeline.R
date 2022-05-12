wd <- choose.dir()
filenames <- list.files(wd)

filestring <- '$PWD/iurtp-paired-end-sequences/'

L001 <- grepl('L001', filenames)
filenames <- filenames[L001 == TRUE]


forward <- grepl('R1', filenames)
reverse <- grepl('R2', filenames)
direction <- c()

for (i in 1:length(filenames)) {
  if (forward[i] == TRUE) {
    adddirection = 'forward'
  } else { 
    adddirection = 'reverse'
  }
  direction = c(direction, adddirection)
}

splitnames <- strsplit(filenames, "_")
sample <- c()

for (i in 1:length(filenames)) {
  addsample <- paste(splitnames[[i]][1], splitnames[[i]][2], sep = "_")
  sample <- c(sample, addsample)
}

csv <- data.frame(sample, paste0(filestring, filenames), direction)
colnames(csv) <- c('sample-id', 'absolute-filepath', 'direction')

write.csv(csv, file = 'C:/Users/Zachary/Desktop/manifest.csv', row.names = FALSE)
