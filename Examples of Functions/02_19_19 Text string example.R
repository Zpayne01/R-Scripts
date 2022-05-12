file <- file.choose()
csv <- read.csv(file, headers = TRUE, stringsAsFactors = FALSE)

filename <- 'C:/Users/zacpayne/Desktop/What.txt'

for (i in 1:nrow(csv)) {
  x <- csv[i,1]
  pasteFunc(x)
}

pasteFunc <- function (x) {
  y1 <- paste('i=i+1;')
  y2 <- paste0("Rnames{i} = '",  x," + RO2 = ROOR';")
  y3 <- paste0("k(:i) = 5e-14;")
  y4 <- paste0("Gstr{i,1} = '", x,"'; Gstr{i,2} = 'RO2';")
  y5 <- paste0("f",x,"(i) = f", x, "(i)-1; fROOR(i) = fROOR(i)+1;")
  cat(c(y1,y2,y3,y4,y5,""), sep = "\n", file = filename, append = TRUE)
}


