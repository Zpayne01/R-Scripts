output <- c()
for (l in 1:3) {
  nameout <- c()
  for (n in 1:4) {
    name1 <- paste0(0.10+(0.05*l-0.05),' ', 10^(n-1), 'm HONO hv')
    name2 <- paste0(0.10+(0.05*l-0.05),' ', 10^(n-1), 'm HONO dep')
    name3 <- paste0(0.10+(0.05*l-0.05),' ', 10^(n-1), 'm HONO OH')
    name4 <- paste0(0.10+(0.05*l-0.05),' ', 10^(n-1), 'm OH NO')
    name5 <- paste0(0.10+(0.05*l-0.05),' ', 10^(n-1), 'm Emis')
    name6 <- paste0(.10+(0.05*l-0.05),' ', 10^(n-1), 'm NO2 dep')
    name <- c(name1, name2, name3, name4, name5, name6)
    nameout <- append(nameout, name)
  }
  output <- append(output, nameout)
}

write.table(t(output), file = 'clipboard', col.names = F, sep = '\t')
