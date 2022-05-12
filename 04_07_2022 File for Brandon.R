

rframe <- data.frame(value = c(seq(1,100,1)), status = c('Online', sample(c('Online', 'Offline'), replace = T, size = 98), 'Offline'))
rframe$status <- sapply(rframe$status, function(x) switch(x, 'Online' = 1, 'Offline' = 0))

data.frame(rframe$status, lag(rframe$status, n = 1, default = 0))

library(dplyr)
rframe <- rframe %>% 
  mutate(statusonlinestart = ifelse(status - lag(status, n = 1, default = 0) == 1, 1, 0)) %>%
  mutate(statusonlineend = ifelse(status - lead(status, n = 1, default = 0) == 1, 1, 0)) %>%
  mutate(statusofflinestart = ifelse(status - lag(status, n = 1, default = 1) == -1, 1, 0)) %>%
  mutate(statusofflineend = ifelse(status - lead(status, n = 1, default = 1) == -1, 1, 0)) 

rframe

aon <- which(rframe$statusonlinestart == 1) #Beginning of online by index
bon <- which(rframe$statusonlineend == 1) #End of online by index
aoff <- which(rframe$statusofflinestart == 1) #Beginning of offline by index
boff <- which(rframe$statusofflineend == 1) #End of offline by index

#Delete extra points
#Create index for deleting 
#Deletepoints = variable; 
#aon + Deletepoints variable

#This will add status positions for the online
for(i in 1:length(aon)) {
  for(j in aon[i]:bon[i]) {
    rframe$status[j] <- i
  }
}

#This will add status positions for the online
for(i in 1:length(aoff)) {
  for(j in aoff[i]:boff[i]) {
    rframe$status[j] <- -i
  }
}

#mean
aggregate(value ~ status, rframe, mean)

