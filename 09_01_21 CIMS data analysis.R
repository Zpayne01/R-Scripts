## Adiren data 2 ##

## Select File ##

file <- file.choose()

## Read File ##

csv <- read.csv(file, sep = ',')

## Check CSV ##

head(csv)

## Set the datetime column to datetime ##

csv$datetime <- paste(csv$date, csv$time)
csv$datetime <- as.POSIXct(csv$datetime, format = '%Y-%m-%d %H:%M:%S')

## Insert the start times ##

start <- seq.POSIXt(as.POSIXct('09-03-2021 15:20', format = '%m-%d-%Y %H:%M'), as.POSIXct('09-04-2021 10:00', format = '%m-%d-%Y %H:%M'), '5 mins') 


## Insert the end times ##

#end <- c('4:25','16:40', '5:40')

## Insert date for POSIXct calculation ##

##date <- '09-02-2021'

## Combine date and time and convert to POSIXct ##

##start <- as.POSIXct(paste(date, start), format = '%m-%d-%Y %H:%M')
end <- start + 300

## Find the closet time in the csv using sapply and which.min ##

startpos <- sapply(start, function(i) which.min(abs(as.integer(i) - as.integer(csv$datetime))))
endpos <- sapply(end, function(i) which.min(abs(as.integer(i) - as.integer(csv$datetime))))

## Extract lists of the start and end positions ##

for (i in 1:length(startpos)) {
  if (i == 1) {
    outputlist <- list()
    average = c()
    stdev <- c()
    outputdf <- data.frame()
  }
  
  output <- csv[startpos[i]:endpos[i],]
  output$group <- i
  # Create output list #
  
  outputlist[[i]] <- output
  outputdf <- rbind(outputdf, output)
  
  # Create output mean #
  
  m <- mean(output$HONO_PPT)
  average <- append(average, m)
  
  # Create output sd #
  
  s <- sd(output$HONO_PPT)
  stdev <- append(stdev, s)
}

## Density plot if I want to look ##

p2 <- ggplot(data=outputdf, aes(x=HONO_PPT, group = group, fill = group)) + 
  geom_density()
p2   

## Average and SD DF

outputdf2 <- data.frame(average = average,
                        sd = stdev)


## Plot ##

plot(csv$datetime, csv$HONO_PPT, type = 'l')

## Subtract Background ##

outputdf2$average <- outputdf2$average - outputdf2$average[1]
