## Correlation ##

## Open File ##

file <- file.choose()
data <- read.csv(file)

## Create Table ##
colnames(data) <- c("time1", "measurement1", "time2", "measurement2")

## Append Times ##
datetime1 <- strptime(data$time1, "%m/%d/%Y %H:%M", tz = "est")
datetime2 <- strptime(data$time2, "%m/%d/%Y %H:%M", tz = "est")
data$min1 <- datetime1$min
data$min2 <- datetime2$min
data$hr1 <- datetime1$hour
data$hr2 <- datetime2$hour
data$day1 <- datetime1$mday
data$day2 <- datetime2$mday

## Correlate Times and Create Correlated Data ##

corrdata <- c()

for(i in 1:nrow(data))
	{day <- data$day1[i]
	 hour <- data$hr1[i]
	 workdata <- data
	 correlation <- FALSE

	 	while(correlation == FALSE){
		j <- match(day, workdata$day2, nomatch = nrow(data)+1)
				if(j > nrow(data))
					{	correlation = TRUE	}
				else
					{
					if(hour == workdata$hr2[j])
						{	newrow <- list(as.character(data$time1[i]),data$measurement1[i],as.character(data$time2[j]),data$measurement2[j])
							corrdata <- rbind(corrdata, newrow)
							correlation <- TRUE
						}
					else 
						{	workdata$day2[j] <- NA	}
					}
		}
	}

write.table(corrdata, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)