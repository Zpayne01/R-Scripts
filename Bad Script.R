## Open File ##

file <- file.choose()
data <- read.csv(file, header = FALSE)

## Create Table ##
colnames(data) <- c("time1", "measurement1", "time2", "measurement2")

## Seperate Data ##

data1 <- data.frame(data$time1, data$measurement1)
data2 <- data.frame(data$time2, data$measurement2)

## Remove invalid points from each set ##

data1 <- subset(data1, data1$data.measurement1 != "NA")
data2 <- subset(data2, data2$data.measurement2 != "NA")

## Append Times ##
datetime1 <- strptime(data1$data.time1, "%m/%d/%Y %H:%M", tz = "est")
datetime2 <- strptime(data2$data.time2, "%m/%d/%Y %H:%M", tz = "est")
data1$min1 <- datetime1$min
data2$min2 <- datetime2$min
data1$hr1 <- datetime1$hour
data2$hr2 <- datetime2$hour
data1$day1 <- datetime1$mday
data2$day2 <- datetime2$mday

## Correlate Times and Create Correlated Data ##

corrdata <- c()

for(i in 1:nrow(data1))
	{day <- data1$day1[i]
	 hour <- data1$hr1[i]
	 min <- data1$min1[i]
	 workdata <- data2
	 correlation <- FALSE

	 	while(correlation == FALSE){
		j <- match(day, workdata$day2, nomatch = nrow(data2)+1)
				if(j > nrow(data2))
					{	correlation <- TRUE	}
				else
					{
					if(hour == workdata$hr2[j] & min == workdata$min2[j])
						{	newrow <- c(as.character(data1$data.time1[i]),data1$data.measurement1[i],as.character(data2$data.time2[j]),data2$data.measurement2[j])
							corrdata <- rbind(corrdata, newrow)
							correlation <- TRUE
						}
					else 
						{	workdata$day2[j] <- NA	}
					}
		}
	}

write.table(corrdata, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)